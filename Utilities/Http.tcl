package provide Http 2.1

proc Trace {{skip 1}} {
    set result {}
    for {set level [expr [info level] - $skip]} {$level >= 0} {incr level -1} {
	lappend result [info level $level]
    }
    return $result
}

proc narrate {args} {
    #return
    set sf [info level -1]
    puts stderr "N: $args: $sf"
}

# translation -- 
#
#	fconfigure the connected socket into a given mode
#
# Arguments:
#	args	additional args to fconfigure
#
# Side Effects:
#	sets the connected socket to the given mode

proc translation {sock args} {
    set additional {}
    for {set i 0} {$i < [llength $args]} {incr i} {
	set a [lindex $args $i]
	switch -glob -- $a {
	    -r* {
		incr i
		set rmode [lindex $args $i]
	    }
	    -w* {
		incr i
		set wmode [lindex $args $i]
	    }
	    default {
		lappend additional $a
	    }
	}
    }

    lassign [fconfigure $sock -translation] crm cwm

    if {[info exists rmode] && ($crm ne $rmode)} {
	Debug.socket {$sock read mode to $rmode} 20
    } else {
	set rmode $crm
    }

    if {[info exists wmode] && ($cwm ne $wmode)} {
	Debug.socket {$sock write mode to $wmode} 20
    } else {
	set wmode $cwm
    }

    fconfigure $sock -translation [list $rmode $wmode] {*}$additional
    Debug.socket {MODE: $rmode $wmode} 20
}

#interp bgerror "" ::Http::bgerror

namespace eval Http {
    proc narrate {args} {
	return
	set sf [info level -1]
	upvar -1 self self
	$self log debug "N: $args: $sf"
    }

    # throw --
    #
    #
    # Arguments:
    #
    #	dict	request dict
    #	msg	error message
    #	code	http error code
    #
    # Results: None
    #
    # Side Effects:
    #
    #	transforms a protocol error into a tcl error with attached
    #	-request dictionary, used to feed errors back to client

    proc throw {dict msg {code 500}} {
	return -code $code -request $dict $msg
    }

    # HTTP error codes and default textual interpretation
    variable Errors
    array set Errors {
	1 "Informational - Request received, continuing process"
	100 Continue
	101 "Switching Protocols"

	2 "Success - received, understood, and accepted"
	200 OK
	201 Created
	202 Accepted
	203 "Non-Authoritative Information"
	204 "No Content"
	205 "Reset Content"
	206 "Partial Content"

	3 "Redirection - Further action needed"
	300 "Multiple Choices"
	301 "Moved Permanently"
	302 "Found"
	303 "See Other"
	304 "Not Modified"
	305 "Use Proxy"
	307 "Temporary Redirect"

	4 "Client Error - request bad or cannot be fulfilled"
	400 "Bad Request"
	401 "Unauthorized"
	402 "Payment Required"
	403 "Forbidden"
	404 "Not Found"
	405 "Method Not Allowed"
	406 "Not Acceptable"
	407 "Proxy Authentication Required"
	408 "Request Time-out"
	409 "Conflict"
	410 "Gone"
	411 "Length Required"
	412 "Precondition Failed"
	413 "Request Entity Too Large"
	414 "Request-URI Too Large"
	415 "Unsupported Media Type"
	416 "Requested range not satisfiable"
	417 "Expectation Failed"

	5 "Server Error - Server failed to fulfill an apparently valid request"
	500 "Internal Server Error"
	501 "Not Implemented"
	502 "Bad Gateway"
	503 "Service Unavailable"
	504 "Gateway Time-out"
	505 "HTTP Version not supported"
    }

    # categorise headers
    variable headers
    foreach n {accept accept-charset accept-encoding accept-language authorization expect from host if-match if-modified-since if-none-match if-range if-unmodified-since max-forwards proxy-authorization referer te user-agent keep-alive cookie} {
	set headers($n) rq
    }
    foreach n {accept-ranges age etag location proxy-authenticate retry-after server vary www-authenticate} {
	set headers($n) rs
    }
    foreach n {allow content-encoding content-language content-length content-location content-md5 content-range content-type expires last-modified} {
	set headers($n) e
    }
    foreach n {cache-control connection date pragma trailer transfer-encoding upgrade via warning} {
	set headers($n) e
    }

    # map http error code to human readable message
    proc ErrorMsg {code} {
	variable Errors
	if {[info exist Errors($code)]} {
	    return $Errors($code)
	} else {
	    return "Error $code"
	}
    }

    # return an http date
    proc DateInSeconds {date} {
	if {[string is integer -strict $date]} {
	    return $date
	} elseif {[catch {clock scan $date \
			-format {%a, %d %b %Y %T GMT} \
			-gmt true} result eo]} {
	    error "DateInSeconds '$date', ($result)"
	} else {
	    return $result
	}
    }

    # return an http date
    proc Date {{seconds ""}} {
	if {$seconds eq ""} {
	    set seconds [clock seconds]
	}

	return [clock format $seconds -format {%a, %d %b %Y %T GMT} -gmt true]
    }

    proc Now {} {
	return [clock format [clock seconds] -format {%a, %d %b %Y %T GMT} -gmt true]
    }

    proc File {rsp path {ctype ""}} {
	set path [file normalize $path]
	dict set rsp -fd [::open $path r]
	dict set rsp -file $path

	if {$ctype eq ""} {
	    dict set rsp content-type [Mime type $path]
	} else {
	    dict set rsp content-type $ctype
	}

	dict set rsp -code 200
	dict set rsp -rtype File
	return $rsp
    }

    proc CacheableFile {rsp path {ctype ""}} {
	set path [file normalize $path]

	# read the file
	set fd [::open $path r]
	chan configure $fd -translation binary
	dict set rsp -content [read $fd]
	close $fd

	dict set rsp -file $path

	set mtime [file mtime $path]
	dict set rsp -modified $mtime
	dict set rsp last-modified [Date $mtime]

	if {$ctype eq ""} {
	    # calculate content-type using mime guessing
	    dict set rsp content-type [Mime type $path]
	} else {
	    dict set rsp content-type $ctype
	}

	# allow server caching
	set rsp [Http Depends $rsp [file normalize $path]]

	dict set rsp -code 200
	dict set rsp -rtype CacheableFile

	#catch {dict unset rsp -content}
	return $rsp
    }

    proc Depends {rsp args} {
	catch {dict unset rsp -dynamic}
	dict lappend rsp -depends {*}$args
	return $rsp
    }

    proc NoCache {rsp} {
	dict set rsp cache-control "no-store, no-cache, must-revalidate, max-age=0"; # HTTP/1.1
	dict set rsp expires "Sun, 01 Jul 2005 00:00:00 GMT"
	dict set rsp pragma "no-cache"	;# HTTP/1.0
	dict set rsp -dynamic 1
	#catch {dict unset rsp last-modified}
	return $rsp
    }

    proc Cache {rsp {age 0} {realm "public"}} {
	if {[string is integer -strict $age]} {
	    # it's an age
	    if {($age != 0)} {
		dict set rsp expires [Date expr {[clock seconds] + $age}]
	    } else {
		catch {dict unset rsp expires}
	    }
	} else {
	    dict set rsp expires [Date [clock scan $age]]
	    set age [expr {[clock scan $age] - [clock seconds]}]
	}
	dict set rsp cache-control "$realm, max-age=$age"
	return $rsp
    }

    proc DCache {rsp {age 0} {realm "public"}} {
	set rsp [Cache $rsp $age $realm]
	dict append rsp cache-control ", must-revalidate"
	return $rsp
    }

    proc CacheableContent  {rsp mtime {content ""} {ctype ""}} {
	# cacheable content must have last-modified
	if {![dict exists $rsp last-modified]} {
	    dict set rsp last-modified [Date $mtime]
	}
	dict set rsp -modified $mtime

	if {$content ne ""} {
	    dict set rsp -content $content
	}

	if {$ctype eq ""} {
	    if {![dict exists $rsp content-type]} {
		dict set rsp content-type "text/html"
	    }
	} else {
	    dict set rsp content-type $ctype
	}

	dict set rsp -code 200
	dict set rsp -rtype CacheableContent
	return $rsp
    }

    proc OkResponse {rsp code rtype content ctype} {
	if {$content ne ""} {
	    dict set rsp -content $content
	}

	if {$ctype eq ""} {
	    if {![dict exists $rsp content-type]} {
		dict set rsp content-type "text/html"
	    }
	} else {
	    dict set rsp content-type $ctype
	}

	dict set rsp -code $code
	dict set rsp -rtype Ok
	return $rsp
    }

    proc Ok {rsp {content ""} {ctype ""}} {
	return [OkResponse $rsp 200 Ok $content $ctype]
    }

    proc Created {rsp location {content ""} {ctype ""}} {
	dict set rsp -location $location
	return [OkResponse $rsp 201 Created $content $ctype]
    }

    proc Accepted {rsp {content ""} {ctype ""}} {
	return [OkResponse $rsp 202 Accepted $content $ctype]
    }

    proc NonAuthoritative {rsp {content ""} {ctype ""}} {
	return [OkResponse $rsp 203 NonAuthoritative $content $ctype]
    }

    proc NoContent {rsp} {
	foreach el {content-type -content -fd} {
	    catch [list dict unset rsp $el]
	}

	dict set rsp -code 204
	dict set rsp -rtype NoContent

	return $rsp
    }

    proc ResetContent {rsp {content ""} {ctype ""}} {
	return [OkResponse $rsp 205 ResetContent $content $ctype]
    }

    proc PartialContent {rsp {content ""} {ctype ""}} {
	return [OkResponse $rsp 206 PartialContent $content $ctype]
    }

    proc ServerError {rsp message {eo ""}} {
	puts stderr "Server Error: $message ($eo)"
	set content ""
	if {$eo ne ""} {
	    append content "<table border='1' width='80%'>" \n
	    append content <tr> <th> Error Info </th> </tr> \n
	    dict for {n v} $eo {
		append content <tr> <td> $n </td> <td> [armour $v] </td> </tr> \n
	    }
	    append content </table> \n
	}

	#if {($eo ne "") && [dict exists $eo -errorinfo]} {
	#    catch {
	#	set message [string map {\n <br>} [armour [dict get $eo -errorinfo]]]
	#    }
	#}

	catch {append content "<p>Caller: [armour [info level -1]]</p>"}
	set message [armour $message]
	catch {dict unset rsp expires}
	dict set rsp content-type "text/html"
	dict set rsp -content "<html><head><title>Server Error: $message</title></head>
	<body>
	<h1>Server Error</h1>
	<p>$message</p>
	<hr>
	$content
	<hr>
	[dump $rsp]
	</body>
	</html>
	"

	dict set rsp -code 500
	dict set rsp -rtype Error
	dict set rsp -dynamic 1

	# Errors are completely dynamic - no caching!
	set rsp [NoCache $rsp]
	Debug.http {ServerError [dumpMsg $rsp 0]}

	return $rsp
    }

    proc NotImplemented {rsp {message ""}} {
	if {$message eq ""} {
	    set message "This function not implemented"
	}
	dict set rsp content-type "text/html"
	dict set rsp -content "<html>\n<title>Not Implemented</title>\n<body>\n<h1>Not Implemented</h1>\n<p>$message</p>\n</body>\n</html>"

	dict set rsp -code 501
	dict set rsp -rtype NotImplemented
	return $rsp
    }

    proc Unavailable {rsp message {delay 0}} {
	dict set rsp content-type "text/html"
	dict set rsp -content "<html>\n<title>Service Unavailable</title>\n<body>\n<h1>Service Unavailable</h1>\n<p>$message</p>\n</body>\n</html>"

	dict set rsp -code 503
	dict set rsp -rtype Unavailable
	if {$delay > 0} {
	    dict set rsp retry-after $delay
	}
	return $rsp
    }

    proc Bad {rsp message {code 400}} {
	dict set rsp content-type "text/html"
	dict set rsp -content "<html>\n<title>Bad Request</title>\n<body>\n<h1>Bad Request</h1>\n<p>$message</p>\n</body>\n</html>"

	dict set rsp -code $code
	dict set rsp -rtype Bad
	dict set rsp -error $message
	return $rsp
    }

    proc NotFound {rsp {content ""} {ctype "text/x-system"}} {
	if {$content ne ""} {
	    dict set rsp content-type $ctype
	    dict set rsp -content $content
	}

	if {![dict exists $rsp -content]} {
	    set uri [dict get $rsp -uri]
	    dict set rsp -content "title: $uri - Not Found
		<h1>$uri Not Found</h1>
		<p>The entity '$uri' doesn't exist.</p>
	    "
	    dict set rsp content-type text/x-system
	}

	dict set rsp -code 404
	dict set rsp -rtype NotFound
	return $rsp
    }

    proc Forbidden {rsp {content ""} {ctype "text/x-html-fragment"}} {
	if {$content ne ""} {
	    dict set rsp content-type $ctype
	    dict set rsp -content $content
	} else {
	    dict set rsp content-type "text/x-system"
	    dict set rsp -content "title:Access Forbidden
	<h1>Access Forbidden</h1>
	<p>You are not permitted to access this page.</p>
	<hr>
	"
	}

	dict set rsp -code 403
	dict set rsp -rtype Forbidden
	return $rsp
    }

    proc Unauthorized {rsp challenge {content ""} {ctype "text/x-html-fragment"}} {
	dict lappend rsp -auth $challenge
	if {$content ne ""} {
	    dict set rsp content-type $ctype
	    dict set rsp -content $content
	}

	dict set rsp -code 401
	dict set rsp -rtype Unauthorized
	return $rsp
    }

    proc Conflict {rsp {content ""} {ctype "text/x-system"}} {
	if {$content ne ""} {
	    dict set rsp content-type $ctype
	    dict set rsp -content $content
	}

	dict set rsp -code 409
	dict set rsp -rtype Conflict
	return $rsp
    }

    proc PreconditionFailed {rsp {content ""} {ctype "text/x-system"}} {
	if {$content ne ""} {
	    dict set rsp content-type $ctype
	    dict set rsp -content $content
	}

	dict set rsp -code 412
	dict set rsp -rtype PreconditionFailed
	return $rsp
    }

    proc NotModified {rsp} {
	catch {dict unset rsp content-type}
	catch {dict unset rsp -content}

	dict set rsp -code 304
	dict set rsp -rtype NotModified
	return $rsp
    }

    # internal redirection generator
    proc genRedirect {title code rsp to content ctype} {
	if {$content eq ""} {
	    dict set rsp content-type "text/html"
	    dict set rsp -content "<html>\n<head>\n<title>$title</title>\n</head>\n<body>\n<h1>$title</h1>\n<p>The page may be found here: <a href='[armour $to]'>[armour $to]</a></p>\n</body>\n</html>\n"
	} else {
	    dict set rsp content-type $ctype
	    dict set rsp -content $content
	}

	if {0} {
	    if {![string match {http:*} $to]} {
		# do some munging to get a URL
		dict set rsp location $rsp [Url redir $rsp $to]
	    } else {
		dict set rsp location $to
	    }
	}

	dict set rsp location $to
	dict set rsp -code $code
	dict set rsp -rtype $title

	dict set rsp -dynamic 1	;# don't cache redirections

	return $rsp
    }

    proc Referer {req} {
	if {[dict exists $req referer]} {
	    return [dict get $req referer]
	} else {
	    return ""
	}
    }

    proc Redirect {rsp to {content ""} {ctype "text/html"} args} {
	set query {}
	foreach {name val} $args {
	    lappend query "$name=[Query encode $val]"
	}
	if {$query ne {}} {
	    append to ? [join $query &]
	}

	return [Http genRedirect Redirect 302 $rsp $to $content $ctype]
    }

    proc RedirectReferer {rsp args} {
	set ref [Referer $rsp]
	if {$ref eq ""} {
	    set ref /
	}
	set query {}
	foreach {name val} $args {
	    lappend query "$name=[Query encode $val]"
	}
	if {$query ne {}} {
	    append ref ? [join $query &]
	}
	return [Redirect $rsp $ref]
    }

    proc Found {rsp to {content ""} {ctype "text/html"}} {
	return [Http genRedirect Redirect 302 $rsp $to $content $ctype]
    }

    proc Relocated {rsp to {content ""} {ctype "text/html"}} {
	return [Http genRedirect Relocated 307 $rsp $to $content $ctype]
    }
    
    proc SeeOther {rsp to {content ""} {ctype "text/html"}} {
	return [Http genRedirect SeeOther 303 $rsp $to $content $ctype]
    }

    proc Moved {rsp to {content ""} {ctype "text/html"}} {
	return [Http genRedirect Moved 301 $rsp $to $content $ctype]
    }

    
    # loadContent -- load a response's file content 
    #	used when the content must be transformed
    #
    # Arguments:
    #	rsp	a response dict
    #
    # Side Effects:
    #	loads the content of a response file descriptor
    #	Possibly close socket

    proc loadContent {rsp} {
	# if rsp has -fd content and no -content
	# we must read the entire file to convert it
	if {[dict exists $rsp -fd]} {
	    if {![dict exists $rsp -content]} {
		if {[catch {
		    set fd [dict get $rsp -fd]
		    fconfigure $fd -translation binary
		    read $fd
		} content eo]} {
		    # content couldn't be read - serious error
		    set rsp [Http ServerError $rsp $content $eo]
		} else {   
		    dict set rsp -content $content
		}

		if {![dict exists $rsp -fd_keep_open]} {
		    # user can specify fd is to be kept open
		    catch {close $fd}
		    dict unset rsp -fd
		} else {
		    seek $fd 0	;# re-home the fd
		}
	    }
	} elseif {![dict exists $rsp -content]} {
	    error "expected content"
	}

	return $rsp
    }

    # dump the context
    proc dump {req {short 1}} {
	catch {
	    set c ""
	    append c "<table border='1' width='80%'>" \n
	    append c <tr> <th> Metadata </th> </tr> \n

	    foreach n [lsort [dict keys $req -*]] {
		if {$short && ($n eq "-content")} continue
		append c \n <tr>
		append c <td> $n </td>
		append c <td> [armour [dict get $req $n]] </td>
		append c </tr> \n
	    }
	    append c </table> \n
	    
	    append c "<table border='1' width='80%'>" \n
	    append c <tr> <th> HTTP </th> </tr> \n
	    foreach n [lsort [dict keys $req {[a-zA-Z]*}]] {
		append c <tr> <td> $n </td> <td> [armour [dict get $req $n]] </td> </tr> \n
	    }
	    append c </table> \n
	    
	    append c "<table border='1' width='80%'>" \n
	    append c <tr> <th> Query </th> </tr> \n
	    array set q [Query flatten [Query parse $req]]
	    foreach {n} [lsort [array names q]] {
		append c <tr> <td> $n </td> <td> $q($n) </td> </tr> \n
	    }
	    append c </table> \n
	
	    append c "<table border='1' width='80%'>" \n
	    append c <tr> <th> Session </th> </tr> \n
	    foreach key [lsort [Session rdict $req keys]] {
		append c <tr> <td> $key </td> <td> [Session rdict $req get $key] </td> </tr> \n
	    }
	    append c </table> \n
	} r eo
	#puts stderr "DUMP: $r ($eo)"
	return $c
    }

    # add a Vary field
    proc Vary {rsp args} {
	foreach field $args {
	    dict set rsp -vary $field 1
	}
	return $rsp
    }

    # add a Vary field
    proc UnVary {rsp args} {
	foreach field $args {
	    catch {dict unset rsp -vary $field}
	}
	return $rsp
    }

    proc Refresh {rsp time {url ""}} {
	catch {dict unset rsp cache-control}
	if {$url == ""} {
	    dict set rsp refresh $time
	} else {
	    dict set rsp refresh "${time};url=$url"
	}
	return $rsp
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

if {0} {
    proc trace_pmod {args} {
	puts stderr "MODIFIED!!!!  $args"
	puts stderr "MODIFIED2!!! [info level -1]"
    }

    trace add command Http {rename delete} trace_pmod
}
