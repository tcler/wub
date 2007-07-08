# HttpdWorker - Httpd Protocol worker thread

# TODO: armour all [chan event]s
package require Debug
package require spiders
#package require Access
#Access open

#puts stderr "Starting Httpd Worker [::thread::id]"
proc bgerror {error eo} {
    Debug.error {Thread [::thread::id]: $error ($eo)}
    catch {
	dict lappend ::request -debug bgerror [list [clock seconds] $error $eo]
    }
    catch {
	Disconnect [dict get $::request -sock] $error $eo
    }
}
interp bgerror {} ::bgerror

#puts stderr "Thread: [::thread::id]";
interp alias {} armour {} string map {& &amp; < &lt; > &gt; \" &quot; ' &#39;}

# arrange gzip Transfer Encoding
if {![catch {package require zlib}]} {
    variable te_encodings {gzip}
} else {
    variable te_encodings {}
}
#set te_encodings {}	;# uncomment to stop gzip transfers

package require WubUtils
package require Debug
Debug off url
Debug off http 10
Debug off cookies
Debug off socket 10

package require Url
package require Http
package require Cookies

if {0} {
    # some utility/debugging code to track shimmering
    package require tweezer
    proc monitor {var} {
	upvar 1 $var v
	trace add variable v {write read} ::shimmer
    }
    proc unmonitor {var} {
	upvar 1 $var v
	catch {trace remove variable v {write read} ::shimmer}
    }

    proc shimmer {var name2 op} {
	upvar $var v
	if {[info exists v]} {
	    puts stderr "$var shimmers to: [tweezer type $v]"
	}
    }
}

variable request ""	;# dict containing request read
#monitor request	;# monitor request shimmering

variable transaction -1	;# transaction count for current connection
variable generation 0	;# worker/connection association generation

# limits on header size
variable maxline 2048	;# max request line length
variable maxfield 4096	;# max request field length
variable maxhead 1024	;# maximum number of header lines
variable maxurilen 1024	;# maximum URI length
variable maxentity -1		;# maximum entity size

package require Timer
Timer txtimer; variable txtime 20000	;# inter-write timeout
Timer rxtimer; variable rxtime 10000	;# inter-read timeout
variable enttime 10000	;# entity inter-read timeout

# transmission state
variable satisfied; array set satisfied {}	;# array of requests satisfied
variable replies; array set replies {}	;# array of replies pending
variable pending 0			;# currently unsatisfied requests
variable response	-1	;# last response sent

proc timeout {timer sock args} {
    dict set ::request -debug timeout $timer $args
    if {![array size ::replies]} {
	#Debug.error {Timeout $args - pending:$::pending replies:[array size ::replies]} 2
	Disconnect $sock "Idle Time-out"; return
    } else {
	$timer restart
    }
}

# detach - force this thread to detach its socket
proc detach {sock} {
    ::thread::detach $sock
}

# readable - make socket readable
proc readable {sock args} {
    dict set ::request -debug readable $args	;# debugging state
    if {[catch {chan event $sock readable $args} r eo]} {
	Debug.error "readable: '$r' ($eo)"
	Disconnect $sock $r $eo; return
    }
}

# writable - make socket writable
proc writable {sock args} {
    dict set ::request -debug writable $args
    if {[catch {chan event $sock writable $args} r eo]} {
	Debug.error "writable: '$r' ($eo)"
	Disconnect $sock $r $eo; return
    }
}

# responder --
#	deliver in-sequence transaction responses
#
# Arguments:
#
# Side Effects:
#	Send transaction responses to client
#	Possibly close socket

proc responder {sock} {
    variable replies
    Debug.http {RESPONDER $sock: [array names replies]} 4
    if {[eof $sock]} {
	Disconnect $sock "Lost connection on transmit"; return
    }

    if {[catch {
	catch {txtimer cancel}

	# determine next available response in transaction# order
	set next [lindex [lsort -integer [array names replies]] 0]

	# ensure we don't send responses out of sequence
	variable response
	if {$next eq {} || $next > ($response + 1)} {
	    # something's blocking the response pipeline
	    # we don't have a response for the next transaction.
	    
	    # we have to wait until all the preceding transactions
	    # have something to send
	    Debug.http {no pending or '$next' doesn't follow '$response'}
	    
	    writable $sock	;# disable responder
	    txtimer after $::txtime timeout txtimer $sock "responder pending"
	} else {
	    # we're going to respond to the next transaction in trx order
	    # unpack and consume the reply from replies queue
	    lassign $replies($next) head content close

	    # remove this response from the pending response structure
	    set response $next	;# move to next response
	    unset replies($next)	;# consume next reply

	    # connection close required?
	    # we only consider closing if all pending requests
	    # have been satisfied.
	    if {$close} {
		Debug.close {close requested on $sock - sending header}
		append head "Connection: close" \r\n	;# send a close just in case
		# Once the header's been sent, we're committed to closing
	    }

	    # send the header
	    puts -nonewline $sock "$head\r\n"	;# send headers with terminating nl
	    set ::sent $head
	    Debug.socket {SENT: $sock $head'} 4

	    # send the content/file (if any)
	    # note: we must *not* send a trailing newline, as this
	    # screws up the content-length and confuses the client
	    # which doesn't then pick up the next response
	    # in the pipeline
	    puts -nonewline $sock $content	;# send the content
	    chan flush $sock			;# no, really, send it

	    incr ::pending -1		;# count one fewer request pending
	    if {$close} {
		Disconnect $sock "Normal termination"
	    } else {
		writable $sock responder $sock	;# keep trying to send replies
	    }
	}
    } r eo]} {
	Debug.error {FAILED send '$r' ($eo)}
	Disconnect $sock "Disconnect - $r" $eo
    } else {
	Debug.socket {SENT content (close: $close) [string length $content] '$content'} 10
    }
}

# expunge - remove metadata from reply dict
proc expunge {reply} {
    foreach n [dict keys $reply content-*] {
	dict unset reply $n
    }
    if {[dict exists $reply transfer-encoding]} {
	dict unset reply transfer-encoding
    }
    if {[dict exists $reply -content]} {
	dict unset reply -content	;# discard content
    }
    return $reply
}

# gzip_content - gzip-encode the content
proc gzip_content {reply} {
    if {[dict exists $reply -gzip]} {
	return $reply	;# it's already been gzipped
    }

    # prepend a minimal gzip file header:
    # signature, deflate compression, no flags, mtime,
    # xfl=0, os=3
    set content [dict get $reply -content]
    set gzip [binary format "H*iH*" "1f8b0800" [clock seconds] "0200"]
    append gzip [zlib deflate $content 9]

    # append CRC and ISIZE fields
    append gzip [zlib crc32 $gzip]
    append gzip [binary format i [string length $content]]

    dict set reply -gzip $gzip
    return $reply
}

proc closing {sock} {
    if {[catch {chan eof $sock} r] || $r} {
	# remote end closed - just forget it
	Disconnect $sock "Remote closed connection"; return
    } elseif {[catch {
	    chan configure $sock -blocking 0
	    read $sock
    } r eo]} {
	Debug.error "trying to close: '$r' ($eo)"
	Disconnect $sock "Remote closed connection - $r" $eo; return
    }
}

proc indicate {args} {
    thread::send -async $::thread::parent $args
}

# te - find and effect appropriate transfer encoding
proc te {reply} {
    # default to identity encoding
    set content [dict get $reply -content]

    if {![dict exists $reply -gzip]
	&& "gzip" in $::te_encodings
    } {
	set reply [gzip_content $reply]
    }

    # choose transfer encoding
    if {[dict exists $reply accept-encoding]
	&& ![dict exists $reply content-encoding]
    } {
	foreach en [split [dict get $reply accept-encoding] ","] {
	    lassign [split $en ";"] en pref
	    set en [string trim $en]
	    if {$en in $::te_encodings} {
		switch $en {
		    "gzip" { # substitute the gzipped form
			if {[dict exists $reply -gzip]} {
			    set content [dict get $reply -gzip]
			    dict set reply content-encoding gzip
			    set reply [Http Vary $reply Accept-Encoding User-Agent]
			    break
			}
		    }
		}
	    }
	}
    }
    return [list $reply $content]
}

# charset - ensure correctly encoded content
proc charset {reply} {
    if {[dict exists $reply -charset]} {
	return $reply	;# don't re-encode by charset
    }

    # handle charset for text/* types
    lassign [split [Dict get? $reply content-type] {;}] ct
    if {[string match text/* $ct]} {
	if {[dict exists $reply -charset]} {
	    set charset [dict get $reply -charset]
	} else {
	    set charset utf-8	;# default charset
	}
	dict set reply -charset $charset
	dict set reply content-type "$ct; charset=$charset"
	dict set reply -content [encoding convertto $charset [dict get $reply -content]]
    }
    return $reply
}

# Send - queue up a transaction response
#
# Arguments:
#	code	list of response code and (optional) error message
#
# Side Effects:
#	queues the response for sending by method responder
proc send {reply {cacheit 1}} {
    #set reply [Access log $reply]

    set sock [dict get $reply -sock]

    # fetch transaction from the caller's identity
    if {![dict exists $reply -transaction]} {
	# can't Send reply: no -transaction associated with request
	Debug.error {Send discarded: no transaction ($reply)}
	return
    } elseif {[dict get $reply -generation] != $::generation} {
	# this reply belongs to an older, disconnected Httpd generation.
	# we must discard it, because it was directed to a different client!
	Debug.error {Send discarded: out of generation '[dict get $reply -generation] != $::generation' ([set x $reply; dict set x -content <ELIDED>; dict set x -entity <ELIDED>; dict set x -gzip <ELIDED>; return $x])}
	return
    }
    set trx [dict get $reply -transaction]

    # discard duplicate responses
    if {[info exists ::satisfied($trx)]} {
	# a duplicate response has been sent - discard this
	# this could happen if a dispatcher sends a response,
	# then gets an error.
	Debug.error {Send discarded: duplicate ($reply)}
	return
    }

    if {[catch {
	# unpack and consume the reply from replies queue
	set code [dict get $reply -code]
	if {$code < 4} {
	    # this was a tcl code, not an HTTP code
	    set code 500
	}

	# allow domains to set their own http header tags
	foreach {n v} [Dict get? $reply -meta] {
	    dict set reply $n $v
	}

	# don't honour 1.0 keep-alives
	set close [expr {[dict get $reply -version] < 1.1}]
	Debug.close {version [dict get $reply -version] implies close=$close}

	# handle 'connection: close' indication
	foreach ct [split [Dict get? $reply connection] ,] {
	    if {[string tolower [string trim $ct]] eq "close"} {
		Debug.close {Tagging $sock close at connection:close request}
		set close 1
	    }
	}

	if {$close} {
	    # we're not accepting more input
	    # but we defer closing the socket until transmission's complete
	    chan configure $sock -blocking 0
	    readable $sock closing $sock
	}

	# set the informational error message
	if {[dict exists $reply -error]} {
	    set errmsg [dict get $reply -error]
	}
	if {![info exists errmsg] || ($errmsg eq "")} {
	    set errmsg [Http ErrorMsg $code]
	}

	#set header "HTTP/[dict get $reply -version] $code $errmsg\r\n"
	set header "HTTP/1.1 $code $errmsg\r\n"

	# format up the headers
	if {$code != 100} {
	    append header "Date: [Http Now]" \r\n
	    append header "Server: $::server_id" \r\n
	}

	# format up and send each cookie
	if {[dict exists $reply -cookies]} {
	    foreach cookie [Cookies format4server [dict get $reply -cookies]] {
		append header "set-cookie: $cookie\r\n"
	    }
	}

	# there is content data
	switch -glob -- $code {
	    204 - 304 - 1* {
		# 1xx (informational),
		# 204 (no content),
		# and 304 (not modified)
		# responses MUST NOT include a message-body
		set reply [expunge $reply]
		set content ""
		set cacheit 0	;# can't cache these
	    }

	    default {
		if {[dict exists $reply -content]} {
		    # correctly charset-encode content
		    set reply [charset $reply]
		
		    # also gzip content so cache can store that.
		    lassign [te $reply] reply content
		    
		    # ensure content-length is correct
		    dict set reply content-length [string length $content]
		} else {
		    set content ""	;# there is no content
		    dict set reply content-length 0
		    set cacheit 0	;# can't cache no content
		}
	    }
	}

	# handle Vary field
	if {[dict exists $reply -vary]} {
	    if {[dict exists $reply -vary *]} {
		dict set reply vary *
	    } else {
		dict set reply vary [join [dict keys [dict get $reply -vary]] ,]
	    }
	    dict unset reply -vary
	}

	# now attend to caching generated content.
	if {$cacheit} {
	    # use -dynamic flag to avoid caching even if it was requested
	    set cacheit [expr {
			       ![dict exists $reply -dynamic]
			       || ![dict get $reply -dynamic]
			   }]

	    if {$cacheit
		&& [dict exists $reply cache-control]
	    } {
		set cacheable [split [dict get $reply cache-control] ,]
		foreach directive $cacheable {
		    set body [string trim [join [lassign [split $directive =] d] =]]
		    set d [string trim $d]
		    if {$d in {no-cache private}} {
			set cacheit 0
			break
		    }
		}
	    }

	    if {$cacheit} {
		# generate an etag for cacheable responses
		dict set reply etag "\"[::thread::id].[clock microseconds]\""
	    }
	}

	if {$code >= 500} {
	    # Errors are completely dynamic - no caching!
	    set cacheit 0
	}

	if {$content eq ""} {
	    # if the content is empty - make sure the headers are consistent
	    dict set reply content-length 0
	    catch {dict unset reply content-type}
	    catch {dict unset reply content-encoding}
	}

	# add in Auth header elements - TODO
	foreach challenge [Dict get? $reply -auth] {
	    append header "WWW-Authenticate: $challenge" \r\n
	}

	if {[dict get $reply -method] eq "HEAD"} {
	    # All responses to the HEAD request method MUST NOT
	    # include a message-body but may contain all the content
	    # header fields.
	    set content ""
	}

	if {!$cacheit} {
	    # dynamic stuff - no caching!
	    set reply [Http NoCache $reply]
	}

	Debug.log {Sending: [set x $reply; dict set x -entity <ELIDED>; dict set x -content <ELIDED>; dict set x -gzip <ELIDED>; return $x]}

	# strip http fields which don't have relevance in response
	dict for {n v} $reply {
	    set nl [string tolower $n]
	    if {$nl ni {server date}
		&& [info exists ::Http::headers($nl)]
		&& $::Http::headers($nl) ne "rq"
	    } {
		append header "$n: $v" \r\n
	    }
	}

	# record transaction reply and kick off the responder
	# response has been collected and is pending output
	# queue up response for transmission
	set ::replies($trx) [list $header $content $close]
	set ::satisfied($trx) 1		;# request has been satisfied
	writable $sock responder $sock	;# kick off transmitter

	Debug.http {ADD TRANS: $header ([array names ::replies])}

	# global consequences - botting and caching
	if {[dict exists $reply -bot_change]} {
	    # this is a newly detected bot - inform parent
	    dict set enbot -bot [dict get $reply -bot]
	    set ip [dict get $reply -ipaddr]
	    if {[::ip::type $ip] ne "normal"
		&& [dict exists $reply x-forwarded-for]
	    } {
		set ip [lindex [split [dict get $reply x-forwarded-for] ,] 0]
	    }
	    dict set enbot -ipaddr $ip
	    indicate Honeypot bot? $enbot
	} elseif {$cacheit} {
	    # handle caching (under no circumstances cache bot replies)
	    dict set reply -code $code
	    indicate Cache put $reply
	}
    } r eo]} {
	Debug.error {Sending Error: '$r' ($eo)}
    } else {
	#Debug.log {Sent: ($header) ($content)}
    }
}

# Disconnect - a fatal socket-level error has occurred
# close everything, report the failure to parent
proc Disconnect {sock error {eo {}}} {
    foreach timer [Timer info instances] {
	$timer cancel
    }

    variable request
    lappend ::req_log $::request

    Debug.socket {Disconnect: $sock ([chan names sock*]) - '$error' ($request)}
    Debug.close {Disconnecting: '$error' ($eo)}

    ;# remove socket
    catch {chan event $sock writable {}}
    catch {chan event $sock readable {}}
    catch {close $sock}

    # inform parent of Disconnect - this thread will now be recycled
    indicate Httpd Disconnect [::thread::id] $sock $error $eo
}

# Handle - handle a protocol error
#
proc Handle {req} {
    Debug.error {Handle: ([set x $req; dict set x -content <ELIDED>; dict set x -entity <ELIDED>; dict set x -gzip <ELIDED>; return $x])}

    set sock [dict get $req -sock]
    readable $sock	;# suspend reading
    catch {rxtimer cancel}
    if {[catch {
	dict set req connection close
	if {![dict exists $req -transaction]} {
	    dict set req -transaction $::transaction
	}
	dict set req -generation $::generation
	send $req 0			;# send our own reply
    } r eo]} {
	dict append req -error "(handler '$r' ($eo))"
	#set req [Access log $request]
	Debug.error {'Handle' error: '$r' ($eo)}
    }

    #Disconnect [dict get $req -sock] [Dict get? $req -error] $::req
}

# we're finished reading the header - inform the parent that work is needed
proc got {req} {
    catch {rxtimer cancel}
    Debug.socket {got: $req}

    variable request $req
    set sock [dict get $req -sock]

    readable $sock	;# suspend reading
    if {[catch {
	#dict set request -received [clock seconds]
	dict set request -received [clock microseconds]

	# rename fields whose names are the same in request/response
	foreach n {cache-control} {
	    if {[dict exists $request $n]} {
		dict set request -$n [dict get $request $n]
		dict unset request $n
	    }
	}

	# fix up non-standard X-Forwarded-For field
	if {[dict exists $request x-forwarded-for]} {
	    set xff [string trim [lindex [split [dict get $request x-forwarded-for] ,] 0]]
	    if {![Http nonRouting? $xff]} {
		dict set request -x-forwarding [Dict get? $request -ipaddr]
		dict set request -ipaddr $xff
	    }
	}

	dict set request -transaction [incr ::transaction]

	#set request [Access log $request]	;# log the request

	# inform parent of parsing completion
	indicate Httpd got [::thread::id] $request
	lappend ::req_log $::request
    } r eo]} {
	Debug.error {'get' error: '$r' ($eo)}
    }

    # reset the request dict to this connection prototype
    set request $::prototype
    readable $sock get $sock	;# resume reading
}

# gzip - 
proc gzip {} {
    variable request
    dict set request -entity [zlib deflate [dict get $request -entity]]
}

# read the entity, informing parent when complete
proc identity {sock length} {
    variable request
    rxtimer cancel

    if {[catch {
	# read as much of the entity as is available
	dict set request -left [expr {$length - [string bytelength [dict get $request -entity]]}]
	dict append request -entity [read $sock [dict get $::request -left]]

	if {[string bytelength [dict get $request -entity]] == $length} {
	    readable $sock	;# disable reading
	    # completed entity - invoke continuation
	    foreach te [Dict get? $request -te] {
		$te
	    }
	    got $request
	} else {
	    rxtimer after $::enttime timeout rxtimer $sock "identity timeout"
	}
    } r eo]} {
	Debug.error {identity error '$r' ($eo)}
    }
}

proc chunk {sock} {
    if {[file eof $sock]} {
	Disconnect $sock chunk; return
    }
}

proc start_transfer {} {
    variable request

    # start the transmission of POST entity, if necessary/possible
    if {
	[dict get $request -version] >= 1.1
	&& [dict exists $request expect]
	&& [string match *100-continue* [string tolower [dict get $request expect]]]
    } {
	# the client wants us to tell it to continue
	# before reading the body.
	# Do so, then proceed to read
	puts -nonewline [dict get $request -sock] "HTTP/1.1 100 Continue\r\n"
    }
}

# Start reading an entity from the client.
# On completion use the supplied completion callback
proc entity {sock} {
    variable request
    if {[dict get $request -method] ne "POST"} {
	return 1 ;# not a post?  No entity available.
    }

    # rfc2616 4.3
    # The presence of a message-body in a request is signaled by the
    # inclusion of a Content-Length or Transfer-Encoding header field in
    # the request's headers.
    if {[dict exists $request transfer-encoding]} {
	set te [dict get $request transfer-encoding]

	# chunked 3.6.1
	# identity 3.6.2
	# gzip 3.5
	# compress 3.5
	# deflate 3.5
	set tels {}
	array set params {}
	variable te_encodings
	variable te_params
	foreach tel [split $te ,] {
	    set param [lassign [split $tel ";"] tel]
	    set tel [string trim $tel]
	    if {$tel ni $te_encodings} {
		# can't handle a transfer encoded entity
		Handle [Http NotImplemented $request "$tel transfer encoding"]
		return
		# see 3.6 - 14.41 for transfer-encoding
		# 4.4.2 If a message is received with both a Transfer-EncodIing
		# header field and a Content-Length header field,
		# the latter MUST be ignored.
	    } else {
		lappend tels $tel
		set params($tel) [split $param ";"]
	    }
	}

	dict set request -te $tels
	dict set request -te_params [array get params]

	if {0 && "chunked" in $tels} {
	    # not handling chunks
	    start_transfer
	    dict set request -entity "" ;# clear any old entity
	    readable $sock chunk $sock

	    return
	} else {
	    # it's *got* to be an identity transfer - strip it
	    set idx [lsearch $tels "identity"] 
	    set tels [lreplace $tels $idx $idx]
	}
    } else {
	dict set request -te {}
    }

    # this is a content-length driven entity transfer
    if {![dict exists $request content-length]} {
	# 411 Length Required
	Handle [Http Bad $request "Length Required" 411]
	return 0
    }

    set length [dict get $request content-length]
    if {$length == 0} {
	dict set request -entity {}
	return 1 ;# the entity, length 0, is therefore already read
	# 14.13: Any Content-Length greater than
	# or equal to zero is a valid value.
    }

    # enforce server limits on Entity length
    variable maxentity
    if {($maxentity > 0) && ($length > $maxentity)} {
	# 413 "Request Entity Too Large"
	Handle [Http Bad $request "Request Entity Too Large" 413]
	return 1
    }

    # start the copy of POST data
    rxtimer after $::enttime timeout rxtimer $sock "entity timeout"
    start_transfer
    dict set request -entity "" ;# clear any old entity
    readable $sock identity $sock $length

    return 0	;# we'll be handling the channel
}

# Parse the entire header in {$req -header}
proc parse {sock} {
    variable request
    Debug.socket {parse: $request} 3
    set header [dict get $request -header]
    #dict unset request -header	;# delete header

    # parse header body
    set key ""
    foreach line [lrange $header 1 end] {
	if {[string index $line 0] in {" " \t}} {
	    # header continuation line
	    # add to the key we're currently assembling
	    if {$key eq ""} {
		Handle [Http Bad $request "malformed header line '$line'"]
		return
	    }
	    dict append request $key " [string trim $line]"
	} else {
	    # this is a new field:value pair
	    set value [string trim [join [lassign [split $line ":"] key] ":"]]
	    set key [string tolower [string trim $key "- \t"]]

	    if {[dict exists $request $key]} {
		dict append request $key ",$value"
	    } else {
		dict set request $key $value
	    }

	    # limit size of each field
	    if {$::maxfield
		&& [string length [dict get $request $key]] > $::maxfield
	    } {
		Handle [Http Bad $request "Illegal header: '$line'"]
		return
	    }
	}
    }

    # we have completely parsed the header body.

    # parse request-line
    set line [split [lindex $header 0]]
    set head(-method) [string toupper [lindex $line 0]]
    set head(-version) [lindex $line end]
    set head(-uri) [join [lrange $line 1 end-1]]

    if {[string match HTTP/* $head(-version)]} {
	set head(-version) [lindex [split $head(-version) /] 1]

	# check URI length (per rfc2616 3.2.1
	# A server SHOULD return 414 (Requestuest-URI Too Long) status
	# if a URI is longer than the server can handle (see section 10.4.15).)
	if {$::maxurilen
	    && [string length $head(-uri)] > $::maxurilen
	} {
	    # send a 414 back
	    Handle [Http Bad $request "URI too long '$head(-uri)'" 414]
	    return
	}

	# record header data in request dict
	#set url [Url parse "http://$head(-uri)"]
	#puts stderr "got head: '$request', '[array get head]'"
	set request [dict merge $request [array get head]]
    } else {
	# Could check for FTP requestuests, etc, here...
	dict set request -error_line $line
	Handle [Http Bad $request "Method unsupported ([array get head])" 405]
	return
    }

    # Send 505 for protocol != HTTP/1.0 or HTTP/1.1
    if {([dict get $request -version] != 1.1)
	&& ([dict get $request -version] != 1.0)} {
	Handle [Http Bad $request "HTTP Version not supported" 505]
	return
    }

    # ensure that the client sent a Host: if protocol requires it
    if {[dict exists $request host]} {
	# client sent Host: field
	if {[string match http:* $head(-uri)]} {
	    # rfc 5.2 1 - a host header field must be ignored
	    # if request-line specified an absolute URL host/port
	    set request [dict merge $request [Url parse $head(-uri)]]
	    dict set request host [Url host $request]
	} else {
	    # no absolute URL was specified by the request-line
	    # use the Host field to determine the host
	    foreach c [split [dict get $request host] :] f {host port} {
		dict set request -$f $c
	    }
	    dict set request host [Url host $request]
	    set request [dict merge $request [Url parse http://[dict get $request host]$head(-uri)]]
	}
    } elseif {[dict get $request -version] > 1.0} {
	Handle [Http Bad $request "HTTP 1.1 is required to send Host request"]
	return
    } else {
	# HTTP 1.0 isn't required to send a Host request but we still need it
	set request [dict merge $request [Url parse $url]]
	if {![dict exists $request -host]} {
	    # make sure the request has some idea of our host&port
	    dict set request -host $::host
	    dict set request -port $::port
	    dict set request host [Url host $request]
	}
    }
    dict set request -url [Url url $request]	;# normalize URL

    # rfc2616 14.10:
    # A system receiving an HTTP/1.0 (or lower-version) message that
    # includes a Connection header MUST, for each connection-token in this
    # field, remove and ignore any header field(s) from the message with
    # the same name as the connection-token.
    if {[dict get $request -version] < 1.1
	&& [dict exists $request connection]
    } {
	foreach token [split [dict get $request connection] ","] {
	    catch {dict unset request [string trim $token]}
	}
	dict unset request connection
    }

    # completed request header decode - now dispatch on the URL

    #puts stderr "PARSE: $request"
    if {[dict get $request -uri] eq "/_error"} {
	error "Test background error handling"
    }

    # remove 'netscape extension' length= from if-modified-since
    if {[dict exists $request if-modified-since]} {
	dict set request if-modified-since [lindex [split [dict get $request if-modified-since] {;}] 0]
    }

    # block spiders by UA
    if {[info exists ::spiders([Dict get? $request user-agent])]} {
	indicate Httpd block [dict get $request -ipaddr] "spider UA"
	Handle [Http NotImplemented $request "Spider Service"]
	return
    }

    incr ::pending
    switch -- [dict get $request -method] {
	POST {
	    if {![dict exists $request content-length]} {
		# Send 411 for missing Content-Length on POST requests
		Handle [Http Bad $request "Length Required" 411]
		return
	    } else {
		# read the entity
		if {[entity $sock]} {
		    #puts stderr "Not Entity: $request"
		    got $request
		} else {
		    #puts stderr "yes Entity: $request"
		}
	    }
	}

	CONNECT {
	    # stop the bastard SMTP spammers
	    indicate Httpd block [dict get $request -ipaddr] "CONNECT method"
	    Handle [Http NotImplemented $request "Spider Service"]
	    return
	}

	default {
	    Debug.http {parse done: $request} 3
	    got $request
	}
    }
}

# get lines of header until it's complete
proc get {sock} {
    rxtimer cancel
    variable request

    Debug.socket {get: $request} 10
    
    if {[catch {
	chan gets $sock line
    } result eo]} {
	Disconnect $sock $result $eo	;# inform parent that we're done
	return
    }

    if {$result == -1} {
	readable $sock	;# completed reading
	if {[chan eof $sock]} {
	    # remote end closed - just forget it
	    Disconnect $sock "Remote closed connection"; return
	} elseif {$::maxline && [chan pending input $sock] > $::maxline} {
	    Handle [Http Bad $request "Line too long"]
	    return
	}

	rxtimer after $::enttime timeout rxtimer $sock "pre-read timeout"
	return
    }

    set line [string trim $line "\r"]
    if {[string trim $line] eq ""} {
	if {[dict exists $request -header]} {
	    # \n terminates the header - go parse it
	    readable $sock	;# completed reading
	    if {[catch {parse $sock} r eo]} {
		Debug.error {parse error: '$r' ($eo)}
	    }
	} else {
	    return	;# this is a leading empty line, ignore it:
	    # rfc2616 4.1: In the interest of robustness,
	    # servers SHOULD ignore any empty line(s)
	    # received where a Request-Line is expected.
	}
    } else {
	# accumulate header lines
	rxtimer after $::rxtime timeout rxtimer $sock "inter-read timeout"
	dict lappend request -header $line
	if {$::maxhead
	    && [llength [dict get $request -header]] > $::maxhead
	} {
	    Handle [Http Bad $request "Header too Large"]
	    return
	}
    }
}

variable prototype {}

# Parent thread will call connect with the pro-forma request
proc connect {req vars sock} {
    Debug.socket {[::thread::id] connect $req $vars $sock}

    # some code to detect races (we hope)
    set chans [chan names sock*]
    if {[llength $chans] > 1
	|| ([llength $chans] > 1)
    } {
	Debug.error {HRACE [::thread::id]: new req from $sock ($chans) - request:[catch {set ::request} xxx; set xxx] - pending: [set ::pending] - satisfied:([array get ::satisfied]) - replies:([array get ::replies]) - req_log: ($::req_log)}
    }

    array unset ::satisfied; array set ::satisfied {}	;# forget request state
    array unset ::replies; array set ::replies {}	;# forget pending replies
    catch {unset request}
    set ::req_log {}
    set ::pending 0		;# no pending requests
    set ::transaction -1
    set ::response -1

    variable {*}$vars	;# instantiate variables

    # remember the request prototype
    variable request $req
    dict set request -generation [incr ::generation]
    dict set request -sock $sock
    dict set request -worker [::thread::id]
    variable prototype $request

    rxtimer after $::txtime timeout rxtimer $sock "first-read timeout"
    readable $sock get $sock
    Debug.socket {[::thread::id] connected}
}

proc Disconnected {args} {
    # do something?
    Debug.log {Disconnected indication from parent: $args}
}

Debug on log 10
Debug off close 10
# now we're able to process commands
#puts stderr "Started Httpd Worker [::thread::id]"
thread::wait
#puts stderr "~Thread: [thread::id]"
