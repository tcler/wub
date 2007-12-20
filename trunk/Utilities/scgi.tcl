# scgi - Simple Common Gateway Interface
# Derived from http://wiki.tcl.tk/19670 by Mark Janssen (http://wiki.tcl.tk/14766)

if {[info exists argv0] && ($argv0 eq [info script])} {
    lappend auto_path [file dirname [info script]]
}

package provide scgi 1.0
package require Debug
Debug on scgi 10

namespace eval scgi {

    # listen - handle incoming connections
    proc listen {port} {
	socket -server [namespace code Connect] $port
    }

    # Connect - a client has connected
    proc Connect {sock ip port args} {
	Debug.scgi {Connect $sock $ip $port $args}
	fconfigure $sock -blocking 0 -translation {binary crlf}
	fileevent $sock readable [namespace code [list read_length $sock -sock $sock -ipaddr $ip -rport $port {*}$args]]
    }

    proc Disconnect {id error {eo ""}} {
    }

    proc read_length {sock args} {
	set length {}
	while {1} {
	    set c [read $sock 1]
	    if {[eof $sock]} {
		close $sock
		return
	    }
	    if {$c eq ":"} {
		fileevent $sock readable [namespace code [list read_headers $sock $length {} {*}$args]]
		return
	    }
	    append length $c
	}
    }

    proc read_headers {sock length read_data args} {
	append read_data [read $sock]
	
	if {[string length $read_data] < $length+1} {
	    # we don't have the complete headers yet, wait for more
	    fileevent $sock readable [namespace code [list read_headers $sock $length $read_data] {*}$args]
	    return
	} else {
	    set headers [string range $read_data 0 $length-1]
	    set headers [lrange [split $headers \0] 0 end-1]
	    set body [string range $read_data $length+1 end]
	    set content_length [dict get $headers CONTENT_LENGTH]
	    read_body $sock $headers $content_length $body {*}$args
	}
    }
    
    proc read_body {sock headers content_length body args} {
	append body [read $sock]
	if {[string length $body] < $content_length} {
	    # we don't have the complete body yet, wait for more
	    fileevent $sock readable [namespace code [list read_body $sock $headers $content_length $body {*}$args]]
	    return
	}
	Debug.scgi {SCGI $sock: $headers ($args)}

	if {[dict exists $args -dispatch]} {
	    # perform some translations of various fields
	    dict set args -entity $body
	    dict set args -socket $sock
	    dict set args -content_length $content_length
	    if {[catch {
		{*}[dict get $args -dispatch] {*}$args {*}[translate $headers]
	    } r eo]} {
		Debug.error {SCGI Error: $r ($eo)}
	    }
	} else {
	    handle_request $sock $headers $body
	}
    }

    variable trans	;# translation map

    lappend trans AUTH_TYPE -auth_type ;# If the server supports user authentication, and the script is protects, this is the protocol-specific authentication method used to validate the user.
    lappend trans CONTENT_LENGTH content-length ;# The length of the said content as given by the client.
    lappend trans CONTENT_TYPE content-type ;# For queries which have attached information, such as HTTP POST and PUT, this is the content type of the data.
    lappend trans DOCUMENT_ROOT -docroot ;# server's docroot
    lappend trans GATEWAY_INTERFACE -cgi	;# The revision of the CGI specification to which this server complies. Format: CGI/revision
    lappend trans PATH_INFO -pathinfo ;# The extra path information, as given by the client. In other words, scripts can be accessed by their virtual pathname, followed by extra information at the end of this path. The extra information is sent as PATH_INFO. This information should be decoded by the server if it comes from a URL before it is passed to the CGI script.
    lappend trans PATH_TRANSLATED -translated ;# The server provides a translated version of PATH_INFO, which takes the path and does any virtual-to-physical mapping to it.
    lappend trans QUERY_STRING -query ;# The information which follows the ? in the URL which referenced this script. This is the query information. It should not be decoded in any fashion. This variable should always be set when there is query information, regardless of command line decoding.
    lappend REDIRECT_STATUS -status
    lappend trans REMOTE_ADDR -ripaddr ;# The IP address of the remote host making the request.
    lappend trans REMOTE_HOST	-rhost ;# The hostname making the request. If the server does not have this information, it should set REMOTE_ADDR and leave this unset.
    lappend trans REMOTE_IDENT -ident ;# If the HTTP server supports RFC 931 identification, then this variable will be set to the remote user name retrieved from the server. Usage of this variable should be limited to logging only.
    lappend trans REMOTE_USER -user ;# If the server supports user authentication, and the script is protected, this is the username they have authenticated as.
    lappend trans REQUEST_METHOD -method ;# The method with which the request was made. For HTTP, this is "GET", "HEAD", "POST", etc.
    lappend trans SCRIPT_NAME	-path ;# A virtual path to the script being executed, used for self-referencing URLs.

    lappend trans SERVER_ADDR -addr	;# The address to which the request was sent.
    lappend trans SERVER_NAME -host ;# The server's hostname, DNS alias, or IP address as it would appear in self-referencing URLs.
    lappend trans SERVER_PORT -port	;# The port number to which the request was sent.
    lappend trans SERVER_PROTOCOL -protocol ;# The name and revision of the information protcol this request came in with. Format: protocol/revision
    lappend trans SERVER_SOFTWARE -server ;# The name and version of the information server software answering the request (and running the gateway). Format: name/version

    # translate - translate headers into Wub form
    proc translate {headers} {
	variable trans

	set result {}
	foreach {from to} $trans {
	    if {[dict exists $headers $from]} {
		lappend result $to [dict get $headers $from]
		dict unset headers $from
	    }
	}

	foreach {n v} $headers {
	    if {[string match HTTP_* $n]} {
		lappend result [string map {_ -} [string range $n 5 end]] $v
		dict unset headers $n
	    }
	}
	lappend result -scgi [list $headers]

	return $result
    }

    proc Send {reply} {
	#set reply [Access log $reply]

	set sock [dict get $reply -sock]
	upvar #0 ::HttpdWorker::connections($sock) connection

	if {[catch {
	    # unpack and consume the reply from replies queue
	    set code [dict get $reply -code]
	    if {$code < 4} {
		# this was a tcl code, not an HTTP code
		set code 500
	    }

	    # set the informational error message
	    if {[dict exists $reply -error]} {
		set errmsg [dict get $reply -error]
	    }
	    if {![info exists errmsg] || ($errmsg eq "")} {
		set errmsg [Http ErrorMsg $code]
	    }

	    #set header "HTTP/[dict get $reply -version] $code $errmsg\r\n"
	    set header "Status: $code $errmsg\r\n"

	    # format up the headers
	    if {$code != 100} {
		append header "Date: [Http Now]" \r\n
		set si [Dict get? $reply -server_id]
		if {$si eq ""} {
		    set si "The Wub"
		}
		append header "Server: $si" \r\n
	    }

	    # format up and send each cookie
	    if {[dict exists $reply -cookies]} {
		set c [dict get $reply -cookies]
		foreach cookie [Cookies format4server $c] {
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
		    set server_cache_it 0	;# can't cache these
		    set no_content 1
		}

		default {
		    set no_content 0
		    if {[dict exists $reply -content]} {
			# correctly charset-encode content
			set reply [charset $reply]

			# also gzip content so cache can store that.
			lassign [CE $reply] reply content

			if {[dict exists $reply -chunked]} {
			    # ensure chunking works properly
			    set chunkit [dict get $reply -chunked]
			    dict set reply transfer-encoding chunked
			    catch {dict unset reply content-length}
			} else {
			    # ensure content-length is correct
			    dict set reply content-length [string length $content]
			}
		    } else {
			set content ""	;# there is no content
			dict set reply content-length 0
			set server_cache_it 0	;# can't cache no content
		    }
		}
	    }

	    # handle Vary field and -vary dict
	    if {[dict exists $reply -vary]} {
		if {[dict exists $reply -vary *]} {
		    dict set reply vary *
		} else {
		    dict set reply vary [join [dict keys [dict get $reply -vary]] ,]
		}
		dict unset reply -vary
	    }

	    if {[dict get $reply -method] eq "HEAD"} {
		# All responses to the HEAD request method MUST NOT
		# include a message-body but may contain all the content
		# header fields.
		set no_content 1
		set content ""
	    }

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

	    # send reply
	    puts $sock $header
	    puts $sock ""
	    puts $sock $content
	    close $sock

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
	    }
	} r eo]} {
	    Debug.error {Sending Error: '$r' ($eo)}
	} else {
	    #Debug.log {Sent: ($header) ($content)}
	}
    }

    # handle_request - generate some test responses
    proc handle_request {sock headers body} {
	package require html
	array set Headers $headers
	
	#parray Headers
	puts $sock "Status: 200 OK"
	puts $sock "Content-Type: text/html"
	puts $sock ""
	puts $sock "<HTML>"
	puts $sock "<BODY>"
	puts $sock [::html::tableFromArray Headers]
	puts $sock "</BODY>"
	puts $sock "<H3>Body</H3>"
	puts $sock "<PRE>$body</PRE>"
	if {$Headers(REQUEST_METHOD) eq "GET"} {
	    puts $sock {<FORM METHOD="post" ACTION="/scgi">}
	    foreach pair [split $Headers(QUERY_STRING) &] {
		lassign [split $pair =] key val
		puts $sock "$key: [::html::textInput $key $val]<BR>"
	    }
	    puts $sock "<BR>"
	    puts $sock {<INPUT TYPE="submit" VALUE="Try POST">}
	} else {
	    puts $sock {<FORM METHOD="get" ACTION="/scgi">}
	    foreach pair [split $body &] {
		lassign [split $pair =] key val
		puts $sock "$key: [::html::textInput $key $val]<BR>"
	    }
	    puts $sock "<BR>"
	    puts $sock {<INPUT TYPE="submit" VALUE="Try GET">}
	}
	
	puts $sock "</FORM>"
	puts $sock "</HTML>"
	close $sock
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    ### Stand-alone configuration
    puts stderr "Listening"
    scgi::listen 8088
    vwait forever
}

#### Wub Listener interface
# Listener listen -host $host -port $listener_port -httpd scgi -dispatch {Backend Incoming}
