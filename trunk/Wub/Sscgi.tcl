# Sscgi - Simple Common Gateway Interface Server
# Derived from http://wiki.tcl.tk/19670 by Mark Janssen (http://wiki.tcl.tk/14766)

if {[info exists argv0] && ($argv0 eq [info script])} {
    lappend auto_path [file dirname [info script]]
}

package provide Sscgi 2.0
package require Debug
package require Url

Debug define sscgi 10

set ::API(Utilities/Scgi) {
    {
	Implements the SCGI interface.
    }
}

oo::class create ::Sscgi {
    method eof {args} {
	Debug.sscgi {eof: ($args)}
    }

    # Send - send a reply down the wire
    method send {reply} {
	if {[catch {
	    set sock [dict get $reply -sock]
	    
	    # wire-format the reply transaction
	    lassign [::Httpd Format $reply 0] reply cache header content file close? empty range

	    #set header "Status: $header" ;# add the SCGI signifier
	    set code [dict get? $reply -code]
	    if {$code eq ""} {
		set code 200
	    }
	    set status "Status: $code [Http ErrorMsg $code]"

	    # send reply to actual server
	    chan configure $sock -translation {crlf binary}
	    puts $sock $status
	    puts $sock $header

	    if {!$empty} {
		chan configure $sock -encoding binary
		puts $sock $content
	    }
	    flush $sock

	    # get back to listening for more requests
	    chan close $sock
	    #chan configure $sock -blocking 0 -translation {binary crlf}
	    #chan event $sock readable [list [self] read_length $sock]
	} r eo]} {
	    Debug.error {SCGI Sending Error: '$r' ($eo)}
	} else {
	    Debug.sscgi {Sent: ($status) ($header)}
	}
    }

    # translate - translate headers into Wub form
    method translate {headers} {
	classvar trans	;# translation map
	if {![info exists trans]} {
	    lappend trans AUTH_TYPE -auth_type ;# If the server supports user authentication, and the script is protects, this is the protocol-specific authentication method used to validate the user.
	    lappend trans CONTENT_LENGTH content-length ;# The length of the said content as given by the client.
	    lappend trans CONTENT_TYPE content-type ;# For queries which have attached information, such as HTTP POST and PUT, this is the content type of the data.
	    lappend trans DOCUMENT_ROOT -docroot ;# server's docroot
	    lappend trans DOCUMENT_URI -path
	    lappend trans GATEWAY_INTERFACE -cgi	;# The revision of the CGI specification to which this server complies. Format: CGI/revision
	    lappend trans PATH_INFO -pathinfo ;# The extra path information, as given by the client. In other words, scripts can be accessed by their virtual pathname, followed by extra information at the end of this path. The extra information is sent as PATH_INFO. This information should be decoded by the server if it comes from a URL before it is passed to the CGI script.
	    lappend trans PATH_TRANSLATED -translated ;# The server provides a translated version of PATH_INFO, which takes the path and does any virtual-to-physical mapping to it.
	    lappend trans QUERY_STRING -query ;# The information which follows the ? in the URL which referenced this script. This is the query information. It should not be decoded in any fashion. This variable should always be set when there is query information, regardless of command line decoding.
	    lappend trans REDIRECT_STATUS -status
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
	}

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

	catch {dict unset result -chunked}	;# we don't do chunked

	lappend result -scgi [list $headers]
	lappend result -scheme http
	lappend result -url [Url url $result]
	lappend result -uri [Url url $result]
	return $result
    }

    method read_body {sock headers content_length body args} {
	Debug.sscgi {read_body $sock headers:($headers) content_length:$content_length body:($body) args($args)}

	if {[eof $sock]} {
	    tailcall close $sock	;# connection closed without data
	}

	append body [read $sock]
	if {[string length $body] < $content_length} {
	    # we don't have the complete body yet, wait for more
	    tailcall chan event $sock readable [list [self] read_body $sock $headers $content_length $body {*}$args]
	}

	Debug.sscgi {SCGI body read sock:$sock: headers:($headers) body:($body) args:($args)}

	catch {
	    # perform some translations of various fields
	    set r $args
	    dict set r -entity $body
	    dict set r -socket $sock
	    dict set r -content_length $content_length
	    dict set r -received [clock microseconds]
	    dict set r -version 1.1		;# HTTP version

	    set r [dict merge $r [my translate $headers]]
	    
	    Debug.sscgi {pre: ($r)}
	    set r1 [::Dispatcher pre $r]
	    set r1 [::Httpd timestamp $r1 dispatch]
	    Debug.sscgi {pre: ($r)}
	    ::Dispatcher do REQUEST $r1
	} rsp eo	;# process the request
	Debug.sscgi {Dispatched: ($eo) rsp:($rsp)}
	set rsp [::Httpd timestamp $rsp postprocess]

	# handle response code from processing request
	set done 0
	switch -- [dict get $eo -code] {
	    0 -
	    2 {
		# does application want to suspend?
		if {[dict size $rsp] == 0 || [dict exists $rsp -suspend]} {
		    if {[dict size $rsp] == 0} {
			set duration 0
		    } else {
			set duration [dict get $rsp -suspend]
		    }

		    Debug.httpd {SUSPEND: $duration}
		    ::watchdog grace $duration	;# response has been suspended
		    incr done
		} elseif {[dict exists $rsp -passthrough]} {
		    # the output is handled elsewhere (as for WOOF.)
		    # so we don't need to do anything more.
		    incr done
		}

		# ok - return
		if {![dict exists $rsp -code]} {
		    set rsp [Http Ok $rsp]	;# default to OK
		}
	    }

	    1 { # error - return the details
		set rsp [Http ServerError $r $rsp $eo]
	    }
	    default {
		puts stderr "got $code"
	    }
	}

	if {!$done} {
	    ::watchdog stroke [self]
	    if {[catch {
		::Dispatcher post $rsp	;# postprocess the response
	    } rspp eo]} {
		# post-processing error - report it
		Debug.error {[self] postprocess error: $rspp ($eo)} 1
		::watchdog stroke [self]

		# report error from post-processing
		my send [::convert convert [Http ServerError $r $rspp $eo]]
	    } else {
		# send the response to client
		Debug.httpd {[info coroutine] postprocess: [Httpd dump $rspp]} 10
		::watchdog stroke [self]

		# does post-process want to suspend?
		if {[dict size $rspp] == 0 || [dict exists $rspp -suspend]} {
		    if {[dict size $rspp] == 0} {
			# returning a {} from postprocess suspends it (really?)
			set duration 0
		    } else {
			# set the grace duration as per request
			set duration [dict get $rspp -suspend]
		    }

		    Debug.httpd {SUSPEND in postprocess: $duration}
		    ::watchdog grace $duration	;# response has been suspended for $duration
		} elseif {[dict exists $rspp -passthrough]} {
		    # the output is handled elsewhere (as for WOOF.)
		    # so we don't need to do anything more.
		} else {
		    my send $rspp	;# send the response through to client
		}
	    }
	}
    }

    method read_headers {sock length read_data args} {
	if {[eof $sock]} {
	    tailcall close $sock	;# connection closed without data
	}

	append read_data [read $sock]
	
	if {[string length $read_data] < $length+1} {
	    # we don't have the complete headers yet, wait for more
	    Debug.sscgi {read_headers $sock $length $args ($read_data)}
	    tailcall chan event $sock readable [list [self] read_headers $sock $length $read_data $args]
	} else {
	    set headers [string range $read_data 0 $length-1]
	    set headers [lrange [split $headers \0] 0 end-1]
	    set body [string range $read_data $length+1 end]
	    set content_length [dict get $headers CONTENT_LENGTH]
	    my read_body $sock $headers $content_length $body {*}$args
	}
    }
    
    method read_length {sock args} {
	set length {}
	while {1} {
	    set c [read $sock 1]
	    if {[eof $sock]} {
		tailcall close $sock	;# connection closed without data
	    }
	    if {$c eq ":"} {
		Debug.sscgi {read_length $sock $args -> $length}
		tailcall chan event $sock readable [list [self] read_headers $sock $length {} {*}$args]
	    }
	    append length $c
	}
    }

    # Connect - a client has connected
    method connect {sock ip port args} {
	Debug.sscgi {Connect $sock $ip $port $args}
	chan configure $sock -blocking 0 -translation {binary crlf}
	chan event $sock readable [list [self] read_length $sock -sock $sock -ipaddr $ip -rport $port {*}$args]
    }

    constructor {args} {
	variable {*}$args
    }
}

if {[info exists argv0] && ($argv0 eq [info script])} {
}

::Sscgi create ::SscgiI	;# create an instance

#### Wub Listener interface
# Listener listen -host $host -port $listener_port -httpd Scgi -dispatch {Backend Incoming}
# vim: ts=8:sw=4:noet
