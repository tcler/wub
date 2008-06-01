# HttpdWorker - Httpd Protocol worker thread

# TODO: armour all [chan event]s
package require Debug
package require spiders
#package require Access
#Access open

#puts stderr "Starting Httpd Worker [id]"
proc bgerror {error eo} {
    Debug.error {Worker Error: $error ($eo)}
    #catch {
	#dict lappend ::request -debug bgerror [list [clock seconds] $error $eo]
    #}
    #catch {
	#Disconnect [dict get $::request -sock] $error $eo
    #}
}

interp bgerror {} ::bgerror

#puts stderr "Thread: [id]";
interp alias {} armour {} string map {& &amp; < &lt; > &gt; \" &quot; ' "&#39;"}

package require WubUtils
package require Debug
Debug off url
Debug off http 10
Debug off cookies
Debug off close 10
Debug off socket 10

package require Url
package require Http
package require Cookies
package require UA

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

namespace eval HttpdWorker {

    variable requests	;# array of dicts containing requests read
    variable gen_count		;# worker/connection association generation

    # limits on header size
    variable maxline 2048	;# max request line length
    variable maxfield 4096	;# max request field length
    variable maxhead 1024	;# maximum number of header lines
    variable maxurilen 1024	;# maximum URI length
    variable maxentity -1		;# maximum entity size

    variable txtime 20000	;# inter-write timeout
    variable rxtime 10000	;# inter-read timeout
    variable enttime 10000	;# entity inter-read timeout

    # arrange gzip Transfer Encoding
    if {![catch {package require zlib}]} {
	variable ce_encodings {gzip}
    } else {
	variable ce_encodings {}
    }
    #set ce_encodings {}	;# uncomment to stop gzip transfers
    variable te_encodings {chunked}

    proc restart {sock timer} {
	upvar #0 ::HttpdWorker::connections($sock) connection
	catch {::after cancel [dict get $connection timers $timer]}
	dict set connection timers $timer [::after {*}[dict get $connection timer_args]]
    }

    # timeout - handle t
    proc timeout {sock timer args} {
	upvar #0 ::HttpdWorker::connections($sock) connection
	upvar #0 ::HttpdWorker::requests($sock) request
	Debug.timer {timeout '$args': Connection: ($connection) Request: ($request)}
	Disconnect $sock $args
    }

    proc after {sock timer when msg} {
	upvar #0 ::HttpdWorker::connections($sock) connection
	set cmd [list $when [namespace code [list timeout $sock $timer [list $msg]]]]
	dict set connection timer_args $timer $cmd
	dict set connection timers $timer [::after {*}$cmd]
    }

    proc cancel {sock timer} {
	upvar #0 ::HttpdWorker::connections($sock) connection
	catch {::after cancel [dict get $connection timers $timer]}
    }

    # readable - make socket readable
    proc readable {sock args} {
	upvar #0 ::HttpdWorker::connections($sock) connection
	dict set connection readable $args
	if {[llength $args]} {
	    set args [namespace code $args]
	}
	if {[catch {chan event $sock readable $args} r eo]} {
	    Debug.error "readable: '$r' ($eo)"
	    Disconnect $sock $r $eo; return
	}
    }

    # writable - make socket writable
    proc writable {sock args} {
	upvar #0 ::HttpdWorker::connections($sock) connection
	dict set connection writable $args
	if {[llength $args]} {
	    set args [namespace code $args]
	}
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
	upvar #0 ::HttpdWorker::connections($sock) connection
	Debug.http {RESPONDER $sock: [dict keys [dict get $connection replies]]} 4

	if {[eof $sock]} {
	    Disconnect $sock "Lost connection on transmit"; return
	}

	set close 0
	set content ""

	if {[catch {
	    catch {cancel $sock txtimer}

	    # determine next available response in transaction# order
	    set next [lindex [lsort -integer [dict keys [dict get $connection replies]]] 0]

	    # ensure we don't send responses out of sequence
	    if {$next eq {}
		|| $next > ([dict get $connection response] + 1)} {
		# something's blocking the response pipeline
		# we don't have a response for the next transaction.

		# we have to wait until all the preceding transactions
		# have something to send
		Debug.http {no pending or '$next' doesn't follow '[dict get $connection response]'}

		writable $sock	;# disable responder
		variable txtime
		after $sock txtimer $txtime "responder pending"
	    } else {
		# we're going to respond to the next transaction in trx order
		# unpack and consume the reply from replies queue
		lassign [dict get $connection replies $next] head content close chunk no_content

		# remove this response from the pending response structure
		dict set connection response $next	;# move to next response
		dict unset connection replies $next	;# consume next reply

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
		chan flush $sock			;# no, really, send it
		dict set connection sent $head
		Debug.socket {SENT: $sock $head'} 4

		# send the content/file (if any)
		# note: we must *not* send a trailing newline, as this
		# screws up the content-length and confuses the client
		# which doesn't then pick up the next response
		# in the pipeline
		if {!$no_content} {
		    if {$chunk} {
			while {[string length $content] > 0} {
			    if {[string length $content] > $chunk} {
				puts -nonewline $sock "[format %0x $chunk]\r\n"
				puts -nonewline $sock "[string range $content 0 $chunk-1]\r\n"
				set content [lrange $content $chunk end]
				Debug.socket {SENT CHUNKED: $chunk bytes} 8
			    } else {
				# send last chunk
				puts -nonewline $sock "[format %0x [string length $content]]\r\n"
				puts -nonewline $sock "${content}\r\n"
				Debug.socket {SENT CHUNKED: [string length $content] bytes} 8
				set content ""
			    }
			    chan flush $sock	;# no, really, send it
			}
			puts -nonewline $sock "0\r\n\r\n" ;# end chunked
		    } else {
			puts -nonewline $sock $content	;# send the content
			Debug.socket {SENT BYTES: [string length $content] bytes} 8
		    }
		}

		chan flush $sock			;# no, really, send it

		dict incr connection pending -1 ;# count one fewer request pending
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
	    Debug.socket {SENT (close: $close) [string length $content] '$content'} 10
	}
    }

    # closing - we are closing the connection, or recognising that it's closed.
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

    # dump - return a stripped request for printing
    proc dump {req} {
	dict set req -content <ELIDED>
	dict set req -entity <ELIDED>
	dict set req -gzip <ELIDED>
	return $req
    }

    # trx_check - get transaction from reply, check for consistency
    proc trx_check {r} {
	set sock [dict get $r -sock]
	upvar #0 ::HttpdWorker::connections($sock) connection

	# fetch transaction from the caller's identity
	variable generation
	if {![dict exists $r -transaction]} {
	    # can't Send reply: no -transaction associated with request
	    Debug.error {Send discarded: no transaction ($r)}
	    return -1
	} elseif {[dict get $r -generation]
		  != [dict get $connection generation]
	      } {
	    # this reply belongs to an older, disconnected Httpd generation.
	    # we must discard it, because it was directed to a different client!
	    Debug.error {Send discarded: out of generation '[dict get $r -generation] != [dict get $connection generation]' ([dump $r])}
	    return -1
	}
	set trx [dict get $r -transaction]

	# discard duplicate responses

	if {[dict exists $connection satisfied $trx]} {
	    # a duplicate response has been sent - discard this
	    # this could happen if a dispatcher sends a response,
	    # then gets an error.
	    Debug.error {Send discarded: duplicate ($r)}
	    return -1
	}

	return $trx
    }

    # close? - should we close this connection?
    proc close? {r} {
	# don't honour 1.0 keep-alives
	set close [expr {[dict get $r -version] < 1.1}]
	Debug.close {version [dict get $r -version] implies close=$close}

	# handle 'connection: close' indication
	foreach ct [split [Dict get? $r connection] ,] {
	    if {[string tolower [string trim $ct]] eq "close"} {
		Debug.close {Tagging close at connection:close request}
		set close 1
	    }
	}

	if {$close} {
	    # we're not accepting more input
	    # but we defer closing the socket until transmission's complete
	    set sock [dict get $r -sock]
	    chan configure $sock -blocking 0
	    readable $sock closing $sock
	}

	return $close
    }

    # Send - queue up a transaction response
    #
    # Arguments:
    #	code	list of response code and (optional) error message
    #
    # Side Effects:
    #	queues the response for sending by method responder
    proc Send {reply {cache 1}} {
	#set reply [Access log $reply]
	if {[dict exists $reply -suspend]} {
	    return	;# this reply has been suspended
	}

	set sock [dict get $reply -sock]
	upvar #0 ::HttpdWorker::connections($sock) connection

	# fetch transaction from the caller's identity
	set trx [trx_check $reply]
	if {$trx < 0} {
	    return	;# invalid reply
	}

	if {[catch {
	    variable ce_encodings	;# what encodings do we support?

	    # wire-format the reply transaction
	    lassign [Http Send $reply -cache $cache -encoding $ce_encodings] reply header content empty cache
	    set header "HTTP/1.1 $header" ;# add the HTTP signifier

	    # record transaction reply and kick off the responder
	    # response has been collected and is pending output
	    # queue up response for transmission
	    set chunkit [expr {[Dict get? $reply transfer-encoding] eq "chunked"}]
	    dict set connection replies $trx [list $header $content [close? $reply] $chunkit $empty]
	    dict set connection satisfied $trx 1 ;# request has been satisfied
	    writable $sock responder $sock	;# kick off transmitter

	    Debug.http {ADD TRANS: $header ([array names ::replies])}

	    # global consequences - botting and caching
	    if {![indicate Honeypot newbot? $reply] && $cache} {
		# handle caching (under no circumstances cache bot replies)
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
	upvar #0 ::HttpdWorker::requests($sock) request
	upvar #0 ::HttpdWorker::connections($sock) connection
	dict lappend connection req_log $request

	foreach timer [dict keys $connection timers] {
	    cancel $sock $timer
	}

	Debug.socket {Disconnect: $sock ([chan names sock*]) - '$error' ($request)}
	Debug.close {Disconnecting: '$error' ($eo)}

	;# remove socket
	catch {chan event $sock writable {}}
	catch {chan event $sock readable {}}
	catch {close $sock}

	# inform parent of Disconnect - this thread will now be recycled
	indicate Httpd Disconnect [dict get $request -cid] $error $eo
    }

    # Handle - handle a protocol error
    proc Handle {req} {
	Debug.error {Handle: ([dump $req])}
	set sock [dict get $req -sock]
	readable $sock	;# suspend reading

	upvar #0 ::HttpdWorker::connections($sock) connection
	catch {cancel $sock rxtimer}

	if {[catch {
	    dict set req connection close
	    if {![dict exists $req -transaction]} {
		dict set req -transaction [dict get $connection transaction]
	    }
	    dict set req -generation [dict get $connection generation]
	    Send $req 0			;# send our own reply
	} r eo]} {
	    dict append req -error "(handler '$r' ($eo))"
	    #set req [Access log $request]
	    Debug.error {'Handle' error: '$r' ($eo)}
	}
    }

    # we're finished reading the header - inform the parent that work is needed
    proc got {req} {
	set sock [dict get $req -sock]
	readable $sock	;# suspend reading
	catch {cancel $sock rxtimer}
	Debug.socket {got: $req}

	upvar #0 ::HttpdWorker::connections($sock) connection
	upvar #0 ::HttpdWorker::requests($sock) request
	set request $req

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

	    dict incr connection transaction
	    dict set request -transaction [dict get $connection transaction]

	    #set request [Access log $request]	;# log the request

	    # inform parent of parsing completion
	    indicate Httpd Got $request
	    dict lappend connection req_log $request
	} r eo]} {
	    Debug.error {'get' error: ($eo) '$r' ([dump $req])}
	}

	# reset the request dict to this connection prototype
	set request [dict get $connection prototype]
	readable $sock get $sock	;# resume reading
    }

    # gzip -
    proc gzip {sock} {
	upvar #0 ::HttpdWorker::requests($sock) request
	dict set request -entity [zlib deflate [dict get $request -entity]]
    }

    # read the entity, informing parent when complete
    proc identity {sock length} {
	cancel $sock rxtimer
	Debug.entity {identity from '$sock' ('$length' remaining) ([fconfigure $sock])}
	upvar #0 ::HttpdWorker::requests($sock) request
	if {[catch {
	    # read as much of the entity as is available
	    dict set request -left [expr {$length - [string length [dict get $request -entity]]}]
	    Debug.entity {identity reading from '$sock' ([dict get $request -left])}

	    dict append request -entity [read $sock [dict get $request -left]]
	    Debug.entity {identity read [string length [dict get $request -entity]]/$length from '$sock' ()}

	    if {[string length [dict get $request -entity]] == $length} {
		readable $sock	;# disable reading
		# completed entity - invoke continuation
		Debug.entity {identity read complete - '[Dict get? $request -te]'}
		foreach te [Dict get? $request -te] {
		    if {$te in {chunked deflate compress gzip}} {
			$te $sock
		    }
		}
		got $request
	    } else {
		variable enttime
		after $sock rxtimer $enttime "identity timeout"
	    }
	} r eo]} {
	    Debug.error {identity error '$r' ($eo)}
	    Disconnect $sock chunk; return
	}
    }

    proc chunk {sock} {
	if {[file eof $sock]} {
	    Disconnect $sock chunk; return
	}
    }

    # start_transfer - start transmission of POST entity, if necessary/possible
    proc start_transfer {sock} {
	upvar #0 ::HttpdWorker::requests($sock) request
	if {[dict get $request -version] >= 1.1
	    && [dict exists $request expect]
	    && [string match *100-continue* [string tolower [dict get $request expect]]]
	} {
	    # the client wants us to tell it to continue
	    # before reading the body.
	    # Do so, then proceed to read
	    puts -nonewline $sock "HTTP/1.1 100 Continue\r\n"
	}
    }

    # Start reading an entity from the client.
    # On completion use the supplied completion callback
    proc entity {sock} {
	upvar #0 ::HttpdWorker::requests($sock) request
	if {[dict get $request -method] ni {POST PUT}} {
	    return 1 ;# not a post?  No entity available.
	}

	# rfc2616 4.3
	# The presence of a message-body in a request is signaled by the
	# inclusion of a Content-Length or Transfer-Encoding header field in
	# the request's headers.
	if {[dict exists $request transfer-encoding]} {
	    set te [dict get $request transfer-encoding]
	    # chunked 3.6.1, identity 3.6.2, gzip 3.5, compress 3.5, deflate 3.5
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
		    # 4.4.2 If a message is received with both
		    # a Transfer-EncodIing header field
		    # and a Content-Length header field,
		    # the latter MUST be ignored.
		} else {
		    lappend tels $tel
		    set params($tel) [split $param ";"]
		}
	    }

	    dict set request -te $tels
	    dict set request -te_params [array get params]

	    if {0 && "chunked" in $tels} {
		# not accepting chunks
		start_transfer $sock
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
	variable enttime
	after $sock rxtimer $enttime "entity timeout"
	start_transfer $sock
	dict set request -entity "" ;# clear any old entity

	readable $sock identity $sock $length

	return 0	;# we'll be handling the channel
    }

    # Parse the entire header in {$req -header}
    proc parse {sock} {
	upvar #0 ::HttpdWorker::requests($sock) request
	Debug.socket {parse: $request} 3
	set header [dict get $request -header]
	dict unset request -header	;# delete header

	# parse header body
	set key ""
	foreach line [lrange $header 1 end] {
	    if {[string index $line 0] in {" " "\t"}} {
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
		variable maxfield
		if {$maxfield
		    && [string length [dict get $request $key]] > $maxfield
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
	    variable maxurilen
	    if {$maxurilen
		&& [string length $head(-uri)] > $maxurilen
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
	    Handle [Http Bad $request "Method unsupported '[lindex $header 0]' ([array get head])" 405]
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
	    Handle [Http Bad $request "HTTP 1.1 required to send Host"]
	    return
	} else {
	    # HTTP 1.0 isn't required to send a Host request but we still need it
	    if {![dict exists $request -host]} {
		# make sure the request has some idea of our host&port
		variable host; dict set request -host $host
		variable port; dict set request -port $port
		dict set request host [Url host $request]
	    }
	    set request [dict merge $request [Url parse http://[Url host $request]/$head(-uri)]]
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

	if {[dict get $request -uri] eq "/_error"} {
	    error "Test background error handling"
	}

	# remove 'netscape extension' length= from if-modified-since
	if {[dict exists $request if-modified-since]} {
	    dict set request if-modified-since [lindex [split [dict get $request if-modified-since] {;}] 0]
	}

	# trust x-forwarded-for if we get a forwarded request from a local ip
	# (presumably local ip forwarders are trustworthy)
	if {[dict exists $request x-varnish-for]} {
	    set xff [string trim [dict get $request x-varnish-for]]
	    dict set forwards [lindex [split $xff :] 0]
	} else {
	    set forwards {}
	}
	if {[dict exists $request x-forwarded-for]} {
	    foreach xff [split [Dict get? $request x-forwarded-for] ,] {
		set xff [string trim $xff]
		if {$xff eq ""
		    || $xff eq "unknown"
		    || [Http nonRouting? $xff]
		} continue
		lappend forwards [lindex [split $xff :] 0]
	    }
	}
	dict set request -forwards $forwards
	dict set request -ipaddr [lindex $forwards end]

	# block spiders by UA
	if {[info exists ::spiders([Dict get? $request user-agent])]} {
	    indicate Block block [dict get $request -ipaddr] "spider UA ([Dict get? $request user-agent])"
	    Handle [Http NotImplemented $request "Spider Service"]
	    return
	}

	# analyse the user agent strings.
	dict set request -ua [ua [Dict get? $request user-agent]]

	dict incr connection pending
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
		indicate Block block [dict get $request -ipaddr] "CONNECT method"
		Handle [Http NotImplemented $request "Spider Service"]
		return
	    }

	    default {
		Debug.http {parse done: [dump $request]} 3
		got $request
	    }
	}
    }

    # get lines of header until it's complete
    proc get {sock} {
	cancel $sock rxtimer
	upvar #0 ::HttpdWorker::requests($sock) request

	Debug.socket {get: $request} 10

	if {[catch {
	    chan gets $sock line
	} result eo]} {
	    Disconnect $sock $result $eo	;# inform parent that we're done
	    return
	}

	if {$result == -1} {
	    readable $sock	;# completed reading
	    variable maxline
	    if {[chan eof $sock]} {
		# remote end closed - just forget it
		Disconnect $sock "Remote closed connection"; return
	    } elseif {$maxline && [chan pending input $sock] > $maxline} {
		Handle [Http Bad $request "Line too long"]
		return
	    }

	    variable enttime
	    after $sock rxtimer $enttime "pre-read timeout"
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
	    variable rxtime
	    after $sock rxtimer $rxtime "inter-read timeout"
	    dict lappend request -header $line
	    variable maxhead
	    if {$maxhead
		&& [llength [dict get $request -header]] > $maxhead
	    } {
		Handle [Http Bad $request "Header too Large"]
		return
	    }
	}
    }

    # prototype of a new incoming request
    # transaction count for current connection
    # generation per socket
    variable prototype {
	replies {}
	req_log {}
	pending 0
	transaction -1
	response -1
	generation $generation
    }

    # Parent thread will call connect with the pro-forma request
    proc Connect {sock req} {
	Debug.socket {[id] connect $req $sock}

	# some code to detect races (we hope)
	if {0} { # threads may now have more than 1 channelldap 
	    set chans [chan names sock*]
	    if {[llength $chans] > 1} {
		Debug.error {HRACE [id]: new req from $sock ($chans) - request:[catch {set ::request} xxx; set xxx]}
	    }
	}

	# generate a connection record
	variable gen_count; set generation [incr gen_count]	;# unique generation
	upvar #0 ::HttpdWorker::connections($sock) connection
	variable prototype; set connection [subst $prototype]	;# connection record

	# associate the request with the socket
	upvar #0 ::HttpdWorker::requests($sock) request
	set request [dict merge $req [list -sock $sock -generation $generation]]
	dict set connection prototype $request

	variable txtime
	after $sock rxtimer $txtime "first-read timeout"
	readable $sock get $sock
	Debug.socket {[id] connected}
    }

    proc Disconnected {args} {
	# do something?
	Debug.log {Disconnected indication from parent: $args}
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

# load up per-worker locals.
catch {source [file join [file dirname [info script]] wlocal.tcl]}

Debug on log 10
#Debug on close 10
#Debug on socket 10
#Debug on http 10
Debug off entity 10
# now we're able to process commands
#puts stderr "Started Httpd Worker [id]"
#puts stderr "~Thread: [thread::id]"
