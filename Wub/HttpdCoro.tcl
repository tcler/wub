# HttpdCoro - Httpd Protocol worker with coroutines
#
# This differs from HttpdSingle and HttpdThread, in that
# it integrates directly with the Httpd namespace, using coroutines
# to parse requests and handle responses.

proc bgerror {error eo} {
    Debug.error {Worker Error: $error ($eo)}
}

interp bgerror {} ::bgerror

interp alias {} armour {} string map {& &amp; < &lt; > &gt; \" &quot; ' "&#39;"}

proc HttpdWorker {args} {
    puts stderr "WOE: [info level -1] / [uplevel 1 [list namespace current]]"
}

package require WubUtils
package require spiders
package require Debug
Debug off HttpdCoro 10

package require Url
package require Http
package require Cookies
package require UA

package provide HttpdCoro 1.0

# import the relevant commands
namespace eval ::tcl::unsupported {namespace export coroutine yield infoCoroutine}
namespace import ::tcl::unsupported::coroutine ::tcl::unsupported::yield ::tcl::unsupported::infoCoroutine

namespace eval Httpd {
    variable generation		;# worker/connection association generation

    # limits on header size
    variable maxline 2048	;# max request line length
    variable maxfield 4096	;# max request field length
    variable maxhead 1024	;# maximum number of header lines
    variable maxurilen 1024	;# maximum URI length
    variable maxentity -1	;# maximum entity size

    variable txtime 20000	;# inter-write timeout
    variable rxtime 10000	;# inter-read timeout
    variable enttime 10000	;# entity inter-read timeout

    # timeout - by default none
    variable rxtimeout -1

    # arrange gzip Transfer Encoding
    if {![catch {package require zlib}]} {
	variable ce_encodings {gzip}
    } else {
	variable ce_encodings {}
    }
    #set ce_encodings {}	;# uncomment to stop gzip transfers
    variable te_encodings {chunked}

    variable uniq [pid]	;# seed for unique coroutine names

    # give a uniq looking name
    proc uniq {} {
	variable uniq
	return [incr uniq]
    }

    # closing - we are closing the connection, or recognising that it's closed.
    proc closing {sock} {
	Debug.HttpdCoro "closing: '$sock'"
	if {[catch {chan eof $sock} r] || $r} {
	    # remote end closed - indicate closure
	    $sock EOF "Remote closed connection"
	} elseif {[catch {
	    chan read $sock	;# read and discard input
	} r eo]} {
	    Debug.error "trying to close: '$r' ($eo)"
	    $sock EOF "error closed connection - $r ($eo)"
	}
    }

    proc sockname {} {
	return [namespace tail [infoCoroutine]]
    }

    # indicate EOF and shut down socket and reader
    proc EOF {{reason ""}} {
	set socket [sockname]
	Debug.HttpdCoro "[infoCoroutine] EOF: '$socket' ($reason)"

	# forget whatever higher level connection info
	upvar \#1 cid cid
	catch {forget $cid}

	# socket has closed while reading
	# clean up socket
	catch {close $socket}

	# report EOF to consumer
	upvar \#1 consumer consumer
	if {[info commands $consumer] eq ""} {
	    Debug.HttpdCoro {reader [infoCoroutine]: consumer gone on EOF}
	}
	if {[catch {
	    $consumer [list EOF $reason]
	} e eo]} {
	    Debug.HttpdCoro {reader [infoCoroutine]: consumer error on EOF $e ($eo)}
	}

	Debug.HttpdCoro {reader [infoCoroutine]: informed $consumer of EOF}
	kill $consumer
	kill [infoCoroutine]
	Debug.HttpdCoro {reader [infoCoroutine]: suicide on EOF}
	# destroy reader - that's all she wrote
	#rename $socket {}	;# that's all she wrote
    }

    # readable - make socket readable
    proc readable {args} {
	set socket [sockname]
	Debug.HttpdCoro "readable: '$socket' ($args)"
	if {[catch {
	    chan event $socket readable [list $socket {*}$args]
	} r eo]} {
	    Debug.error "readable: '$r' ($eo)"
	    EOF "ERROR $r $eo"
	}
    }

    # close? - should we close this connection?
    proc close? {r} {
	# don't honour 1.0 keep-alives
	set close [expr {[dict get $r -version] < 1.1}]
	Debug.HttpdCoro {version [dict get $r -version] implies close=$close}

	# handle 'connection: close' indication
	foreach ct [split [Dict get? $r connection] ,] {
	    if {[string tolower [string trim $ct]] eq "close"} {
		Debug.close {Tagging close at connection:close request}
		set close 1
	    }
	}

	if {$close} {
	    # we're not accepting more input
	    # but we defer closing the socket until all pending transmission's complete
	    readable CLOSING
	}

	return $close
    }

    # rdump - return a stripped request for printing
    proc rdump {req} {
	dict set req -content <ELIDED>
	dict set req -entity <ELIDED>
	dict set req -gzip <ELIDED>
	return $req
    }

    # we have been told we can write a reply
    proc write {r cache} {
	upvar \#1 generation generation
	if {[dict exists $r -suspend]} {
	    return 0	;# this reply has been suspended anyway
	}

	set socket [sockname]
	upvar \#1 replies replies response response sequence sequence consumer consumer generation generation satisfied satisfied transaction transaction

	Debug.HttpdCoro {write ([rdump $r]) satisfied: ($satisfied)}

	# fetch transaction from the caller's identity
	if {![dict exists $r -transaction]} {
	    # can't Send reply: no -transaction associated with request
	    Debug.error {Send discarded: no transaction ($r)}
	    return 1
	} elseif {[dict get $r -generation] != $generation} {
	    # this reply belongs to an older, disconnected Httpd generation.
	    # we must discard it, because it was directed to a different client!
	    Debug.error {Send discarded: out of generation '[dict get $r -generation] != $generation' ([rdump $r])}
	    return 1
	}
	set trx [dict get $r -transaction]

	# discard duplicate responses
	if {[dict exists $satisfied $trx]} {
	    # a duplicate response has been sent - discard this
	    # this could happen if a dispatcher sends a response,
	    # then gets an error.
	    Debug.error {Send discarded: duplicate ([rdump $r])}
	    return 0
	}

	variable ce_encodings	;# what encodings do we support?

	# wire-format the reply transaction - messy
	lassign [Http Send $r -cache $cache -encoding $ce_encodings] r header content empty cache
	set header "HTTP/1.1 $header" ;# add the HTTP signifier

	# record transaction reply and kick off the responder
	# response has been collected and is pending output
	# queue up response for transmission
	
	Debug.HttpdCoro {ADD TRANS: $header ([dict keys $replies])}

	# global consequences - botting and caching
	if {![Honeypot newbot? $r] && $cache} {
	    # handle caching (under no circumstances cache bot replies)
	    Cache put $r	;# cache it before it's sent
	}

	# queue new response for sending
	# response is broken down into
	# header - formatted to go down the line in crlf mode
	# content - content to go down the line in binary mode
	# close? - is the connection to be closed after this response?
	# chunked? - is the content to be sent in chunked mode?
	# empty? - is there actually no content, as distinct from 0-length content?
	dict set replies $trx [list $header $content [close? $r] $empty]

	Debug.HttpdCoro {pending to send: [dict keys $replies]}
	foreach next [lsort -integer [dict keys $replies]] {
	    if {[eof $socket]} {
		Debug.HttpdCoro {Lost connection on transmit}
		return TX
	    }

	    # ensure we don't send responses out of sequence
	    if {$next != $response} {
		# something's blocking the response pipeline
		# we don't have a response for the next transaction.

		# we have to wait until all the preceding transactions
		# have something to send
		Debug.HttpdCoro {no pending or $next doesn't follow $response}
		break
	    } else {
		# we're going to respond to the next transaction in trx order
		# unpack and consume the reply from replies queue
		lassign [dict get $replies $next] head content close empty

		# remove this response from the pending response structure
		set response [expr {1 + $next}]		;# move to next response
		dict unset replies $next	;# consume next reply

		# connection close required?
		# we only consider closing if all pending requests
		# have been satisfied.
		if {$close} {
		    Debug.close {close requested on $socket - sending header}
		    append head "Connection: close" \r\n	;# send a close just in case
		    # Once this header's been sent, we're committed to closing
		}

		# send the header
		puts -nonewline $socket "$head\r\n"	;# send headers with terminating nl
		Debug.socket {SENT: $socket $head'} 4
		
		# send the content/file (if any)
		# note: we must *not* send a trailing newline, as this
		# screws up the content-length and confuses the client
		# which doesn't then pick up the next response
		# in the pipeline
		if {!$empty} {
		    puts -nonewline $socket $content	;# send the content
		    Debug.socket {SENT BYTES: [string length $content] bytes} 8
		}
		dict set satisfied $trx {}

		if {$close} {
		    chan flush $socket	;# no, really, send it all
		    close $socket	;# now close it and let the fileevent announce
		    return CLOSED
		}
	    }
	}
	chan flush $socket	;# no, really, send it all
	return ""
    }

    # send --
    #	deliver in-sequence transaction responses
    #
    # Arguments:
    #
    # Side Effects:
    #	Send transaction responses to client
    #	Possibly close socket
    proc send {r {cache 1}} {
	Debug.HttpdCoro {send: ([rdump $r]) $cache}

	if {[catch {
	    # send all pending responses, ensuring we don't send out of sequence
	    write $r $cache
	} close eo]} {
	    if {[dict get $eo -errorcode] eq {POSIX EPIPE {broken pipe}}} {
		Debug.error {premature disconnect send '$close' ($eo)}
	    } else {
		Debug.error {FAILED send '$close' ($eo)}
	    }
	    return ERROR
	} else {
	    Debug.socket {SENT (close: $close)'} 10
	    return $close
	}
    }

    # yield wrapper with command dispatcher
    proc yield {{retval ""}} {
	set socket [sockname]
	upvar \#1 timer timer timeout timeout cmd cmd consumer consumer

	set time $timeout
	while {1} {
	    Debug.HttpdCoro {coro [infoCoroutine] yielding}
	    if {$time > 0} {
		lappend timer [after $time $socket [list $socket TIMEOUT]]	;# set new timer
		Debug.HttpdCoro {timer '[infoCoroutine]' $timer}
	    }

	    # wait for an event
	    set args [lassign [::yield $retval] op]
	    Debug.HttpdCoro {yield '[infoCoroutine]' ($retval) -> $op ($args)}

	    # cancel all outstanding timers for this coro
	    foreach t $timer {
		catch {
		    after cancel $t	;# cancel old timer
		} e eo
		Debug.HttpdCoro {cancel '[infoCoroutine]' $t - $e ($eo)}
	    }
	    set timer {}

	    # dispatch on command
	    switch -- [string toupper $op] {
		READ {
		    # this can only happen in the reader coro
		    return $args
		}

		SEND {
		    # send a response to client
		    set close [send {*}$args]
		    if {$close ne ""} {
			EOF $close
		    }
		}

		EOF {
		    # we've been informed that we're closed
		    EOF {*}$args
		}

		CLOSING {
		    # we're kind of half-closed
		    # we won't accept any more input but we want to send
		    # all pending responses
		    
		    if {[catch {chan eof $socket} res] || $res} {
			# remote end closed - just forget it
		    } elseif {[catch {chan read $socket} r eo]} {
			Debug.error "trying to close: '$r' ($eo)"
			EOF "error closed connection - $r ($eo)"
		    }
		}
		
		TIMEOUT {
		    # we've timed out - oops
		    if {[catch {
			$consumer [list TIMEOUT [infoCoroutine]]
		    }] && [info commands $consumer] eq ""} {
			Debug.HttpdCoro {reader [infoCoroutine]: consumer error or gone on EOF}
			return -code return
		    }
		    set time -1
		}
	    }
	}
    }

    # handle - handle a protocol error
    proc handle {req} {
	Debug.error {handle: ([rdump $req])}
	set socket [sockname]
	
	# we have an error, so we're going to try to reply then die.
	readable closing	;# suspend reading - junk whatever's read
	# must keep socket readable to detect close, but will chuck anything read.

	upvar \#1 transaction transaction generation generation
	if {[catch {
	    dict set req connection close	;# we want to close this connection
	    if {![dict exists $req -transaction]} {
		dict set req -transaction [incr transaction]
	    }
	    dict set req -generation $generation

	    # send a response to client
	    send $req	;# queue up error response
	} r eo]} {
	    dict append req -error "(handler '$r' ($eo))"
	    Debug.error {'handle' error: '$r' ($eo)}
	}
	while {1} yield	;# just process events until we're killed
    }

    proc get {socket {reason ""}} {
	set result [yield]
	variable maxline
	while {[chan gets $socket line] == -1 && ![chan eof $socket]} {
	    set result [yield]
	    if {$maxline && [chan pending input $socket] > $maxline} {
		handle [Http Bad $request "Line too long"]
		return -code return
	    }
	}
	
	if {![chan eof $socket]} {
	    Debug.HttpdCoro {get: '$line' [chan blocked $socket] [chan eof $socket]}
	    return $line
	}

	EOF $reason
    }

    proc read {socket size} {
	# read a chunk of $size bytes
	set chunk ""
	while {$size && ![chan eof $socket]} {
	    set result [yield]
	    set chunklet [chan read $socket $size]
	    incr size -[string length $chunklet]
	    append chunk $chunklet
	}

	# return the chunk
	if {![chan eof $socket]} {
	    Debug.HttpdCoro {read: '$chunk'}
	    return $chunk
	}

	EOF ENTITY
    }

    proc parse {lines} {
	# we have a complete header - parse it.
	set r {}
	set last ""
	set size 0
	foreach line $lines {
	    if {[string index $line 0] in {" " "\t"}} {
		# continuation line
		dict append r $last " [string trim $line]"
	    } else {
		set value [join [lassign [split $line ":"] key] ":"]
		set key [string tolower [string trim $key "- \t"]]
		
		if {[dict exists $r $key]} {
		    dict append r $key ",$value"
		} else {
		    dict set r $key [string trim $value]
		}

		# limit size of each field
		variable maxfield
		if {$maxfield
		    && [string length [dict get $r $key]] > $maxfield
		} {
		    handle [Http Bad $request "Illegal header: '$line'"]
		}
	    }
	}
	return $r
    }

    variable reader {
	Debug.HttpdCoro {create reader [infoCoroutine] - $args}

	# unpack all the passed-in args
	set replies {}	;# dict of replies pending
	set satisfied {};# dict of requests satisfied
	set response 1	;# which is the next response to send?
	set sequence -1	;# which is the next response to queue?
	set writing 0	;# we're not writing yet
	dict with args {}
	set timer {}	;# collection of active timers
	set transaction 0	;# count of incoming requests

	# keep receiving input requests
	while {1} {
	    # get whole header
	    set headering 1
	    set lines {}
	    while {$headering} {
		set line [get $socket HEADER]
		Debug.HttpdCoro {reader [infoCoroutine] got line: ($line)}
		if {[string trim $line] eq ""} {
		    # rfc2616 4.1: In the interest of robustness,
		    # servers SHOULD ignore any empty line(s)
		    # received where a Request-Line is expected.
		    if {[llength $lines]} {
			set headering 0
		    }
		} else {
		    lappend lines $line
		}
	    }

	    # parse the header into a request
	    set r [dict merge $prototype [parse [lrange $lines 1 end]]]	;# parse the header
	    dict set r -received [clock microseconds]
	    dict set r -transaction [incr transaction]
	    dict set r -sock $socket

	    # unpack the header line
	    set header [lindex $lines 0]
	    dict set r -method [string toupper [lindex $header 0]]
	    switch -- [dict get $r -method] {
		CONNECT {
		    # stop the bastard SMTP spammers
		    Block block [dict get $r -ipaddr] "CONNECT method"
		    handle [Http NotImplemented $r "Spider Service"]
		}

		GET - PUT - POST - HEAD {}

		default {
		    # Could check for and service FTP requestuests, etc, here...
		    dict set r -error_line $line
		    handle [Http Bad $r "Method unsupported '[lindex $header 0]'" 405]
		}
	    }

	    dict set r -version [lindex $header end]
	    dict set r -uri [join [lrange $header 1 end-1]]

	    # check URI length (per rfc2616 3.2.1
	    # A server SHOULD return 414 (Requestuest-URI Too Long) status
	    # if a URI is longer than the server can handle (see section 10.4.15).)
	    variable maxurilen
	    if {$maxurilen && [string length [dict get $r -uri]] > $maxurilen} {
		# send a 414 back
		handle [Http Bad $r "URI too long '[dict get $r -uri]'" 414]
	    }

	    if {[string match HTTP/* [dict get $r -version]]} {
		dict set r -version [lindex [split [dict get $r -version] /] 1]
	    }

	    # Send 505 for protocol != HTTP/1.0 or HTTP/1.1
	    if {([dict get $r -version] != 1.1)
		&& ([dict get $r -version] != 1.0)} {
		handle [Http Bad $r "HTTP Version '[dict get $r -version]' not supported" 505]
	    }

	    Debug.HttpdCoro {reader got request: ($r)}

	    # parse the URL
	    set r [dict merge $r [Url parse [dict get $r -uri]]]

	    # block spiders by UA
	    if {[info exists ::spiders([Dict get? $r user-agent])]} {
		Block block [dict get $r -ipaddr] "spider UA ([Dict get? $r user-agent])"
		handle [Http NotImplemented $r "Spider Service"]
	    }

	    # analyse the user agent strings.
	    dict set r -ua [ua [Dict get? $r user-agent]]

	    # check the incoming ip for blockage
	    if {[Block blocked? [Dict get? $r -ipaddr]]} {
		handle [Http Forbidden $r]
	    } elseif {[Honeypot guard r]} {
		# check the incoming ip for bot detection
		# this is a bot - reply directly to it
		send $req	;# queue up error response
		continue
	    }

	    # ensure that the client sent a Host: if protocol requires it
	    if {[dict exists $r host]} {
		# client sent Host: field
		if {[string match http:* [dict get $r -uri]]} {
		    # rfc 5.2 1 - a host header field must be ignored
		    # if request-line specified an absolute URL host/port
		    set r [dict merge $r [Url parse [dict get $r -uri]]]
		    dict set r host [Url host $r]
		} else {
		    # no absolute URL was specified by the request-line
		    # use the Host field to determine the host
		    foreach c [split [dict get $r host] :] f {host port} {
			dict set r -$f $c
		    }
		    dict set r host [Url host $r]
		    set r [dict merge $r [Url parse http://[dict get $r host][dict get $r -uri]]]
		}
	    } elseif {[dict get $r -version] > 1.0} {
		handle [Http Bad $r "HTTP 1.1 required to send Host"]
	    } else {
		# HTTP 1.0 isn't required to send a Host request but we still need it
		if {![dict exists $r -host]} {
		    # make sure the request has some idea of our host&port
		    dict set r -host $host
		    dict set r -port $port
		    dict set r host [Url host $r]
		}
		set r [dict merge $r [Url parse http://[Url host $r]/[dict get $r -uri]]]
	    }
	    dict set r -url [Url url $r]	;# normalize URL

	    # rfc2616 14.10:
	    # A system receiving an HTTP/1.0 (or lower-version) message that
	    # includes a Connection header MUST, for each connection-token in this
	    # field, remove and ignore any header field(s) from the message with
	    # the same name as the connection-token.
	    if {[dict get $r -version] < 1.1 && [dict exists $r connection]} {
		foreach token [split [dict get $r connection] ","] {
		    catch {dict unset r [string trim $token]}
		}
		dict unset r connection
	    }

	    # completed request header decode - now dispatch on the URL
	    Debug.HttpdCoro {reader complete: $header ([rdump $r])}

	    # rename fields whose names are the same in request/response
	    foreach n {cache-control} {
		if {[dict exists $r $n]} {
		    dict set r -$n [dict get $r $n]
		    dict unset r $n
		}
	    }

	    # rfc2616 4.3
	    # The presence of a message-body in a request is signaled by the
	    # inclusion of a Content-Length or Transfer-Encoding header field in
	    # the request's headers.
	    if {[dict exists $r transfer-encoding]} {
		set te [dict get $r transfer-encoding]
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
			handle [Http NotImplemented $r "$tel transfer encoding"]
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

		dict set r -te $tels
		dict set r -te_params [array get params]
	    } elseif {[dict get $r -method] in {POST PUT}
		      && ![dict exists $r content-length]} {
		dict set r -te {}
		
		# this is a content-length driven entity transfer
		# 411 Length Required
		handle [Http Bad $r "Length Required" 411]
	    }

	    if {[dict get $r -version] >= 1.1
		&& [dict exists $r expect]
		&& [string match *100-continue* [string tolower [dict get $r expect]]]
	    } {
		# the client wants us to tell it to continue
		# before reading the body.
		# Do so, then proceed to read
		puts -nonewline $socket "HTTP/1.1 100 Continue\r\n"
	    }

	    # fetch the entity (if any)
	    if {"chunked" in [Dict get? $r -te]} {
		set chunksize 1
		while {$chunksize} {
		    chan configure $socket -translation {crlf binary}
		    set chunksize 0x[get $socket CHUNK]
		    chan configure $socket -translation {binary binary}
		    if {$chunksize eq "0x"} {
			Debug.HttpdCoro {Chunks all done}
			break	;# collected all the chunks
		    }
		    set chunk [read $socket $chunksize]
		    Debug.HttpdCoro {Chunk: $chunksize ($chunk)}
		    get $socket CHUNK
		    dict append r -entity $chunk
		    
		    # enforce server limits on Entity length
		    variable maxentity
		    if {$maxentity > 0
			&& [string length [dict get $r -entity]] > $maxentity} {
			# 413 "Request Entity Too Large"
			handle [Http Bad $r "Request Entity Too Large" 413]
		    }
		}
	    } elseif {[dict exists $r content-length]} {
		set left [dict get $r content-length]

		# enforce server limits on Entity length
		variable maxentity
		if {$maxentity > 0 && $left > $maxentity} {
		    # 413 "Request Entity Too Large"
		    handle [Http Bad $r "Request Entity Too Large" 413]
		}

		if {$left == 0} {
		    dict set r -entity ""
		    # the entity, length 0, is therefore already read
		    # 14.13: Any Content-Length greater than
		    # or equal to zero is a valid value.
		} else {
		    set entity ""
		    chan configure $socket -translation {binary binary}
		    Debug.HttpdCoro {reader getting entity of length ($left)}
		    while {$left > 0} {
			set chunk [read $socket $left]
			incr left -[string length $chunk]
			Debug.HttpdCoro {reader getting remainder of entity of length ($left)}
			dict append r -entity $chunk
			Debug.HttpdCoro {reader got whole entity}
		    }
		}
	    }

	    # reset socket to header config, having read the entity
	    chan configure $socket -encoding binary -translation {crlf binary}

	    # now we postprocess/decode the entity
	    Debug.entity {entity read complete - '[Dict get? $r -te]'}
	    if {"gzip" in [Dict get? $r -te]} {
		dict set r -entity [zlib deflate [dict get $r -entity]]
	    }

	    # remove 'netscape extension' length= from if-modified-since
	    if {[dict exists $r if-modified-since]} {
		dict set r if-modified-since [lindex [split [dict get $r if-modified-since] {;}] 0]
	    }

	    # trust x-forwarded-for if we get a forwarded request from a local ip
	    # (presumably local ip forwarders are trustworthy)
	    set forwards {}
	    if {[dict exists $r x-forwarded-for]} {
		foreach xff [split [Dict get? $r x-forwarded-for] ,] {
		    set xff [string trim $xff]
		    set xff [lindex [split $xff :] 0]
		    if {$xff eq ""
			|| $xff eq "unknown"
			|| [Http nonRouting? $xff]
		    } continue
		    lappend forwards $xff
		}
	    }
	    lappend forwards [dict get $r -ipaddr]
	    dict set r -forwards $forwards
	    dict set r -ipaddr [lindex $forwards 0]

	    # check Cache for match
	    if {[dict size [set cached [Cache check $r]]] > 0} {
		# reply from cache
		dict set cached -transaction [dict get $r -transaction]
		dict set cached -generation [dict get $r -generation]

		Debug.HttpdCoro {sending cached ([rdump $cached])}
		send $cached 0
		continue
	    }

	    # deliver request to consumer
	    if {[info commands $consumer] ne {}} {
		# deliver the assembled request to the consumer
		after 1 $consumer [list [list INCOMING $r]]
		Debug.HttpdCoro {reader [infoCoroutine]: sent to consumer, waiting for next}
	    } else {
		# the consumer has gone away
		Debug.HttpdCoro {reader [infoCoroutine]: consumer gone $consumer}
		EOF CONSUMER	;# terminate this coro
	    }
	}
    }

    proc disconnect {args} {}

    # the consumer has gone - inform the reader
    proc dead_consumer {reader} {
	Debug.HttpdCoro {dead_consumer $reader}
	catch {$reader EOF consumer}	;# inform the reader
	catch {rename $reader {}}	;# kill the reader
    }

    # the socket has gone - inform the consumer
    proc dead_socket {consumer} {
	Debug.HttpdCoro {dead_socket $consumer}
	catch {$consumer [list EOF socket]}	;# inform the consumer
	catch {rename $consumer {}}	;# kill the consumer
    }

    # the coro must go
    proc kill {coro} {
	Debug.HttpdCoro {kill $coro}
	if {[info commands $coro] eq {}} {
	    return
	}
	foreach tr [trace info command $coro] {
	    lassign $tr ops prefix
	    trace remove command $coro $ops $prefix 
	}
	catch {rename $coro {}}
    }

    # the request consumer
    variable consumer {
	Debug.HttpdCoro {consumer: $args}
	dict with args {}
	while {1} {
	    set args [lassign [::yield] op]
	    Debug.HttpdCoro {consumer [infoCoroutine] got: $op $args}
	    switch -- $op {
		TIMEOUT -
		EOF {
		    kill $reader	;# kill reader
		    kill [infoCoroutine];# kill self
		}

		INCOMING {
		    set r [lindex $args 0]
		    catch {Responder do $r} rsp eo
		    switch [dict get $eo -code] {
			0 -
			2 { # ok - return
			    if {![dict exists $rsp -code]} {
				set rsp [Http Ok $rsp]
			    }
			}
			
			1 { # error
			    set rsp [Http ServerError $r $rsp $eo]
			}
		    }

		    if {[catch {
			Responder post $rsp
		    } e eo]} {
			Debug.error {POST ERROR: $e ($eo)} 1
			set rsp [Http ServerError $r $e $eo]
		    } else {
			Debug.HttpdCoro {POST: [rdump $e]} 10
			set rsp $e
		    }

		    if {[catch {
			$reader [list SEND $rsp]
		    } e ro]} {
			Debug.error {sending error: $e ($eo)} 1
		    }
		}
	    }
	}
    }

    # we have received a new connection
    # set up a consumer and a reader for it
    proc associate {request} {
	set socket [dict get $request -sock]
	set cid [dict get $request -cid]	;# remember the connection id
	Debug.socket {associate $request $socket}

	# condition the socket
	chan configure $socket -buffering none -translation {crlf binary}

	# generate a connection record prototype
	variable generation	;# unique generation
	set gen [incr generation]
	set request [dict merge $request [list -sock $socket -generation $gen]]

	# create reader coroutine
	variable reader
	set R ::Httpd::${socket}
	if {[info commands $R] ne {}} {
	    # the old socket stuff hasn't yet been cleaned up.
	    # this is potentially very bad.
	}

	# construct consumer
	set cr ::Httpd::C[uniq]
	variable consumer; coroutine $cr ::apply [list args $consumer ::Httpd] reader $R

	# construct the reader
	variable rxtimeout
	set result [coroutine $R ::apply [list args $reader ::Httpd] socket $socket timeout $rxtimeout consumer $cr prototype $request generation $gen]

	# ensure there's a cleaner for this connection's coros
	trace add command $cr delete [namespace code [list dead_consumer $R]]
	trace add command $R delete [namespace code [list dead_socket $cr]]

	# start the ball rolling
	chan event $socket readable [list $R READ]

	return $result
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

# load up per-worker locals.
catch {source [file join [file dirname [info script]] wlocal.tcl]}

Debug on log 10
Debug off close 10
Debug off socket 10
Debug off http 10
Debug off entity 10
# now we're able to process commands
