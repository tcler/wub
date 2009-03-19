# Httpd - near HTTP/1.1 protocol server.
#

if {[info exists argv0] && ($argv0 eq [info script])} {
    # test Httpd
    lappend auto_path [pwd] ../Utilities/ ../extensions/
    package require Http
}

package require Debug
Debug off Httpd 10
Debug off HttpdLow 10
Debug off Watchdog 10

package require Listener
package require Query
package require Block

package require Cache 2.0
package require Honeypot
package require Html

package require Url
package require Http
package require UA

package provide Httpd 4.0

proc bgerror {args} {
    Debug.error {bgerror: $args}
}
interp bgerror {} bgerror

# define a default [pest] predicate, allow it to be overriden by pest.tcl
proc pest {req} {return 0}
catch {source [file join [file dirname [info script]] pest.tcl]}

namespace eval Httpd {
    variable server_port ;# server's port (if different from Listener's)
    variable server_id "Wub/[package provide Httpd]"
    variable cid 0		;# unique connection ID

    # exhaustion control
    variable max_conn 20	;# max connections per IP
    variable connbyIP		;# count of connections
    array set connbyIP {}
    variable too_many		;# how many times has this IP address been told?
    array set too_many {}
    variable no_really 30	;# after this many max_conns - it's blocked
    variable retry_wait 20

    # ensure that client is not spamming us with too many connections
    # (sadly we can't do this if we're reverse-proxied)
    proc countConnections {sock ipaddr} {
	variable connbyIP
	variable max_conn
	# normal external connection
	if {[incr connbyIP($ipaddr)] > $max_conn} {
	    # Too many connections for $ipaddr - no more than $max_conn
	    variable too_many
	    variable no_really
	    if {[incr too_many($ipaddr)] > $no_really} {
		# this client has been told repeatedly - block it.
		#Block block $ipaddr "Repeatedly too many connections"
	    } else {
		Debug.log {Too many connections for $ipaddr on $sock ($connbyIP($ipaddr))}
	    }
	    return 0
	    # use the default retry wait advisory period
	    variable retry_wait
	    set retry $retry_wait

	    Debug.socket {Exhausted $sock $eo $retry}

	    variable server_id
	    puts $sock "HTTP/1.1 503 Exhaustion\r"
	    puts $sock "Date: [Http Now]\r"
	    puts $sock "Server: $server_id\r"
	    puts $sock "Connection: Close\r"
	    puts $sock "Retry-After: $retry\r"
	    puts $sock "Content-Length: 0\r"
	    puts $sock \r
	    flush $sock
	    close $sock
	    
	    incr connbyIP($ipaddr) -1
	    return 1
	} else {
	    return 0
	}
    }

    variable generation		;# worker/connection association generation

    # limits on header size
    variable maxline 2048	;# max request line length
    variable maxfield 4096	;# max request field length
    variable maxhead 1024	;# maximum number of header lines
    variable maxurilen 1024	;# maximum URI length
    variable maxentity -1	;# maximum entity size

    # timeout - by default none
    variable timeout 60000

    # activity log - array used for centralised timeout
    variable activity

    variable ce_encodings {gzip}
    variable te_encodings {chunked}

    variable uniq [pid]	;# seed for unique coroutine names

    # give a uniq looking name
    proc uniq {} {
	variable uniq
	return [incr uniq]
    }

    # rdump - return a stripped request for printing
    proc rdump {req} {
	dict set req -content <ELIDED>
	dict set req -entity <ELIDED>
	dict set req -gzip <ELIDED>
	return $req
    }

    # wrapper for chan ops - alert on errors
    proc chan {args} {
	set code [catch {uplevel 1 [list ::chan {*}$args]} e eo]
	if {$code} {
	    if {[info coroutine] ne ""} {
		Debug.Httpd {[info coroutine]: chan error $code - $e ($eo)}
		if {[lindex $args 0] ne "close"} {
		    terminate $e	;# clean up and close unless we're already closing
		}
	    } else {
		Debug.error {chan error $code - $e ($eo)}
	    }
	} else {
	    return $e
	}
    }

    # shut down socket and reader
    proc terminate {{reason ""}} {
	corovars consumer
	if {![info exists consumer]} {
	    # this is the consumer coro
	    # the consumer is trying to terminate directly
	    # so ask socket coro to terminate too
	    corovars reader
	    if {[catch {
		$reader [list TERMINATE $reason]
	    } e eo]} {
		Debug.error {[info coroutine] direct termination via $reader: $e ($eo)} 1
	    }
	    rename [info coroutine] ""; ::yield	;# terminate consumer
	}

	# this is the reader - trying to terminate
	Debug.Httpd {[info coroutine] terminate: ($reason)}

	# disable inactivity reaper for this coro
	variable activity
	catch {unset activity([info coroutine])}

	# don't fear the reaper
	variable reaper
	catch {
	    after cancel $reaper([info coroutine])
	    unset reaper([info coroutine])
	}

	# forget whatever higher level connection info
	corovars cid socket ipaddr

	# terminate consumer if it's still alive
	if {[info commands $consumer] ne {}} {
	    catch {
		set cn ${consumer}_DEAD_[uniq]
		rename $consumer $cn
		after 1 [list catch [list $cn TERMINATE]]
	    }
	}

	# clean up on disconnect
	variable connbyIP; catch {incr connbyIP($ipaddr) -1}

	# clean up socket - the only point where we close
	catch {chan event $socket readable ""}	;# is this necessary?
	catch {chan event $socket writable ""}	;# is this necessary?
	catch {chan close $socket}

	# destroy reader - that's all she wrote
	Debug.Httpd {reader [info coroutine]: terminated}
	rename [info coroutine] ""; ::yield	;# terminate coro
    }

    # close? - should we close this connection?
    proc close? {r} {
	# don't honour 1.0 keep-alives - why?
	set close [expr {[dict get $r -version] < 1.1}]
	Debug.HttpdLow {version [dict get $r -version] implies close=$close}

	# handle 'connection: close' request from client
	foreach ct [split [dict get? $r connection] ,] {
	    if {[string tolower [string trim $ct]] eq "close"} {
		Debug.HttpdLow {Tagging close at connection:close request}
		set close 1
		break	;# don't need to keep going
	    }
	}

	if {$close} {
	    # we're not accepting more input but defer actually closing the socket
	    # until all pending transmission's complete
	    corovars status closing socket
	    set closing 1	;# flag the closure
	    lappend status CLOSING
	    chan event $socket readable [list [info coroutine] CLOSING]
	}

	return $close
    }

    # arrange gzip Transfer Encoding
    variable chunk_size 4196	;# tiny little chunk size
    variable gzip_bugged {}	;# these browsers can't take gzip

    # gzip_content - gzip-encode the content
    proc gzip_content {reply} {
	if {[dict exists $reply -gzip]} {
	    return $reply	;# it's already been gzipped
	}

	# prepend a minimal gzip file header:
	# signature, deflate compression, no flags, mtime,
	# xfl=0, os=3
	set content [dict get $reply -content]
	set gztype [expr {[string match text/* [dict get $reply content-type]]?"text":"binary"}]
	set gzip [zlib gzip $content -header [list crc 0 time [clock seconds] type $gztype]]

	dict set reply -gzip $gzip
	return $reply
    }

    # CE - find and effect appropriate content encoding
    proc CE {reply args} {
	# default to identity encoding
	set content [dict get $reply -content]
	Debug.http {CE -encoding:[dict get? $args -encoding]}
	if {![dict exists $reply -gzip]
	    && ("gzip" in [dict get? $args -encoding])
	    && ![string match image/* [dict get? $reply content-type]] 
	} {
	    set reply [gzip_content $reply]
	}

	# choose content encoding - but not for MSIE
	variable chunk_size
	variable gzip_bugged
	if {[dict get? $reply -ua id] ni $gzip_bugged
	    && [dict exists $reply accept-encoding]
	    && ![dict exists $reply content-encoding]
	} {
	    foreach en [split [dict get $reply accept-encoding] ","] {
		lassign [split $en ";"] en pref
		set en [string trim $en]
		if {$en in [dict get? $args -encoding]} {
		    switch $en {
			"gzip" { # substitute the gzipped form
			    if {[dict exists $reply -gzip]} {
				set content [dict get $reply -gzip]
				dict set reply content-encoding gzip
				#set reply [Http Vary $reply Accept-Encoding User-Agent]
				if {[dict get $reply -version] > 1.0} {
				    # this is probably redundant, since 1.0
				    # doesn't define accept-encoding (does it?)
				    #dict set reply -chunked $chunk_size
				    #dict set reply transfer-encoding chunked
				}
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
	if {[dict exists $reply -chconverted]} {
	    return $reply	;# don't re-encode by charset
	}

	# handle charset for text/* types
	lassign [split [dict get? $reply content-type] {;}] ct
	if {[string match text/* $ct] || [string match */*xml $ct]} {
	    if {[dict exists $reply -charset]} {
		set charset [dict get $reply -charset]
	    } else {
		set charset utf-8	;# default charset
	    }
	    dict set reply -charset $charset
	    dict set reply -chconverted $charset
	    dict set reply content-type "$ct; charset=$charset"
	    dict set reply -content [encoding convertto $charset [dict get $reply -content]]
	}
	return $reply
    }

    variable etag_id [clock microseconds]

    # format4send - format up a reply for sending.
    proc format4send {reply args} {
	set sock [dict get $reply -sock]
	set cache [expr {[dict get? $args -cache] eq "1"}]
	if {[catch {
	    # unpack and consume the reply from replies queue
	    if {![dict exists $reply -code]} {
		set code 200	;# presume it's ok
	    } else {
		set code [dict get $reply -code]
	    }

	    if {$code < 4} {
		# this was a tcl error code, not an HTTP code
		set code 500
	    }

	    # set the informational error message
	    if {[dict exists $reply -error]} {
		set errmsg [dict get $reply -error]
	    }
	    if {![info exists errmsg] || ($errmsg eq "")} {
		set errmsg [Http ErrorMsg $code]
	    }

	    set header "$code $errmsg\r\n"	;# note - needs prefix

	    # format up the headers
	    if {$code != 100} {
		append header "Date: [Http Now]" \r\n
		set si [dict get? $reply -server_id]
		if {$si eq ""} {
		    set si "The Wub"
		}
		append header "Server: $si" \r\n
	    }

	    # format up and send each cookie
	    if {[dict exists $reply -cookies]} {
		Debug.cookies {Http processing: [dict get $reply -cookies]}
		set c [dict get $reply -cookies]
		foreach cookie [Cookies format4server $c] {
		    Debug.cookies {Http set: '$cookie'}
		    append header "set-cookie: $cookie\r\n"
		}
	    } else {
		Debug.cookies {Http processing: no cookies}
	    }

	    # there is content data
	    switch -glob -- $code {
		204 - 304 - 1* {
		    # 1xx (informational),
		    # 204 (no content),
		    # and 304 (not modified)
		    # responses MUST NOT include a message-body
		    Debug.HttpdLow {format4write: code is $code, content is [llength $content] bytes}
		    set reply [Http expunge $reply]
		    set content ""
		    set cache 0	;# can't cache these
		    set empty 1
		}

		default {
		    set empty 0
		    if {[dict exists $reply -content]} {
			# correctly charset-encode content
			set reply [charset $reply]

			#Debug.HttpdLow {pre-CE content length [string length [dict get $reply -content]]}
			# also gzip content so cache can store that.
			lassign [CE $reply {*}$args] reply content

			# ensure content-length is correct
			dict set reply content-length [string length $content]
			#Debug.HttpdLow {post-CE content length [string length $content]}
		    } else {
			Debug.HttpdLow {format4write: response empty - no content in reply}
			set content ""	;# there is no content
			set empty 1	;# it's empty
			dict set reply content-length 0
			set cache 0	;# can't cache no content
		    }
		}
	    }

	    # handle Vary field and -vary dict
	    dict set reply -vary Accept-Encoding 1
	    if {[dict exists $reply -vary]} {
		if {[dict exists $reply -vary *]} {
		    dict set reply vary *
		} else {
		    dict set reply vary [join [dict keys [dict get $reply -vary]] ,]
		}
		dict unset reply -vary
	    }

	    # now attend to caching generated content.
	    if {$empty || [dict get $reply content-length] == 0} {
		set cache 0	;# don't cache no content
	    } elseif {$cache} {
		# use -dynamic flag to avoid caching even if it was requested
		set cache [expr {
				 ![dict exists $reply -dynamic]
				 || ![dict get $reply -dynamic]
			     }]

		if {$cache && [dict exists $reply cache-control]} {
		    set cacheable [split [dict get $reply cache-control] ,]
		    foreach directive $cacheable {
			set body [string trim [join [lassign [split $directive =] d] =]]
			set d [string trim $d]
			if {$d in {no-cache private}} {
			    set cache 0
			    break
			}
		    }
		}

		if {$cache && ![dict exists $reply etag]} {
		    # generate an etag for cacheable responses
		    variable etag_id
		    dict set reply etag "\"H[incr etag_id]\""
		}
	    }

	    # add in Auth header elements - TODO
	    foreach challenge [dict get? $reply -auth] {
		append header "WWW-Authenticate: $challenge" \r\n
	    }

	    if {[dict get $reply -method] eq "HEAD"} {
		# All responses to the HEAD request method MUST NOT
		# include a message-body but may contain all the content
		# header fields.
		set empty 1
		set content ""
	    }

	    #if {!$cache} {
	    # dynamic stuff - no caching!
	    #set reply [Http NoCache $reply]
	    #}

	    #Debug.log {Sending: [dump $reply]}

	    if {$code >= 500} {
		# Errors are completely dynamic - no caching!
		set cache 0
	    }

	    # strip http fields which don't have relevance in response
	    set dh {}
	    dict for {n v} $reply {
		set nl [string tolower $n]
		if {$nl ni {server date}
		    && [info exists ::Http::headers($nl)]
		    && $::Http::headers($nl) ne "rq"
		} {
		    append header "$n: $v" \r\n
		    lappend dh $n $v	;# keep dict of what we're actually sending
		}
	    }
	} r eo]} {
	    if {![info exists code] || $code >= 500} {
		# Errors are completely dynamic - no caching!
		set cache 0
	    }

	    Debug.error {Sending Error: '$r' ($eo) Sending Error}
	} else {
	    Debug.HttpdLow {format4server: ($dh)}
	}

	return [list $reply $header $content $empty $cache]
    }

    # respond to client with as many consecutive responses as he can consume
    proc respond {} {
	corovars replies response sequence generation satisfied transaction closing unsatisfied socket
	if {[string match DEAD* [info coroutine]]} {
	    Debug.Httpd {[info coroutine] appears to be dead}
	    terminate "oops - we're dead"
	    return
	}
	if {$closing && ![dict size $unsatisfied]} {
	    # we have no more requests to satisfy and we want to close
	    Debug.Httpd {[info coroutine] closing as there's nothing pending}
	    terminate "finally close in responder"
	    return
	}

	# shut down responder if there's nothing to write
	if {![dict size $replies]} {
	    chan event $socket writable ""	;# no point in trying to write
	}

	variable activity

	# send all responses in sequence from the next expected to the last available
	Debug.Httpd {[info coroutine] pending to send: [dict keys $replies]}
	foreach next [lsort -integer [dict keys $replies]] {
	    set activity([info coroutine]) [clock milliseconds]
	    
	    if {[chan eof $socket]} {
		# detect socket closure ASAP in sending
		Debug.Httpd {[info coroutine] Lost connection on transmit}
		terminate "eof on $socket"
		return 1	;# socket's gone - terminate session
	    }
	    
	    # ensure we don't send responses out of sequence
	    if {$next != $response} {
		# something's blocking the response pipeline
		# so we don't have a response for the next transaction.
		# we must therefore wait until all the preceding transactions
		# have something to send
		Debug.Httpd {[info coroutine] no pending or $next doesn't follow $response}
		chan event $socket writable ""	;# no point in trying to write
		
		if {[chan pending output $socket]} {
		    # the client hasn't consumed our output yet
		    # stop reading input until he does
		    chan event $socket readable ""
		} else {
		    # there's space for more output, so accept more input
		    chan event $socket readable [list [info coroutine] READ]
		}

		return 0
	    }
	    set response [expr {1 + $next}]	;# move to next response
	    
	    # respond to the next transaction in trx order
	    # unpack and consume the reply from replies queue
	    # remove this response from the pending response structure
	    lassign [dict get $replies $next] head content close empty
	    dict unset replies $next		;# consume next response

	    # connection close required?
	    # NB: we only consider closing if all pending requests
	    # have been satisfied.
	    if {$close} {
		# inform client of intention to close
		Debug.HttpdLow {close requested on $socket - sending header}
		append head "Connection: close" \r\n	;# send a close just in case
		# Once this header's been sent, we're committed to closing
	    }

	    # send headers with terminating nl
	    chan puts -nonewline $socket "$head\r\n"
	    Debug.Httpd {[info coroutine] SENT HEADER: $socket '[lindex [split $head \r] 0]' [string length $head] bytes} 4
	    chan flush $socket	;# try to flush as early as possible

	    # send the content/entity (if any)
	    # note: we must *not* send a trailing newline, as this
	    # screws up the content-length and confuses the client
	    # which doesn't then pick up the next response
	    # in the pipeline
	    if {!$empty} {
		chan puts -nonewline $socket $content	;# send the content
		Debug.Httpd {[info coroutine] SENT ENTITY: [string length $content] bytes} 8
	    }
	    chan flush $socket

	    # only send for unsatisfied requests
	    catch {dict unset unsatisfied $next}

	    if {$close} {
		return 1	;# terminate session on request
	    }

	    if {[chan pending output $socket]} {
		# the client hasn't consumed our output yet - stop sending more
		break
	    }
	}

	if {[chan pending output $socket]} {
	    # the client hasn't consumed our output yet
	    # stop reading input until he does
	    chan event $socket readable ""
	} else {
	    # there's space for more output, so accept more input
	    chan event $socket readable [list [info coroutine] READ]
	}
    }

    # we have been told we can write a reply
    proc write {r cache} {
	corovars replies response sequence generation satisfied transaction closing unsatisfied socket

	if {$closing && ![dict size $unsatisfied]} {
	    # we have no more requests to satisfy and we want to close
	    terminate "finally close in write"
	}

	if {[dict exists $r -suspend]} {
	    return 0	;# this reply has been suspended - we haven't got it yet
	}

	Debug.Httpd {write [info coroutine] ([rdump $r]) satisfied: ($satisfied) unsatisfied: ($unsatisfied)}

	# fetch transaction from the caller's identity
	if {![dict exists $r -transaction]} {
	    # can't Send reply: no -transaction associated with request
	    Debug.error {Send discarded: no transaction ($r)}
	    return 1	;# close session
	}
	set trx [dict get $r -transaction]

	# discard duplicate responses
	if {[dict exists $satisfied $trx]} {
	    # a duplicate response has been sent - discard this
	    # this could happen if a dispatcher sends a response,
	    # then gets an error.
	    Debug.error {Send discarded: duplicate ([rdump $r])}
	    return 0	;# duplicate response - just ignore
	}

	# only send for unsatisfied requests
	if {![dict exists $unsatisfied $trx]} {
	    Debug.error {Send discarded: satisfied duplicate ([rdump $r])}
	    continue	;# duplicate response - just ignore
	}

	# wire-format the reply transaction - messy
	variable ce_encodings	;# what encodings do we support?
	lassign [format4send $r -cache $cache -encoding $ce_encodings] r header content empty cache
	set header "HTTP/1.1 $header" ;# add the HTTP signifier

	# global consequences - botting and caching
	if {![Honeypot newbot? $r] && $cache} {
	    # handle caching (under no circumstances cache bot replies)
	    Debug.Httpd {Cache put: $header}
	    Cache put $r	;# cache it before it's sent
	}

	# record transaction reply and kick off the responder
	# response has been collected and is pending output
	# queue up response for transmission
	#
	# response is broken down into:
	# header - formatted to go down the line in crlf mode
	# content - content to go down the line in binary mode
	# close? - is the connection to be closed after this response?
	# chunked? - is the content to be sent in chunked mode?
	# empty? - is there actually no content, as distinct from 0-length content?
	Debug.Httpd {[info coroutine] ADD TRANS: ([dict keys $replies])}
	dict set replies $trx [list $header $content [close? $r] $empty]
	dict set satisfied $trx {}	;# record satisfaction of transaction

	# having queued the response, we allow it to be sent on writable
	chan event $socket writable [list [info coroutine] WRITABLE]

	if {[chan pending output $socket]} {
	    # the client hasn't consumed our output yet
	    # stop reading input until he does
	    chan event $socket readable ""
	} else {
	    # there's space for more output, so accept more input
	    chan event $socket readable [list [info coroutine] READ]
	}

	return 0
    }

    # send from consumer via associated reader
    proc csend {reader rsp {cache 1}} {
	# ask socket coro to send the response for us
	if {[catch {
	    $reader [list SEND $rsp]
	} e eo] || $e in {EOF ERROR}} {
	    Debug.error {[info coroutine] sending terminated via $reader: $e ($eo)} 1
	    rename [info coroutine] ""; ::yield	terminated	;# terminate coror
	} else {
	    return $e
	}
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
	corovars consumer
	if {![info exists consumer]} {
	    # a consumer is trying to send directly
	    # ask socket coro to send the response for it
	    corovars reader
	    return [csend $r $cache]
	}

	Debug.Httpd {[info coroutine] send: ([rdump $r]) $cache}

	# check generation
	corovars generation
	if {![dict exists $r -generation]} {
	    # there's no generation here - hope it's a low-level auto response
	    # like Block etc.
	    Debug.log {[info coroutine] Send without -generation ($r)}
	    dict set r -generation $generation
	} elseif {[dict get $r -generation] != $generation} {
	    # report error to sender, but don't die ourselves
	    Debug.error {Send discarded: out of generation [dict get $r -generation] != $generation ($r)}
	    return ERROR
	}

	if {[catch {
	    # send all pending responses, ensuring we don't send out of sequence
	    write $r $cache
	} close eo]} {
	    Debug.error {FAILED write $close ($eo) IP [dict get $r -ipaddr] ([dict get? $r user-agent]) wanted [dict get $r -uri]}

	    terminate closed
	}

	# generate a log line
	variable log
	if {$log ne "" && [catch {
	    puts $log [Http clf $r]	;# generate a log line
	} le leo]} {
	    Debug.error {log error: $le ($leo)}
	}

	# deal with socket
	if {$close} {
	    terminate closed
	}
    }

    # yield wrapper with command dispatcher
    proc yield {{retval ""}} {
	corovars cmd consumer status unsatisfied socket

	while {1} {
	    Debug.HttpdLow {coro [info coroutine] yielding}
	    set x [after info]
	    if {[llength $x] > 10} {
		puts stderr "After: [llength $x]"
	    }

	    # unpack event
	    if {[catch {
		set args [lassign [::yield $retval] op]; set retval ""
	    } e eo]} {
		terminate yieldcrash
	    }
	    if {[eof $socket] || [string match DEAD* [info coroutine]]} {
		terminate "oops - we're dead"
		return
	    }

	    lappend status $op
	    Debug.HttpdLow {yield [info coroutine] -> $op}

	    # record a log of our activity to fend off the reaper
	    variable activity

	    # dispatch on command
	    switch -- [string toupper $op] {
		STATS {
		    set retval {}
		    foreach x [uplevel \#1 {info locals}] {
			catch [list uplevel \#1 [list set $x]] r
			lappend retval $x $r
		    }
		}

		READ {
		    # fileevent tells us there's input to be read
		    # check the channel
		    if {[chan eof $socket]} {
			Debug.Httpd {[info coroutine] eof detected from yield}
			terminate "EOF on reading"
		    } else {
			set activity([info coroutine]) [clock milliseconds]
			return $args
		    }
		}

		CLOSING {
		    # fileevent tells us there's input, but we're half-closed
		    # and won't process any more input, but we want to send
		    # all pending responses
		    if {[chan eof $socket]} {
			# remote end closed - just forget it
			terminate "socket is closed"
		    } else {
			# just read incoming data
			set activity([info coroutine]) [clock milliseconds]
			set x [chan read $socket]
			Debug.Httpd {[info coroutine] is closing, read [string length $x] bytes}
		    }
		}

		SEND {
		    # send a response to client on behalf of consumer
		    set activity([info coroutine]) [clock milliseconds]
		    set retval [send {*}$args]
		}

		WRITABLE {
		    # there is space available in the output queue
		    set retval [respond {*}$args]
		}

		SUSPEND {
		    grace [lindex $args 0]	;# a response has been suspended
		}

		REAPED {
		    # we've been reaped
		    corovars satisfied ipaddr closing headering
		    Debug.Watchdog {[info coroutine] Reaped - status:($status) satisfied:($satisfied) unsatisfied:($unsatisfied) ipaddr:$ipaddr closing:$closing headering:$headering}
		    
		    terminate {*}$args
		}

		TERMINATE {
		    # we've been informed that the socket closed
		    terminate {*}$args
		}

		TIMEOUT {
		    # we've timed out - oops
		    terminate TIMEOUT
		}

		default {
		    error "[info coroutine]: Unknown op $op $args"
		}
	    }
	}
    }

    # handle - handle a protocol error
    proc handle {req {reason "Error"}} {
	Debug.error {handle $reason: ([rdump $req])}

	# we have an error, so we're going to try to reply then die.
	corovars transaction generation status closing socket
	lappend status ERROR
	if {[catch {
	    dict set req connection close	;# we want to close this connection
	    if {![dict exists $req -transaction]} {
		dict set req -transaction [incr transaction]
	    }
	    dict set req -generation $generation

	    # send a response to client
	    send $req 0	;# queue up error response (no caching)
	} r eo]} {
	    dict append req -error "(handler '$r' ($eo))"
	    Debug.error {'handle' error: '$r' ($eo)}
	}

	# return directly to event handler to process SEND and STATUS
	set closing 1
	chan event $socket readable [list [info coroutine] CLOSING]

	Debug.error {'handle' closing}
	return -level [expr {[info level] - 1}]	;# return to the top coro level
	#rename [info coroutine] ""; ::yield	;# terminate coro
    }

    # coroutine-enabled gets
    proc get {socket {reason ""}} {
	variable maxline
	set result [yield]
	set line ""
	while {[chan gets $socket line] == -1 && ![chan eof $socket]} {
	    set result [yield]
	    if {$maxline && [chan pending input $socket] > $maxline} {
		handle [Http Bad $request "Line too long"] "Line too long"
	    }
	}

	if {[chan eof $socket]} {
	    terminate $reason	;# check the socket for closure
	}

	# return the line
	Debug.HttpdLow {[info coroutine] get: '$line' [chan blocked $socket] [chan eof $socket]}
	return $line
    }

    # coroutine-enabled read
    proc read {socket size} {
	# read a chunk of $size bytes
	set chunk ""
	while {$size && ![chan eof $socket]} {
	    set result [yield]
	    set chunklet [chan read $socket $size]
	    incr size -[string length $chunklet]
	    append chunk $chunklet
	}
	
	if {[chan eof $socket]} {
	    terminate entity	;# check the socket for closure
	}
	
	# return the chunk
	Debug.HttpdLow {[info coroutine] read: '$chunk'}
	return $chunk
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
		    handle [Http Bad $request "Illegal header: '$line'"] "Illegal Header"
		}
	    }
	}

	return $r
    }

    proc reader {args} {
	Debug.Httpd {create reader [info coroutine] - $args}

	# unpack all the passed-in args
	set replies {}	;# dict of replies pending
	set requests {}	;# dict of requests unsatisfied
	set satisfied {};# dict of requests satisfied
	set unsatisfied {} ;# dict of requests unsatisfied
	set response 1	;# which is the next response to send?
	set sequence -1	;# which is the next response to queue?
	set writing 0	;# we're not writing yet
	set ipaddr 0	;# ip address

	dict with args {}
	set transaction 0	;# count of incoming requests
	set status INIT	;# record transitions
	set closing 0	;# flag that we want to close

	# keep receiving input requests
	while {1} {
	    # get whole header
	    set headering 1
	    set lines {}
	    while {$headering} {
		set line [get $socket HEADER]
		Debug.HttpdLow {reader [info coroutine] got line: ($line)}
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
	    dict set r -header $header
	    dict set r -method [string toupper [lindex $header 0]]
	    switch -- [dict get $r -method] {
		CONNECT -
		LINK {
		    # stop the bastard SMTP spammers
		    Block block [dict get $r -ipaddr] "CONNECT method"
		    handle [Http NotImplemented $r "Connect Method"] "CONNECT method"
		}

		GET - PUT - POST - HEAD {}

		default {
		    # Could check for and service FTP requestuests, etc, here...
		    dict set r -error_line $line
		    handle [Http Bad $r "Method unsupported '[lindex $header 0]'" 405] "Method Unsupported"
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
		handle [Http Bad $r "URI too long '[dict get $r -uri]'" 414] "URI too long"
	    }

	    if {[string match HTTP/* [dict get $r -version]]} {
		dict set r -version [lindex [split [dict get $r -version] /] 1]
	    }

	    # Send 505 for protocol != HTTP/1.0 or HTTP/1.1
	    if {([dict get $r -version] != 1.1)
		&& ([dict get $r -version] != 1.0)} {
		handle [Http Bad $r "HTTP Version '[dict get $r -version]' not supported" 505] "Unsupported HTTP Version"
	    }

	    Debug.Httpd {[info coroutine] reader got request: ($r)}

	    # parse the URL
	    set r [dict merge $r [Url parse [dict get $r -uri]]]

	    # re-check the connections and log a bit of stuff
	    variable connbyIP
	    variable max_conn
	    if {[info exists connbyIP([dict get $r -ipaddr])]
		 && $connbyIP([dict get $r -ipaddr]) > $max_conn
	     } {
		dict set r -OC 1
		# let's log this sucker and see what he's asking for
		Debug.log {Overconnector [dict get $r -ipaddr] ([dict get? $r user-agent]) wants [dict get $r -uri]}
	    }

	    # block spiders by UA
	    if {[info exists ::spiders([dict get? $r user-agent])]} {
		Block block [dict get $r -ipaddr] "spider UA ([dict get? $r user-agent])"
		handle [Http NotImplemented $r "Spider Service"] "Spider"
	    }

	    # analyse the user agent strings.
	    dict set r -ua [ua [dict get? $r user-agent]]

	    # check the incoming ip for blockage
	    if {[Block blocked? [dict get? $r -ipaddr]]} {
		handle [Http Forbidden $r] Forbidden
	    } elseif {[Honeypot guard r]} {
		# check the incoming ip for bot detection
		# this is a bot - reply directly to it
		send $r	0	;# queue up error response
		Debug.log {Honeypot Sticks [dict get $r -ipaddr] ([dict get? $r user-agent]) wants [dict get $r -uri]}
		continue
	    }

	    # ensure that the client sent a Host: if protocol requires it
	    if {[dict exists $r host]} {
		# client sent Host: field
		if {[string match http*:* [dict get $r -uri]]} {
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
		handle [Http Bad $r "HTTP 1.1 required to send Host"] "No Host"
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
	    Debug.Httpd {[info coroutine] reader complete: $header ([rdump $r])}

	    # rename fields whose names are the same in request/response
	    foreach n {cache-control pragma} {
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
			Debug.log {Got a $tel transfer-encoding which we can't handle}
			handle [Http NotImplemented $r "$tel transfer encoding"] "Unimplemented TE"
			continue
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
		handle [Http Bad $r "Length Required" 411] "Length Required"
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
	    if {"chunked" in [dict get? $r -te]} {
		set chunksize 1
		while {$chunksize} {
		    chan configure $socket -translation {crlf binary}
		    set chunksize 0x[get $socket CHUNK]
		    chan configure $socket -translation {binary binary}
		    if {$chunksize eq "0x"} {
			Debug.HttpdLow {[info coroutine] Chunks all done}
			break	;# collected all the chunks
		    }
		    set chunk [read $socket $chunksize]
		    Debug.HttpdLow {[info coroutine] Chunk: $chunksize ($chunk)}
		    get $socket CHUNK
		    dict append r -entity $chunk

		    # enforce server limits on Entity length
		    variable maxentity
		    if {$maxentity > 0
			&& [string length [dict get $r -entity]] > $maxentity} {
			# 413 "Request Entity Too Large"
			handle [Http Bad $r "Request Entity Too Large" 413] "Entity Too Large"
		    }
		}
	    } elseif {[dict exists $r content-length]} {
		set left [dict get $r content-length]

		# enforce server limits on Entity length
		variable maxentity
		if {$maxentity > 0 && $left > $maxentity} {
		    # 413 "Request Entity Too Large"
		    handle [Http Bad $r "Request Entity Too Large" 413] "Entity Too Large"
		}

		if {$left == 0} {
		    dict set r -entity ""
		    # the entity, length 0, is therefore already read
		    # 14.13: Any Content-Length greater than
		    # or equal to zero is a valid value.
		} else {
		    set entity ""
		    chan configure $socket -translation {binary binary}
		    Debug.HttpdLow {[info coroutine] reader getting entity of length ($left)}
		    while {$left > 0} {
			set chunk [read $socket $left]
			incr left -[string length $chunk]
			Debug.HttpdLow {[info coroutine] reader getting remainder of entity of length ($left)}
			dict append r -entity $chunk
			Debug.HttpdLow {[info coroutine] reader got whole entity}
		    }
		}
	    }

	    # reset socket to header config, having read the entity
	    chan configure $socket -encoding binary -translation {crlf binary}

	    # now we postprocess/decode the entity
	    Debug.entity {entity read complete - '[dict get? $r -te]'}
	    if {"gzip" in [dict get? $r -te]} {
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
		foreach xff [split [dict get? $r x-forwarded-for] ,] {
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
		dict set unsatisfied [dict get $cached -transaction] {}

		Debug.Httpd {[info coroutine] sending cached ([rdump $cached])}
		lappend status CACHED
		send $cached 0	;# send cached response directly
		continue
	    }

	    # deliver request to consumer
	    if {[info commands $consumer] ne {}} {
		# deliver the assembled request to the consumer
		dict set unsatisfied [dict get $r -transaction] {}
		dict set r -send [info coroutine]	;# let consumer know how to reply
		lappend status PROCESS
		after 1 [list catch [list $consumer [list REQUEST $r]]]
		Debug.Httpd {reader [info coroutine]: sent to consumer, waiting for next}
	    } else {
		# the consumer has gone away
		Debug.Httpd {reader [info coroutine]: consumer gone $consumer}
		lappend status DOA
		set closing 1
		chan event $socket readable [list [info coroutine] CLOSING]
	    }
	}
    }

    # run the postprocess
    proc pprocess {r} {
	if {[catch {
	    post $r
	} rsp eo]} {
	    Debug.error {[info coroutine] POST ERROR: $rsp ($eo)} 1
	    return [Http ServerError $r $rsp $eo]
	} else {
	    Debug.Httpd {[info coroutine] POST: [rdump $rsp]} 10
	    return $rsp
	}
    }

    # handle responses from a client
    proc client {op connection args} {
	variable client
	if {[info exists client($connection)]} {
	    apply $client($connection) $op $connection {*}$args
	}
    }

    # the request consumer
    proc consumer {args} {
	Debug.Httpd {consumer: $args}
	dict with args {}

	set retval ""
	while {1} {
	    # unpack event
	    set args [lassign [::yield $retval] op]; set retval ""

	    switch -- $op {
		TERMINATE {
		    Debug.Httpd {consumer [info coroutine] terminating because "$op $args"}
		    catch {
			do TERMINATE $args
		    } e eo	;# process the termination
		    return -level [info level] terminated	;# terminate coro
		}

		RESUME {
		    # post-process the response
		    set r [dict merge [lindex $args 0] {*}[lrange $args 1 end]]
		    csend $reader [pprocess $r]
		}

		RESPONSE -
		CLOSED {
		    # HTTP client has responded or closed
		    client $op {*}$args
		}

		REQUEST {
		    Debug.Httpd {consumer [info coroutine] got: $op ($args)}
		    set r [dict merge [lindex $args 0] {*}[lrange $args 1 end]]
		    catch {
			do REQUEST [pre $r]
		    } rsp eo	;# process the request
		    # handle response code
		    
		    switch [dict get $eo -code] {
			0 -
			2 {
			    # does application want to suspend?
			    if {[dict size $rsp] == 0 || [dict exists $rsp -suspend]} {
				if {[dict size $rsp] == 0} {
				    set duration 0
				} else {
				    set duration [dict get $rsp -suspend]
				}
				$reader [list SUSPEND $duration]
				continue
			    }

			    # ok - return
			    if {![dict exists $rsp -code]} {
				set rsp [Http Ok $rsp]
			    }
			}
		
			1 { # error
			    set rsp [Http ServerError $r $rsp $eo]
			}
		    }

		    csend $reader [pprocess $rsp]
		}
	    }
	}
    }

    # return a bunch of status information about sock procs
    proc stats {} {
	set result {}
	foreach coro [info commands ::Httpd::sock*] {
	    lappend result $coro [$coro STATS]
	}
	return $result
    }

    # return a bunch of data about all the channels in use by Httpd
    proc chans {} {
	foreach cchan [info commands ::Httpd::sock*] {
	    set chan [namespace tail $cchan]
	    catch {
		list eof [chan eof $chan] input [chan pending input $chan] output [chan pending output $chan] blocked [chan blocked $chan] readable [chan event $chan readable] writable [chan event $chan writable] {*}[chan configure $chan]
	    } el
	    lappend result "name $chan $el"
	}
	return $result
    }

    # grant the caller some timeout grace
    proc grace {{grace 20000}} {
	variable activity
	if {$grace < 0} {
	    # take this coro off the reaper's list until next activity
	    catch {unset activity([info coroutine])}
	} else {
	    set activity([info coroutine]) [expr {$grace + [clock milliseconds]}]
	}
    }

    # format something to suspend the consumer coro
    proc Suspend {{grace -1}} {
	return [list -suspend $grace]
    }

    # every script
    proc every {interval script} {
	variable everyIds
	if {$interval eq "cancel"} {
	    after cancel $everyIds($script)
	    return
	}
	set everyIds($script) [after $interval [info level 0]]	;# restart the timer
	set rc [catch {
	    uplevel #0 $script
	} result eo]
	if {$rc == [catch break]} {
	    after cancel $everyIds($script)
	    set rc 0
	} elseif {$rc == [catch continue]} {
	    # Ignore - just consume the return code
	    set rc 0
	} elseif {$rc == [catch error]} {
	    Debug.error {every: $interval ($script) - ERROR: $result ($eo)}
	}

	# TODO: Need better handling of errorInfo etc...
	#return -code $rc $result
	return $result
    }

    proc kill {args} {
	Debug.Watchdog {killing: "$args"}
	foreach what $args {
	    if {[catch {
		rename $what {}	;# kill this coro right now
	    } r eo]} {
		Debug.Watchdog {killed $what: '$r' ($eo)}
	    }
	}
    }

    variable reaper	;# array of hardline events 
    proc reaper {} {
	variable timeout
	set now [clock milliseconds]
	set then [expr {$now - $timeout}]
	Debug.Watchdog {Reaper Running [Http Now]}

	foreach s [chan names] {
	    catch {
		if {[eof $s]} {
		    close $s
		}
	    }
	}

	variable reaper
	foreach {n v} [array get reaper] {
	    unset reaper($n)
	    if {$v < $now} {
		catch {kill $n}
	    }
	}

	variable activity
	foreach {n v} [array get activity] {
	    catch {
		if {[info commands $n] eq {}} {
		    catch {unset activity($n)}	;# this is bogus
		} elseif {$v < $then} {
		    Debug.Watchdog {Reaping $n}
		    catch {unset activity($n)}	;# prevent double-triggering
		    catch {$n REAPED}	;# alert coro to its fate
		    set reaper($n) [expr {$now + 2 * $timeout}]	;# if it doesn't respond, kill it.
		}
	    }
	}
    }

    proc pre {req} {
	package require Cookies
	package require Session
	proc pre {req} {
	    return [::Cookies 4Server $req]	;# fetch the cookies
	}
	return [pre $req]
    }

    proc post {req} {
	package require Cookies
	package require Convert
	proc post {req} {
	    return [::Convert do $req]	;# got to do post-conversion
	}
	return [post $req]
    }

    proc do {op req} {
	#Debug on Httpd 10
	#Debug on HttpdLow 10
	if {[info commands ::wub] eq {}} {
	    package require Mason
	    Mason create ::wub -url / -root $::Site::docroot -auth .before -wrapper .after
	}
	proc do {op req} {
	    switch -- $op {
		REQUEST {
		    switch -glob -- [dict get $req -path] {
			/ -
			/* {
			    # redirect / to /wub
			    return [::wub do $req]
			}
		    }
		}
		TERMINATE {
		    return
		}
		RESPONSE {
		    # HTTP client has sent us a response
		}
		CLOSED {
		    # HTTP client has closed.
		}
		default {
		    error "[info coroutine] OP $op not understood by consumer"
		}
	    }
	}
	return [do REQUEST $req]
    }

    # connect - process a connection request
    proc Connect {sock ipaddr rport args} {
	# the socket must stay in non-block binary binary-encoding mode
	chan configure $sock -blocking 0 -translation {binary binary} -encoding binary

	switch -- [::ip::type $ipaddr] {
	    "normal" {
		# check list of blocked ip addresses
		if {[Block blocked? $ipaddr]} {
		    Debug.log {Blocked attempt from $ipaddr}

		    # dump this connection with a minimum of fuss.
		    variable server_id
		    puts $sock "HTTP/1.1 403 Forbidden\r"
		    puts $sock "Date: [Http Now]\r"
		    puts $sock "Server: $server_id\r"
		    puts $sock "Connection: Close\r"
		    puts $sock "Content-Length: 0\r"
		    puts $sock \r
		    flush $sock
		    close $sock
		    return
		}

		# check spamming
		if {[countConnections $sock $ipaddr]} return
	    }

	    "private" {
		# TODO - this may not be desired behavior.  ReThink
		# just because an ip connection is local doesn't mean it's
		# unlimited, does it?
		# OTOH, it may just be from a local cache, and the original
		# ip address may come from a higher level protocol.
	    }
	}

	# record connection id - unique over the life of this server process
	variable cid; set id [incr cid]
	dict set args -cid $id

	# record significant values
	dict set args -sock $sock
	dict set args -ipaddr $ipaddr
	dict set args -rport $rport
	dict set args -received_seconds [clock seconds]

	# get port on which connection arrived
	# this may differ from Listener's port if reverse proxying
	# or transparent ip-level forwarding is performed
	variable server_port
	if {[info exists server_port]} {
	    # use defined server port
	    dict set args -port $server_port
	} else {
	    # use listener's port
	}

	# record some per-server request values
	variable server_id; dict set args -server_id $server_id
	dict set args -version 1.1	;# HTTP/1.1

	# condition the socket
	chan configure $sock -buffering none -translation {crlf binary}

	# generate a connection record prototype
	variable generation	;# unique generation
	set gen [incr generation]
	set args [dict merge $args [list -generation $gen]]

	# create reader coroutine
	variable reader
	set R ::Httpd::${sock}_[uniq]

	# the old socket stuff hasn't yet been cleaned up.
	# this is potentially very bad.
	foreach n [info commands ::Httpd::${sock}_*] {
	    Debug.log {reader $R not dead yet, rename to ${R}_DEAD to kill it.}
	    catch {rename $n DEAD_$n}
	    catch {DEAD_$n [list TERMINATE "socket's gone"]}	;# ensure the old reader's dead
	    catch {rename DEAD_$n ""}
	}

	# construct consumer
	set cr ::Httpd::CO_${sock}_[uniq]
	
	foreach n [info commands ::Httpd::CO_${sock}_*] {
	    # the consumer seems to be lingering - we have to tell it to die
	    Debug.log {consumer $cr not dead yet, rename to kill it.}
	    catch {rename $n DEAD_$n}
	    catch {DEAD_$n ""}	;# ensure the old consumer's dead
	    catch {rename DEAD_$n ""}	;# really kill it
	}
	coroutine $cr ::Httpd::consumer reader $R

	# construct the reader
	variable timeout
	variable log
	set result [coroutine $R ::Httpd::reader socket $sock consumer $cr prototype $args generation $gen cid $cid log $log]

	#trace add command $cr {rename delete} [list ::Httpd::corogone $R]
	#trace add command $R {rename delete} [list ::Httpd::sockgone $cr $sock]

	# start the ball rolling
	chan event $sock readable [list $R READ]

	return $result
    }

    # this is not used
    proc corogone {match from to op} {
	if {$op eq "delete"} {
	    catch {$match TERMINATE}
	    Debug.Httpd {CORO $op: $from}
	} elseif {$op eq "rename"} {
	    Debug.Httpd {CORO $op: $from $to ([trace info command $from]) ([trace info command $to])}
	}
    }

    # this is not used
    proc sockgone {match sock from to op} {
	if {$op eq "delete"} {
	    # clean up socket - the only point where we close
	    catch {chan event $socket readable ""}	;# is this necessary?
	    catch {chan event $socket writable ""}	;# is this necessary?
	    catch {chan close $sock}
	    catch {$match TERMINATE}
	    Debug.Httpd {SOCK $op: $from $to}
	} elseif {$op eq "rename"} {
	    Debug.Httpd {SOCK $op: $from $to -([trace info command $from]) ([trace info command $to])}
	}
    }

    # common log format log - per request, for log analysis
    variable log ""	;# fd of open log file - default none
    variable logfile ""	;# name of log file - "" means none

    # configure - set Httpd protocol defaults
    proc configure {args} {
	variable {*}$args

	# open the web analysis log
	variable logfile
	variable log
	if {$logfile ne "" && $log eq ""} {
	    set log [open $logfile a]		;# always add to the end
	    fconfigure $log -buffering line	;# we want to try to make writes atomic
	}
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

Debug on block 10

if {[info exists argv0] && ($argv0 eq [info script])} {
    package require Stdin
    package require Listener
    package require Debug

    Debug off socket 10
    Debug off http 2
    Debug off cache 10

    set listener [Listener %AUTO% -port 8080 -sockets Httpd -httpd {-dispatch "puts"}]
    set forever 0
    vwait forever
}

Httpd every $Httpd::timeout {Httpd reaper}	;# start the inactivity reaper
