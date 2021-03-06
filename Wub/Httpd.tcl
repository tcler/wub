package require Debug
Debug define httpd 10
Debug define httpdlow 10
Debug define httpdclient 10
Debug define httpdthread 10
Debug define watchdog 10
Debug define reaper 10
Debug define stamp 0
Debug define httpd-resume 0
Debug define entity 10

package require Listener
package require Chan
if {[catch {package require WebSockets} e eo]} {
    puts stderr "No WebSockets: '$e' ($eo)"
}

package require Query
package require Html
package require Url
package require Http
package require OO

package provide Httpd 6.0

# Dispatcher - contains all necessary code to dispatch on request
namespace eval ::Dispatcher {
    proc pre {r} {
	package require Cookies
	if {[llength [info command ::pest]]} {
	    proc pre {r} {
		# default request pre-process
		::pest $r
		set r [::Cookies 4Server $r]	;# fetch the cookies
		set r [Human track $r]	;# track humans by cookie
		return $r
	    }
	} else {
	    proc pre {r} {
		# default request pre-process
		set r [::Cookies 4Server $r]	;# fetch the cookies
		set r [Human track $r]	;# track humans by cookie
		return $r
	    }
	}
	return [pre $r]
    }

    proc post {r} {
	set r [Http timestamp $r post]
	# do per-connection postprocess (if any)
	foreach c [dict get? $r -postprocess] {
	    set r [{*}$c $r]
	}

	# do per-connection conversions (if any)
	foreach c [dict get? $r -convert] {
	    set r [$c convert $r]
	}

	# do default conversions
	set r [Http timestamp $r convert]
	set r [::convert convert $r]
	set r [Http timestamp $r postpost]
	return $r
    }

    proc /clients {r} {
	foreach client [info class instances ::HttpdClient] {
	    set connections [$client connections]
	    foreach connection $connections {
	    }
	}
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

# HttpdClient - an object for each Httpd client currently connected
oo::class create ::HttpdClient {
    method add_ {what} {
	variable connections
	dict set connections $what [clock seconds]
	Debug.httpdclient {[self] add $what [dict size $connections]}
    }

    classmethod del_ {ip} {
	classvar clients
	dict unset clients $ip
    }

    method del {what} {
	variable connections
	dict unset connections $what
	Debug.httpdclient {[self] del $what [dict size $connections]}
	if {[dict size $connections] == 0} {
	    variable ip
	    Debug.httpdclient {destroying [self] for $ip}
	    my del_ $ip
	    my destroy
	}
    }

    method connections {} {
	variable connections
	return $connections
    }

    classmethod add {ip what} {
	classvar clients
	if {![info exists clients]} {
	    set clients {}
	}
	if {[dict exists $clients $ip]} {
	    set x [dict get $clients $ip]
	    Debug.httpdclient {found existing $x for ip:$ip}
	} else {
	    set x [HttpdClient new ip $ip]
	    dict set clients $ip $x
	    Debug.httpdclient {created new $x for ip:$ip}
	}
	Debug.httpdclient {add $ip $what -> $x}
	$x add_ $what
	return $x
    }

    classmethod all {} {
	classvar clients
	return $clients
    }

    constructor {args} {
	variable {*}$args
	variable connections {}
	Debug.httpdclient {constructed [self] for $ip}
    }
}

# watchdog timer
namespace eval ::watchdog {
    variable timeout 60000	;# mS of permissable inactivity
    variable activity	;# this variable records activity of entities being watched
    set activity() ""; unset activity()

    # stroke - placate the watchdog
    proc stroke {what} {
	variable activity
	set activity($what) [clock milliseconds]
    }

    # grace - give entity some grace
    proc grace {what {grace 20000}} {
	variable activity
	if {$grace < 0} {
	    # take this coro off the reaper's list until next activity
	    Debug.watchdog {Giving $what infinite grace}
	    catch {unset activity($what)}
	} else {
	    Debug.watchdog {Giving $what $grace grace}
	    set activity($what) [expr {$grace + [clock milliseconds]}]
	}
    }

    # gone - this entity is gone
    proc gone {what} {
	variable activity
	catch {unset activity($what)}
    }

    # every - run $script every $interval mS
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
	} elseif {$rc == 1} {
            # the script errored
	    Debug.error {every: $interval ($script) - ERROR: $result ($eo)}
	}

	# TODO: Need better handling of errorInfo etc...
	#return -code $rc $result
	return $result
    }

    proc reaper {} {
	Debug.watchdog {Reaper Running [Http Now]}

	variable timeout
	set kill [expr {[clock milliseconds] - $timeout}]

	# kill any moribund entities
	variable activity
	foreach {n v} [array get activity] {
	    if {[info commands $n] eq {}} {
		Debug.log {Bogus watchdog over $n}
		catch {unset activity($n)}	;# this is bogus
	    } elseif {$v < $kill} {
		# try to give some status of thing being destroyed
		Debug.watchdog {Reaping $n}
		catch {unset activity($n)}	;# prevent double-triggering
		Debug.reaper {$n status: [$n state]}
		if {[$n eof]} {
		    Debug.log {$n lingered: [$n state]}
		}
		if {[catch {
		    $n terminate "Reaped"	;# kill this entity right now
		} e eo]} {
		    Debug.error {killed $n: '$e' ($eo)}
		}
	    }
	}
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}

    ::watchdog every $timeout {::watchdog reaper}	;# start the inactivity reaper
}

# Httpd - an object for each Httpd connection
oo::class create ::Httpd {
    # timing - make a nice little table of uS times of state transitions for request dict
    method timing {r} {
	set timings [dict get $r -time]
	dict set timings connected 0
	set result {}
	set running 1
	set index 1
	foreach {state time} $timings {
	    if {$state eq "connected"} continue
	    lappend result $state [expr {$time - [lindex $timings $index]}]
	    incr index 2
	}

	if {[info exists ::Httpd::timers]} {
	    dict set ::Httpd::timers [dict get $r -pipeline] [dict get $r -transaction] [list [dict get $r -ipaddr] [dict get $r -url] {*}$result]
	}

	return "remote:[dict get $r -ipaddr] pipeline: [dict get $r -pipeline] tx:[dict get $r -transaction] ([dict get $r -url]) - [join $result]"
    }

    # state - report on connection state
    method state {} {
	variable istate
	variable ostate
	set rr [list -istate $istate -ostate $ostate]

	variable events
	dict set rr -comm_events $events

	variable socket
	foreach f {blocked eof {pending input} {pending output}} {
	    catch {dict set rr -[join $f _] [chan {*}$f $socket]}
	}

	variable replies
	dict set rr -replies [dict size $replies]
	variable unsatisfied
	dict set rr -unsatisfied [dict size $unsatisfied]
	dict for {u v} $unsatisfied {
	    if {[catch {
		dict lappend rr -unsatisfied [dict get $v -url]
	    }]} {
		dict lappend rr -unsatisfied [dict get $v -uri]
	    }
	}

	variable request
	if {[dict size $request]} {
	    set rr [dict merge $rr $request]
	    foreach f {-content -entity -gzip} {
		if {[dict exists $rr $f]} {
		    dict set rr $f "<ELIDED [string length [dict get? $rr $f]]>"
		}
	    }
	}

	return $rr
    }

    method eof {} {
	variable socket
	return [chan eof $socket]
    }

    # control the writable state of $socket
    method Unwritable {} {
	variable socket
	variable events
	chan event $socket writable ""
	dict unset events writable
    }
    method Writable {{what respond}} {
	variable socket
	variable events
	dict set events writable $what
    }

    # control the readable state of $socket
    method Unreadable {} {
	variable socket
	variable events
	catch {chan event $socket readable ""}
	catch {dict unset events readable}
    }
    method Readable {{what READ}} {
	variable socket
	variable events
	dict set events readable $what
    }

    # terminate - destroy self, closing files and recovering resources
    method terminate {reason} {
	variable socket
	Debug.httpd {[self] terminated $reason over $socket}
	my destroy
    }

    # handle - handle a protocol error
    # close read-side of socket, send an error response
    method handle {r {reason "Error"}} {
	Debug.error {handle $reason: ([Httpd dump $r])}

	# we have an error, so we're going to try to reply then die.
	variable socket
	catch {chan close $socket read}	;# close the read direction of socket

	if {[catch {
	    if {![dict exists $r -transaction]} {
		variable transaction
		dict set r -transaction [incr transaction]
	    }

	    # send a response to client
	    my send $r 0	;# queue up error response (no caching)
	} e eo]} {
	    dict append r -error "(handler '$e' ($eo))"
	    Debug.error {'handle' error: '$e' ($eo)}
	}
    }

    # Yield wrapper with command dispatcher
    method Yield {{retval ""}} {
	variable socket
	variable last
	variable events

	if {![info exists last]} {
	    set last [::tcl::clock::milliseconds]
	}

	# this will repeat until we get a READ indication
	variable live 1
	while {$live} {
	    Debug.httpdlow {coro [info coroutine] yielding in [self] over socket $socket}

	    # turn on selected events for this connection
	    dict for {k v} $events {
		if {[catch {chan event $socket $k [list [info coroutine] {*}$v]} e eo]} {
		    Debug.error {coro [info coroutine] 'chan event $k $v' FAIL $e ($eo)}
		} else {
		    Debug.httpdlow {coro [info coroutine] 'chan event $k $v'}
		}
	    }

	    # unpack event
	    set args [lassign [::yieldm $retval] op]; set retval ""

	    # turn off all events for this connection
	    foreach k {readable writable} {
		catch {chan event $socket $k ""}
	    }

	    #Debug.httpdlow {back from yield [info coroutine] -> $op ($args)}

	    # dispatch on command
	    switch -- $op {
		READ {
		    # fileevent tells us there's input to be read
		    # and we're waiting on input
		    variable unsatisfied
		    if {[chan pending input $socket] == -1
			|| [chan eof $socket]
		    } {
			Debug.httpd {[info coroutine] eof detected from yield ([dict size $unsatisfied] replies remaining)}
			my Unreadable	;# turn off reader

			# determine whether there's anything pending
			if {![dict size $unsatisfied]} {
			    my terminate EOF
			    return -level [expr {[info level]-1}]	;# return to the top coro level
			}
		    } else {
			::watchdog stroke [self]
			return $args	;# return to the reader
		    }
		}

		default {
		    Debug.httpdlow {back from yield [info coroutine] -> $op ($args)}
		    [self] $op {*}$args	;# coro calls object
		}
	    }

	    Debug.httpdlow {coro [info coroutine] processed '$op' in [self]}
	}

	return -level [expr {[info level]-1}]	;# destroy the coro
    }

    # inbound entity fcopy has completed - now process the request
    method fcin {r fd bytes read {error ""}} {
	variable socket
	Debug.entity {[info coroutine] fcin: entity:$fd expected:$bytes read:$read error:'$error'}
	# reset socket to header config, having read the entity or failed
	chan configure $socket -encoding binary -translation {crlf binary}

	set r [::Httpd timestamp $r fcindone]
	if {[set gone [catch {chan eof $socket} eof]] || $eof} {
	    # detect socket closure ASAP in sending
	    Debug.entity {[info coroutine] Lost connection on fcin}
	    if {$error eq ""} {
		set error "eof on $socket in fcin"
	    }
	}

	# if $bytes != $written or $error ne "", we have a problem
	variable outbuffer
	if {$gone || $eof || $bytes != $read || $error ne ""} {
	    if {$error eq ""} {
		set error "fcin failed to receive $bytes bytes, only got $read."
	    }
	    Debug.error $error
	    my terminate "$error in fcin"
	    return
	} elseif {[chan pending output $socket] <= $outbuffer} {
	    # only when the client has consumed our output do we
	    # restart reading input
	    Debug.entity {[info coroutine] fcin: restarting reader}
	    my Readable	;# this will restart the reading loop
	} else {
	    Debug.entity {[info coroutine] fcin: suspending reader [chan pending output $socket]}
	}

	# see if the writer needs service
	my Writable

	# at this point we have a complete entity in $entity file, it's already been ungzipped
	# we need to process it somehow
	chan seek $fd 0

	# invoke process in coroutine
	variable coro; after 0 [list $coro process $r]
    }

    # process a chunk which has been fcopied in
    method fchunk {r raw entity total bytes read {error ""}} {
	variable socket
	Debug.entity {[info coroutine] fchunk: raw:$raw entity:$entity read:$read error:'$error'}
	incr total $bytes	;# keep track of total read

	if {[set gone [catch {chan eof $socket} eof]] || $eof} {
	    # detect socket closure ASAP in sending
	    Debug.entity {[info coroutine] Lost connection on fchunk}
	    if {$error eq ""} {
		set error "eof on $socket in fchunk"
	    }
	}

	# if $bytes != $written or $error ne "", we have a problem
	if {$gone || $eof || $bytes != $read || $error ne ""} {
	    if {$error eq ""} {
		set error "fchunk failed to receive all my chunks - expected:$bytes got:$read."
	    }
	    Debug.error $error
	    my terminate "$error in fchunk"
	    return
	}

	# read a chunksize
	chan configure $socket -translation {crlf binary} -encoding [encoding system]
	set chunksize 0x[get $socket FCHUNK]	;# we have this many bytes to read
	chan configure $socket -translation {binary binary} -encoding [dict get $r -encoding]

	if {$chunksize ne "0x0"} {
	    Debug.entity {[info coroutine] fchunking along}
	    # enforce server limits on Entity length
	    variable maxentity
	    if {$maxentity > 0 && $total > $maxentity} {
		# 413 "Request Entity Too Large"
		my handle [Http Bad $r "Request Entity Too Large" 413] "Entity Too Large"
		return -code break
	    } else {
		# do another fchunk
		chan copy $raw $entity -size $chunksize -command [list [info coroutine] fchunk $r $raw $entity $total $chunksize]
	    }
	    return	;# await arrival
	}

	# we have all the chunks we're going to get
	# reset socket to header config, having read the entity or failed
	chan configure $socket -encoding binary -translation {crlf binary}
	variable outbuffer
	if {[chan pending output $socket] <= $outbuffer} {
	    # only when the client has consumed our output do we
	    # restart reading input
	    Debug.entity {[info coroutine] fchunk: restarting reader}
	    my Readable	;# this will restart the reading loop
	} else {
	    Debug.entity {[info coroutine] fchunk: suspending reader [chan pending output $socket]}
	}

	# see if the writer needs service
	my Writable

	Debug.entity {got chunked entity in $entity}

	# at this point we have a complete entity in $entity file, it's already been ungzipped
	# we need to process it somehow

	chan seek $entity 0
	variable files; set epath [dict get $files $entity]
	variable todisk
	if {$todisk < 0 || [file size $epath] <= $todisk} {
	    # we don't want to have things on disk, or it's small enough to have in memory
	    # ??? How is entity encoded?
	    dict set r -entity [dict read $entity]	;# grab the entity
	    chan close $entity				;# close the entity fd
	    file delete [dict get $files $entity]	;# clean up the file
	    dict unset files $entity			;# don't need to clean up for us
	} else {
	    # leave some hints for Query file processing
	    dict set r -entity $entity
	    dict set r -entitypath [dict get $files $entity]
	}

	variable istate POST_ENTITY
	variable coro; after 0 [list $coro my process $r]
    }

    # headers - read headers as a block
    method Headers {} {
	variable istate HEADERS
	upvar 1 r r
	# keep receiving input requests
	set headering 1
	set lines {}
	variable maxline; variable maxheaders
	variable socket
	set hstart 0

	while {$headering} {
	    my Yield	;# wait for READ event
	    set line ""
	    set status 0
	    while {![chan eof $socket]
		   && [chan pending input $socket] > -1
		   && [set status [chan gets $socket line]] == -1
	       } {
		Debug.httpdlow {[info coroutine] Get: '$line' [chan blocked $socket] [chan pending input $socket]}

		my Yield	;# wait for READ event
		if {$maxline && [chan pending input $socket] > $maxline} {
		    my handle [Http Bad $r "Line too long (over $maxline)"] "Line too long (over $maxline) '[string range $line 0 20]..."
		    return -code break	;# signal error to caller
		}
	    }

	    Debug.httpdlow {line:'$line' status:$status eof:[chan eof $socket] pending:[chan pending input $socket]}

	    if {!$hstart} {
		set hstart [clock microseconds]	;# time we got first line
	    }

	    Debug.httpdlow {reader [info coroutine] got line: ($line)}
	    if {[string trim $line] eq ""} {
		# rfc2616 4.1: In the interest of robustness,
		# servers SHOULD ignore any empty line(s)
		# received where a Request-Line is expected.
		if {[llength $lines]} {
		    set headering 0
		}
	    } else {
		lappend lines $line	;# append all lines in header
		if {[llength $lines] > $maxheaders} {
		    my handle [Http Bad $r "Too many headers (over $maxheaders)"] "Too many headers (over $maxheaders)"
		    return -code break	;# signal error to caller
		}
	    }
	}

	# record some timings
	variable start;
	set r [::Httpd timestamp $r headerstart $hstart]
	set r [::Httpd timestamp $r headerdone]

	# record the header
	set lines [lassign $lines header]
	dict set r -header $header
	lassign [split $header] method
	dict set r -method [string toupper $method]

	# ensure the method is valid
	switch -- [dict get $r -method] {
	    GET - PUT - POST - HEAD - OPTIONS {
		# these are acceptable headers - pass through
	    }

	    CONNECT -
	    LINK {
		# stop the bastard SMTP spammers
		variable istate BAD_METHOD
		Block block [dict get $r -ipaddr] "[dict get $r -method] method ([dict get? $r user-agent])"
		my handle [Http NotImplemented $r "Connect Method [dict get $r -method]"] "CONNECT method [dict get $r -method]"
		return -code break	;# signal error to caller
	    }

	    default {
		# Could check for and service FTP requests, etc, here...
		variable istate UNKNOWN_METHOD
		dict set r -error_line $line
		my handle [Http Bad $r "Method unsupported '[lindex $header 0]'" 405] "Method Unsupported [lindex $header 0]"
		return -code break	;# signal error to caller
	    }
	}

	return $lines
    }

    # fcopy: our outbound fcopy has completed
    method fcopy {fd bytes next r written {error ""}} {
	variable replies
	variable socket

	Debug.httpd {[info coroutine] fcopy: $fd $bytes $written '$error'}
	set r [::Httpd timestamp $r fcopydone]
	Debug.stamp {[info coroutine] [my timing $r]}

	::watchdog stroke [self]

	catch {close $fd}	;# remove file descriptor
	variable files; dict unset files $fd

	if {[set gone [catch {chan eof $socket} eof]] || $eof} {
	    # detect socket closure ASAP in sending
	    Debug.httpd {[info coroutine] Lost connection on fcopy}
	    if {$error eq ""} {
		append error "eof on $socket in fcopy"
	    }
	}

	# if $bytes != $written or $error ne "", we have a problem
	variable outbuffer
	if {$gone || $eof || $bytes != $written || $error ne ""} {
	    if {$error eq ""} {
		set error "fcopy failed to send $bytes bytes, only sent $written."
	    }
	    Debug.error $error
	    my terminate "$error in fcopy"
	    return
	} elseif {[chan pending output $socket] <= $outbuffer} {
	    # only when the client has consumed our output do we
	    # restart reading input
	    Debug.httpdlow {[info coroutine] fcopy: restarting reader}
	    my Readable
	} else {
	    Debug.httpdlow {[info coroutine] fcopy: suspending reader [chan pending output $socket]}
	}

	# record that this request was satisfied
	variable unsatisfied
	dict unset unsatisfied $next	;# satisfied this

	# see if the writer needs service
	my Writable
    }

    # respond - to client with as many consecutive responses as it can consume
    method respond {} {
	variable sequence
	variable satisfied
	variable transaction
	variable socket
	variable outbuffer
	variable ostate
	variable unsatisfied

	if {[chan eof $socket] && ![dict size $unsatisfied]
	} {
	    # we have no more requests to satisfy and no more input will come
	    Debug.httpd {[info coroutine] closing as there's nothing pending}
	    set ostate CLOSED
	    my terminate "finally close in responder"
	    error "finally closed"
	}

	# shut down responder if there's nothing left to write
	# we expect there'll be another request soon
	variable replies
	if {![dict size $replies]} {
	    set ostate EMPTY
	    my Unwritable	;# no point in trying to write
	}

	# send all responses in sequence from the next expected to the last available
	Debug.httpd {[info coroutine] pending to send: ([dict keys $replies])}
	variable response
	variable outbuffer
	foreach next [lsort -integer [dict keys $replies]] {
	    if {[chan pending output $socket] > $outbuffer} {
		# the client hasn't consumed our output yet
		# stop communicating for 10mS then retry
		my Unwritable	;# stop writing
		my Unreadable	;# stop reading
		set ostate PENDING
		after 10 [list [info coroutine] respond] ;# restart in 10mS
		return 0
	    }

	    ::watchdog stroke [self]	;# tickle the watchdog

	    if {[set gone [catch {chan eof $socket} eof]] || $eof} {
		# detect socket closure ASAP in sending
		Debug.httpd {[info coroutine] Lost connection on transmit}
		set ostate EOF
		my terminate "eof on $socket"
		error "finally closed"
	    }

	    # ensure we don't send responses out of sequence
	    if {$next != $response} {
		# something's blocking the response pipeline
		# so we don't have a response for the next transaction.
		# we must therefore wait until all the preceding transactions
		# have something to send
		Debug.httpd {[info coroutine] no pending or $next doesn't follow $response}
		my Unwritable	;# no point in trying to write

		if {[chan pending output $socket] > $outbuffer} {
		    # the client hasn't consumed our output yet
		    # stop reading input until he does
		    my Unreadable
		    set ostate OOS_UNREADABLE
		} else {
		    # there's space for more output, so accept more input
		    my Readable
		    set ostate OOS_READABLE
		}

		return 0
	    }

	    set response [expr {1 + $next}]	;# move to next response
	    set ostate "SENDING $response"

	    # respond to the next transaction in trx order
	    # unpack and consume the reply from replies queue
	    # remove this response from the pending response structure
	    lassign [dict get $replies $next] req cache head content file close empty range
	    dict unset replies $next		;# consume next response
	    set req [::Httpd timestamp $req txstart]

	    # connection close after transmission required?
	    # NB: we only consider closing if all pending requests
	    # have been satisfied.
	    if {$close} {
		# inform client of intention to close
		Debug.httpdlow {close requested on $socket - sending header}
		append head "Connection: close" \r\n	;# send a close just in case
		# Once this header's been sent, we're committed to closing
	    }

	    # send headers - can be empty if proxying
            if {[string length $head]} {
                chan puts -nonewline $socket $head\r\n
                Debug.httpd {[info coroutine] SENT HEADER: $socket '[lindex [split $head \r] 0]' [string length $head] bytes} 4
                chan flush $socket	;# try to flush as early as possible
                Debug.httpdlow {[info coroutine] flushed $socket} 4
            }

	    set req [::Httpd timestamp $req txcontent]

	    # send the content/entity (if any)
	    # note: we must *not* send a trailing newline, as this
	    # screws up the content-length and confuses the client
	    # which doesn't then pick up the next response
	    # in the pipeline
	    if {!$empty} {
		if {$file ne ""} {
		    # send content of file descriptor using fcopy
		    set fd [open $file r]
		    variable files; dict set files $fd 1 ;# remember fd for closing later
		    set bytes [file size $file]

		    chan configure $socket -translation binary
		    chan configure $fd -translation binary
		    my Unreadable	;# stop reading input while fcopying
		    my Unwritable	;# stop writing while fcopying
		    ::watchdog grace [self] -1	;# stop the watchdog resetting the link

		    if {[llength $range]} {
                        # process file content as a range
			lassign $range from to
			chan seek $fd $from
			set bytes [expr {$to-$from+1}]
			Debug.httpd {[info coroutine] FCOPY RANGE: '$file' bytes $from-$to/$bytes} 8
			chan copy $fd $socket -command [list [info coroutine] fcopy $fd $bytes $next $req]
		    } else {
			# send entire file
			Debug.httpd {[info coroutine] FCOPY ENTITY: '$file'/$fd $bytes bytes} 8
			chan copy $fd $socket -command [list [info coroutine] fcopy $fd $bytes $next $req]
		    }
		    set ostate "FCOPY $response"
		    break	;# no more i/o on $socket until fcopy completion
		} elseif {[llength $range]} {
		    # send literal content range
		    lassign $range from to
		    chan puts -nonewline $socket [string range $content $from $to]
		    set req [::Httpd timestamp $req txrangesent]
		    Debug.stamp {[info coroutine] [my timing $req]}
		    Debug.httpd {[info coroutine] SENT RANGE: bytes $from-$to/[string length $content] bytes} 8
		} else {
                    # send literal content
		    chan puts -nonewline $socket $content	;# send the content
		    set req [::Httpd timestamp $req txsent]
		    Debug.stamp {[info coroutine] [my timing $req]}
		    Debug.httpd {[info coroutine] SENT ENTITY: [string length $content] bytes} 8
		}
	    }

	    if {$close} {
		dict unset unsatisfied $next
		my terminate "Closed on request"
		set ostate RCLOSE
		return
	    } elseif {[dict get $req -code] == 100} {
		# this was a continue ... we need to reschedule entity reading
		# keep the transaction unsatisfied
		set ostate CONTINUE
		Debug.httpd {[info coroutine] CONTINUE} 8
		dict set unsatisfied $next $req
		catch {dict unset satisfied $next}
		after 0 [list [dict get $req -send] entity $req ok]
	    } else {
		# this request is no longer unsatisfied
		dict unset unsatisfied $next
	    }
	}

	if {[chan pending output $socket] > $outbuffer} {
	    # the client hasn't consumed our output yet
	    # stop reading input until he does
	    my Unreadable
	} else {
	    # there's space for more output, so accept more input
	    my Readable
	}

	return 0
    }

    # send --
    #	queue up responses for delivery in-sequence
    #
    # Arguments:
    #
    # Side Effects:
    #	Send transaction responses to client
    #	Possibly close socket, possibly cache response

    method send {r {cache 1}} {
	Debug.httpd {[info coroutine] send: ([Httpd dump $r]) $cache [expr {[dict get? $r -ua_class] ni {browser unknown}}]}
	variable socket

	# process suspension at lowest level
	if {[dict exists $r -suspend]} {
	    set r [::Httpd timestamp $r suspended]
	    return	;# this reply has been suspended - we haven't got it yet
	    # so we simply return.  The lack of a response for the corresponding
	    # pipelined request has the effect of suspending the pipeline until
	    # the response has been delivered.
	    # requests will still be processed while the pipeline's suspended,
	    # but their responses will only be returned in strict and close order.
	}
	set r [::Httpd timestamp $r sent]

	# if this isn't a browser - do not cache!
	variable ua
	if {$ua && [dict get? $r -ua_class] ni {browser unknown}} {
	    Debug.httpd {not a browser - do not cache [dict get? $r -uri]}
	    set cache 0
	}

	variable satisfied
	variable unsatisfied
	Debug.httpd {send: [info coroutine] ([Httpd dump $r]) satisfied: ([dict keys $satisfied]) unsatisfied: ([dict keys $unsatisfied])}

	# send all pending responses, ensuring we don't send out of sequence
	# discard duplicate responses
	# fetch transaction from the response
	set trx [dict get? $r -transaction]
	if {$trx eq ""} {
	    # can't Send reply: no -transaction associated with request
	    Debug.error {Send discarded: no transaction ($r)}
	    my terminate "no transaction"
	    return
	} elseif {[dict exists $satisfied $trx]} {
	    # a duplicate response has been sent - discard this
	    # this could happen if a dispatcher sends a response,
	    # then gets an error.
	    Debug.error {Send discarded: duplicate ([Httpd dump $r]) - sent:([Httpd dump [dict get? $satisfied $trx]])}
	    return	;# duplicate response - just ignore
	} elseif {![dict exists $unsatisfied $trx]} {
	    # only send for unsatisfied requests
	    Debug.error {Send discarded: satisfied duplicate ([Httpd dump $r])}
	    return	;# duplicate response - just ignore
	}

	# wire-format the reply transaction - messy

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
	variable replies
	if {[dict get $r -code] != 100} {
	    set response [::Httpd Format $r $cache]
	    set response [::Httpd timestamp $response replied]

	    dict set replies $trx $response
	    dict set satisfied $trx 1	;# record satisfaction of transaction
	    Debug.httpd {[info coroutine] ADD TRANS: ([dict keys $replies])}

	    # global consequences - caching
	    lassign $response r cache
	    if {$cache} {
		# handle caching (under no circumstances cache bot replies)
		set r [Cache put $r]	;# cache it before it's sent
		set r [::Httpd timestamp $r cached]
	    } else {
		Debug.httpd {Do Not Cache put: ([Httpd dump $r]) cache:$cache}
	    }

	    variable outbuffer
	    if {[chan pending output $socket] > $outbuffer} {
		# the client hasn't consumed our output yet
		# stop reading input until he does
		set r [::Httpd timestamp $r unreadable]
		my Unreadable
	    } else {
		# there's space for more output, so accept more input
		my Readable
	    }
	} elseif {![my close? $r]} {
	    # special case 100-continue ...
	    set r [::Httpd Format $r $cache]
	    set r [::Httpd timestamp $r replied]

	    dict set replies $trx $r
	    Debug.httpd {[info coroutine] ADD CONTINUATION: ([dict keys $replies])}
	    # this is a continuation - we expect more
	    my Readable
	}

	# generate a log line
	variable log
	if {$log ne "" && [catch {
	    puts $log [Http clf $r]	;# generate a log line
	    chan flush $log
	} le leo]} {
	    Debug.error {log error: $le ($leo)}
	}

	my Writable
    }

    # proxy - send content as an undifferentiated blob
    method proxy {r content} {
        variable satisfied
        variable unsatisfied

	set trx [dict get? $r -transaction]
	if {$trx eq ""} {
	    # can't Send reply: no -transaction associated with request
	    Debug.error {Send discarded: no transaction ($r)}
	    my terminate "no transaction"
	    return
	} elseif {[dict exists $satisfied $trx]} {
	    # a duplicate response has been sent - discard this
	    # this could happen if a dispatcher sends a response,
	    # then gets an error.
	    Debug.error {Send discarded: duplicate ([Httpd dump $r]) - sent:([Httpd dump [dict get? $satisfied $trx]])}
	    return	;# duplicate response - just ignore
	} elseif {![dict exists $unsatisfied $trx]} {
	    # only send for unsatisfied requests
	    Debug.error {Send discarded: satisfied duplicate ([Httpd dump $r])}
	    return	;# duplicate response - just ignore
	}

        # make up a reply with the minimum to get it sent
        variable replies
        dict set replies $trx [list $r 0 "" $content "" 0 0 {}]
        dict set satisfied $trx 1	;# record satisfaction of transaction
    }

    # parse - convert a complete header to a request dict
    method Parse {lines} {
	variable istate PARSE
	upvar 1 r r
	set size 0
	foreach line $lines {
	    if {[string index $line 0] in {" " "\t"}} {
		# continuation line
		dict append r $key " [string trim $line]"
	    } else {
		set value [join [lassign [split $line ":"] key] ":"]
		set key [string tolower [string trim $key "- \t"]]

		if {[dict exists $r $key]} {
		    # duplicate header
		    dict append r $key ",$value"
		} else {
		    # new header
		    dict set r $key [string trim $value]
		    dict lappend r -clientheaders $key
		}
	    }

	    # limit size of each field
	    variable maxfield
	    if {$maxfield
		&& [string length [dict get $r $key]] > $maxfield
	    } {
		my handle [Http Bad $r "Illegal header: '[string range $line 0 20]...' [string length [$dict get $r $key]] is too long"] "Illegal Header - [string length [dict get $r $key]] is too long"
		return -code break
	    }
	}
    }

    # Protocol - analyse the received request
    method Protocol {} {
	variable istate PROTOCOL
	upvar 1 r r
	# get and test HTTP version
	dict set r -version [lindex [dict get $r -header] end]		;# HTTP version
	if {[string match HTTP/* [dict get $r -version]]} {
	    dict set r -version [lindex [split [dict get $r -version] /] 1]
	}

	# Send 505 for protocol != HTTP/1.0 or HTTP/1.1
	if {[dict get $r -version] ni {1.1 1.0}} {
	    set istate UNSUPPORTED
	    my handle [Http Bad $r "HTTP Version '[dict get $r -version]' not supported" 505] "Unsupported HTTP Version"
	    return -code break
	}

	# ua - analyse user-agent strings.
	variable ua
	if {$ua} {
	    dict set r -ua [UA parse [dict get? $r user-agent]]
	    dict set r -ua_class [UA classify [dict get? $r user-agent]]	;# classify client by UA
	    switch -- [dict get $r -ua_class] {
		blank {
		    # anonymous by user-agent
		    if {[dict get? $r -uri] ne "/robots.txt"} {
			set istate SPIDER
			my handle [Http NotImplemented $r "Possible Spider Service - set your User-Agent"] "Spider"
			return -code break
		    } else {
			# allow anonymous people to collect robots.txt
		    }
		}

		spammer {
		    # known spider user-agent
		    set istate SPAMMER
		    Block block [dict get $r -ipaddr] "spammer UA ([dict get? $r user-agent])"
		    my handle [Http NotImplemented $r "Spammer"] "Spammer"
		    return -code break
		}

		browser {
		    # let known browsers through
		}

		unknown {
		    #Debug.log {unknown UA: [dict get $r user-agent]}
		}

		default {
		    # dict set r -dynamic 1	;# make this dynamic
		}
	    }
	}

	Debug.httpd {[info coroutine] reader got request: ($r)}

	# get request URL
	# check URI length (per rfc2616 3.2.1
	# A server SHOULD return 414 (Requestuest-URI Too Long) status
	# if a URI is longer than the server can handle
	# (see section 10.4.15).)
        set uri [join [lrange [dict get $r -header] 1 end-1]]

	variable maxurilen
	if {$maxurilen && [string length $uri] > $maxurilen} {
	    # send a 414 back
	    set istate LONGURI
	    my handle [Http Bad $r "URI too long '$uri'" 414] "URI too long"
	    return -code break
	}

	# parse the URL - deal with the client's Host: if protocol requires it
	if {[dict exists $r host]} {
	    # client sent Host: field
	    if {[string match http*:* $uri]} {
                # absolute Host: field
		# rfc 5.2 1 - a host header field must be ignored
		# if request-line specified an absolute URL host/port
		set r [dict merge $r [Url parse $uri 1]]
	    } else {
		# no absolute URL was specified on the request-line
		# use the Host field to determine the host
                set port [dict get $r -port]	;# default port sent in by Listener
		lassign [split [dict get $r host] :] host port
                dict set r -host $host
                dict set r -port $port
		set r [dict merge $r [Url parsePath $uri 1]]
	    }
	} elseif {[dict get $r -version] > 1.0} {
	    set istate BADPROTOCOL
	    my handle [Http Bad $r "HTTP 1.1 required to send Host"] "No Host"
	    return -code break
	} else {
	    # HTTP 1.0 isn't required to send a Host field
	    # but we still need host info as provided by Listener
            set r [dict merge $r [Url parsePath $uri 1]]
            dict set r host [Url host $r]
	}
	dict set r -url [Url url $r]	;# normalize URL
        dict set r -uri [Url uri $r]

	# rfc2616 14.10:
	# A system receiving an HTTP/1.0 (or lower-version) message that
	# includes a Connection header MUST, for each connection-token
	# in this field, remove and ignore any header field(s) from the
	# message with the same name as the connection-token.
	if {[dict get $r -version] < 1.1 && [dict exists $r connection]} {
	    foreach token [split [dict get $r connection] ","] {
		catch {dict unset r [string trim $token]}
	    }
	    dict unset r connection
	}

	# completed request header decode - now dispatch on the URL
	Debug.httpd {[info coroutine] reader complete: [dict get $r -header] ([Httpd dump $r])}

	# rename fields whose names are the same in request/response
	foreach n {cache-control pragma} {
	    if {[dict exists $r $n]} {
		dict set r -$n [dict get $r $n]
		dict unset r $n
	    }
	}

	# remove 'netscape extension' length= from if-modified-since
	if {[dict exists $r if-modified-since]} {
	    dict set r if-modified-since [lindex [split [dict get $r if-modified-since] {;}] 0]
	}

	# trust x-forwarded-for if we get a forwarded request from
	# a local ip (presumably local ip forwarders are trustworthy)
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
	dict set r -forwards $forwards
	#dict set r -ipaddr [lindex $forwards 0]

	# filter out all X-* forms, move them to -x-* forms
	# so we don't re-send them in reply
	foreach x [dict keys $r x-*] {
	    dict set r -$x [dict get $r $x]
	    dict unset r $x
	}
    }

    # Entity - read entity from the wire
    method Entity {} {
	variable istate ENTITY
	upvar 1 r r
	set r [::Httpd timestamp $r entitystart]
	variable socket

	# rfc2616 4.3
	# The presence of a message-body in a request is signaled by the
	# inclusion of a Content-Length or Transfer-Encoding header field in
	# the request's headers.
	if {[dict exists $r transfer-encoding]} {
	    set te [dict get $r transfer-encoding]
	    Debug.entity {got transfer-encoding: $te}

	    # chunked 3.6.1, identity 3.6.2, gzip 3.5,
	    # compress 3.5, deflate 3.5
	    set tels {}
	    array set params {}

	    variable te_encodings
	    variable te_params
	    foreach tel [split $te ,] {
		set param [lassign [split $tel ";"] tel]
		set tel [string trim $tel]
		if {$tel ni $te_encodings} {
		    # can't handle a transfer encoded entity
		    # queue up error response (no caching)
		    Debug.log {Got a $tel transfer-encoding which we can't handle}
		    my send [Http NotImplemented $r "$tel transfer encoding"] 0
		    return	;# listen for new request

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
	    my send [Http Bad $r "Length Required" 411]
	    return
	}

        # determine the charset of any content
        set charset [join [lassign [split [dict get? $r content-type] \;] ctype] \;]
	if {[string match "charset=*" $charset]} {
	    set charset [string trim [lindex [split [string tolower $charset] =] 1]]
	} else {
	    set charset ""
	}
        switch -glob -- $ctype\;$charset {
            "application/*;" {
                set charset binary
            }
            "text/*;" {
                variable def_charset
                set charset $def_charset
            }
            "*;" {
                # no charset specified
                set charset binary
            }
            "*/*;*" {
                # both ctype and charset specified
                if {$charset ni [encoding names]} {
                    # send NotAcceptable?  But how?
                    my send [Http NotAcceptable $r]
                    return
                }
            }
        }

        dict set r -encoding $charset	;# record the encoding we've selected
        Debug.httpd {Char Encoding: $charset}

	# fetch the entity (if any)
	if {"chunked" in [dict get? $r -te]} {
	    # write chunked entity to disk
	    set istate CHUNKED
	    set chunksize 0x[get $socket FCHUNK]	;# how many bytes to read?
	    Debug.entity {[info coroutine] FCHUNK} 8

	    if {$chunksize ne "0x0"} {
		# create a temp file to contain entity, remember it in $r and files
		set entity [file tempfile entitypath]
		dict set r -entity $entity		;# entity fd
		variable files; dict set files $entity $entitypath

		# prepare output file for receiving chunks
		chan configure $entity -translation binary
		if {"gzip" in [dict get? $r -te]} {
		    Debug.entity {[info coroutine] FCHUNK is gzipped} 8
		    ::zlib push inflate $entity	;# inflate it on the run
		}

		# prepare the socket for fchunk
		my Unreadable	;# stop reading input while fcopying
		my Unwritable	;# stop writing while fcopying
		::watchdog grace [self] 120000	;# prevent the watchdog resetting the link

		# start the entity fcopy
		set istate FCHUNK
		chan configure $socket -translation binary -encoding $charset
		chan copy $socket $entity -size $chunksize -command [list [info coroutine] fchunk $r $raw $entity 0 $chunksize]
	    } else {
		# we had a 0-length chunk ... may as well let it fall through
		dict set r -entity ""
	    }
	    return	;# we loop around until there are more requests
	} elseif {[dict exists $r content-length]
		  && [dict get $r content-length]} {
            # straight 'entity follows header' with explicit length
	    set left [dict get $r content-length]
	    Debug.entity {content-length: $left}

	    # enforce server limits on Entity length
	    variable maxentity
	    if {$maxentity > 0 && $left > $maxentity} {
		# 413 "Request Entity Too Large"
		set istate OVERSIZED_ENTITY
		my handle [Http Bad $r "Request Entity Too Large" 413] "Entity Too Large"
		return -code break
	    }

            # decide whether to read to RAM or disk
	    variable todisk
	    if {$todisk > 0 && $left > $todisk} {
		# this entity is too large to be handled in memory,
		# write it to disk
		set istate ENTITY_TO_DISK
		Debug.entity {[info coroutine] FCIN: '$left' bytes} 8

		# create a temp file to contain entity,
                # remember it in $r and files
		set entity [file tempfile entitypath]
		dict set r -entity $entity
		variable files; dict set files $entity $entitypath

		# prepare entity file for receiving chunks
		chan configure $entity -translation {binary binary} -encoding binary
		if {"gzip" in [dict get? $r -te]} {
		    Debug.entity {[info coroutine] FCIN is gzipped} 8
		    ::zlib push inflate $entity	;# inflate it on the run
		}

		# prepare the socket for fcin
		my Unreadable	;# stop reading input while fcopying
		my Unwritable	;# stop writing while fcopying
		::watchdog grace [self] 120000	;# stop the watchdog resetting the link

		Debug.entity {[info coroutine] FCIN: starting with $left writing to '$entitypath'} 8

		# start the fcopy
		chan configure $socket -translation binary -encoding $charset
		chan copy $socket $entity -size $left -command [list [info coroutine] fcin $r $entity $left]
		return	;# we loop around until there are more requests
	    }

	    # load entity into memory
	    if {$left == 0} {
		dict set r -entity ""
		# the entity, length 0, is therefore already read
		# 14.13: Any Content-Length greater than
		# or equal to zero is a valid value.
	    } else {
		set entity ""
		chan configure $socket -translation {binary binary} -encoding $charset
		Debug.httpdlow {[info coroutine] getting entity of length ($left)}
		set chunk ""
		while {[string length $chunk] < $left
		       && [chan pending input $socket] != -1
		       && ![chan eof $socket]
		   } {
		    my Yield	;# wait for READ event
                    Debug.httpdlow {reading $socket [fconfigure $socket]}
		    append chunk [chan read $socket $left]
		}

		dict append r -entity $chunk
	    }
	    Debug.entity {memory entity of length: [string length [dict get $r -entity]]}
	}

	# reset socket to header config, having read the entity
	chan configure $socket -encoding binary -translation {crlf binary}
	set r [::Httpd timestamp $r entityend]

	# now we postprocess/decode the entity
	Debug.entity {entity read complete - '[dict get? $r -te]'}
	if {"gzip" in [dict get? $r -te]} {
	    dict set r -entity [::zlib inflate [dict get? $r -entity]]
	}
    }

    method process {r} {
	variable istate PROCESS
        variable unsatisfied
        variable satisfied

        if {[dict get? $r -method] eq "OPTIONS"
            && [dict exists $r access-control-request-method]} {
            # simplistic CORS
            Debug.httpd {CORS [dict get $r -transaction] / satisfied: ([dict keys $satisfied]) unsatisfied: ([dict keys $unsatisfied])}
            my send [Http CORS $r] 0
            Debug.httpd {CORS POST satisfied: ([dict keys $satisfied]) unsatisfied: ([dict keys $unsatisfied])}
            return
        }

	# check Cache for match
	if {[dict size [set cached [Cache check $r]]] > 0} {
	    # reply directly from cache
	    dict set unsatisfied [dict get $cached -transaction] $r
	    dict set cached -caching retrieved
	    dict set cached -sent [clock microseconds]

	    # clean up timestamp
	    dict set cached -time [dict get $r -time]
	    set cached [::Httpd timestamp $cached fromcache]

	    Debug.httpd {[info coroutine] sending cached [dict get $r -uri] ([Httpd dump $cached])}
	    set fail [catch {
		my send [dict merge $r $cached] 0
	    } result eo]

	    # clean up any entity file hanging about
	    if {[dict exists $r -entitypath]} {
		variable files; dict unset files [dict get $r -$entitypath]
		catch {close [dict get $r -entity]}
		# leave the temp file ... should we delete it here?
	    }

	    if {$fail} {
		Debug.error {FAILED write $result ($eo) IP [dict get $r -ipaddr] ([dict get? $r user-agent]) wanted [dict get $r -uri]}
		my terminate "closed while processing request $result"
	    }
	    return	;# we've sent the cached copy, we're done
	}

	if {[dict exists $r -entitypath]} {
	    set entfd [dict get $r -entity]
	}

	if {[dict exists $r etag]} {
	    # move requested etag aside, so domains can provide their own
	    dict set $r -etag [dict get $r etag]
	}

	dict set r -received [clock microseconds]
	variable istate DISPATCH
	catch {
	    set r1 [::Dispatcher pre $r]
	    set r1 [::Httpd timestamp $r1 dispatch]
	    ::Dispatcher do REQUEST $r1
	} rsp eo	;# process the request
	set istate POSTPROCESS

	# handle response code from processing request
	set done 0
	switch -- [dict get $eo -code] {
	    0 -
	    2 {
		# does application want to suspend?
		set rsp [::Httpd timestamp $rsp postprocess]
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
		Debug.error {[info coroutine] postprocess error: $rspp ($eo)} 1
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

	# clean up any entity file hanging about
	if {[info exists entfd]} {
	    variable files; dict unset files $entfd	;# don't need to clean up for us
	    catch {close $entfd}
	    # leave the temp file ... should we delete it here?
	}
    }

    method entity {r} {
	my Entity		;# process entity
	tailcall my process $r	;# now process the request
    }

    # associate - file descriptor with connection lifetime
    method associate {fd {with unknown}} {
	variable files
	dict set files $fd $with	;# police fd
    }

    # disassociate - file descriptor from connection lifetime
    method disassociate {fd} {
	variable files
	dict unset files $fd
    }

    # enthread - entangle connection with thread
    method entangle {resource} {
	variable resources
	dict set resources $resource 1
    }
    method detangle {resource} {
	variable resources
	dict unset resources $resource
    }

    # coro - the coroutine which processes incoming requests
    method coro {args} {
	Debug.httpd {create reader [info coroutine] - $args}

	my Readable	;# kick off the readable event

	dict with args {}	;# instantiate $args alist as corovars
	variable transaction 0	;# count of incoming requests
	variable socket
	variable files; dict set files $socket SOCKET	;# police socket
	variable threads {}	;# police threads
	variable unsatisfied	;# reader queues up unsatisfied requests
	variable proto; dict set proto -send [info coroutine]	;# remember coroutine as sender

	# check the incoming ip for blockage
	variable ipaddr
	if {[Block blocked? $ipaddr]} {
	    my handle [Http Forbidden {-reason blocked -ipaddr $ipaddr}] Forbidden	;# this will never start reading
	}

	::watchdog stroke [self]
	variable start
	variable istate RUNNING
	variable request
	while {[chan pending input $socket] >= 0} {
	    set r $proto	;# start with blank request
	    dict set r -transaction [incr transaction]
	    dict set r -time connected $start	;# when we got connected - for timestamping

	    # read the header and unpack the header line
	    # parse and merge header lines into request dict
	    my Parse [my Headers]
	    set request $r	;# record current request dict as $request corovar

	    my Protocol		;# process request protocol - manipulates $r
	    # (Protocol may break out of this loop to handle errors/spiders/etc.)

	    # record request as unsatisfied and in $request corovar
	    set request $r
	    dict set unsatisfied [dict get $r -transaction] $r

	    # intercept websockets request, process it
	    variable websockets
	    if {$websockets
		&& [string tolower [dict r.connection?]] eq "upgrade"
	    } {
		# initiate WebSockets connection
		my Unreadable; my Unwritable	;# turn off socket processing
		my destroy	;# destroy us
		tailcall [WebSockets create] handshake $r ;# hand over to WebSockets
	    }

	    # the client wants us to tell it to continue
	    # before reading the body.
	    # Do so, then proceed to process entity
	    if {0 && [dict get $r -version] >= 1.1
		&& [dict exists $r expect]
		&& [string match *100-continue* [string tolower [dict get $r expect]]]
	    } {
		set istate CONTINUE
		my send [Http Continue $r] 0	;# send a 100 Continue
	    } else {
		set istate ENTITY
		my entity $r		;# process entity
	    }
	}

	# reading is complete, but we may have more to send
	set istate CLOSING
	my respond
	while {1} {
	    my Yield
	}
	set istate CLOSED
    }

    # reader - main coroutine supervising connection
    method reader {args} {
	if {[set code [catch {my coro {*}$args} e eo]]} {
	    Debug.error {reader obj:[self] coro:[info coroutine] $code: $e ($eo)}
	} else {
	    Debug.httpdlow {reader obj:[self] coro:[info coroutine] terminated}
	}
	catch {my destroy}
    }

    # thread_response - turn a threaded processing response into
    # an Http response, and resume it.
    method thread_response {r thread type code rs eo} {
	Debug.httpdthread {thread_response: $code ($eo) ([Httpd dump $r]) -> ([Httpd dump $rs])}

	if {[set live [expr {$thread in [::thread::names]}]]} {
	    ::thread::release $thread	;# release the thread
	}

	if {$live && $type ne ""
	    && ![string match tid0x* $type]
	} {
	    # put idle thread back on available queue
	    dict lappend [info object namespace ::Httpd]::threads $type $thread
	}

	if {$code} {
	    set rs [Http ServerError $r $rs $eo]
	}
	return [Httpd Resume $rs]	;# resume request
    }

    # thread - run the given script in the given thread,
    # suspending while processing, resuming when complete
    method thread {thread type script rvar r args} {
	# collect thread args
	dict set r -thread [::thread::id]	;# remember the currently running thread
	set vars [list $rvar {*}[dict keys $args]]
	set vals [list $r {*}[dict values $args]]

	# if this is a one-shot script, we need termination script
	if {$type eq ""} {
	    set terminate "incr ::forever"	;# kick the one-shot out of event loop
	} else {
	    set terminate ""		;# nothing needs doing for other thread types
	}

	# generate script to send to child thread
	# child thread evaluation uses [::apply] to collect all results of evaluation of $script
	# Result is passed back to calling thread/object by means of [my thread_response]
	# $script return value is a response dict or error
	# thread_response is given the original request dict, response dict, error dict, code
	# and thread type, which determines the disposition of the running thread.
	set sscript [string map [list %S% [list $script] %V% [list $vars] %A% $vals %ME% [::thread::id] %R% [list $r] %O% [self] %TYPE% $type %TERM% $terminate] {
	    #puts stderr "Thread Running: [::thread::id]"
	    set code [catch {::apply [list %V% %S%] %A%} rs eo]
	    ::thread::send -async %ME% [list %O% thread_response %R% [::thread::id] "%TYPE%" $code $rs $eo]
	    %TERM%
	}]
	Debug.httpdthread {thread: $thread sscript: '$sscript'}

	::thread::send -async $thread $sscript	;# kick off the thread by passing it our script

	# Thread's now running - Suspend [self] pending response from child thread
	if {[dict exists $r -grace]} {
	    set grace [dict get $r -grace]	;# caller specified grace
	} else {
	    set grace -1	;# wait forever for the child to respond
	}

	return [Httpd Suspend $r $grace]
    }

    # access Httpd variables
    method var {var} {
	variable $var
	return [set $var]
    }

    destructor {
	Debug.httpd {Destroying [self]}
	::watchdog gone [self]	;# deregister from watchdog

	# destroy reader coroutine
	variable coro
	catch {rename $coro {}}

	variable socket
	::Httpd delSock $socket [self]
	set socket ""

	variable client
	if {[info exists client]} {
	    catch {$client del [self]}
	}

	# release any associated resources
	variable resources
	foreach {resource .} $resources {
	    catch {{*}$resource}
	}

	# close any open files
	variable files
	foreach {f name} $files {
	    catch {close $f}
	}

	variable live 0
	Debug.httpd {Destroyed [self]}
    }

    constructor {sock ip rport args} {
	Debug.httpd {Constructed [self] for socket $sock ip $ip rport $rport $args}
	variable socket $sock	;# remember the socket
	chan configure $sock -blocking 0 -buffering none -translation {crlf binary}

	variable ipaddr $ip
	variable maxfield 0	;# maximum field size
	variable maxentity 0	;# maximum entity size
	variable maxline 4096	;# maximum line length
	variable maxheaders 200	;# maximum number of headers in request
	variable ua 1		;# perform UA analysis
	variable server_id "Wub [package present Httpd]"
	variable maxurilen 0	;# maximum length of URI
	variable websockets 0	;# want to support websockets?
	variable log ""		;# log off by default
	variable todisk 0	;# don't save entities to disk
        variable def_charset [encoding system]
	variable {*}[Site var? Httpd]	;# allow .config file to modify defaults
	variable {*}$args

	variable istate INITIALIZING	;# input state
	variable ostate INITIALIZING	;# output state
	variable request {}		;# last request received

	variable te_encodings {chunked}

	variable replies {}	;# dict of replies pending
	variable requests {}	;# dict of requests unsatisfied
	variable satisfied {};# dict of requests satisfied
	variable unsatisfied {} ;# dict of requests unsatisfied
	variable response 1	;# which is the next response to send?
	variable sequence -1	;# which is the next response to queue?
	variable writing 0	;# we're not writing yet
	variable events {}	;# readable/writable
	variable files {}	;# files open to this connection
	variable resources {}	;# resources entangled with this connection
	variable client [::HttpdClient add $ip [self]]
	variable outbuffer 40960 ;# amount of output we are prepared to buffer
	variable start [clock microseconds]
	variable proto [list -sock $socket -cid [self] -ipaddr $ipaddr -rport $rport -received_seconds [clock seconds]]
	dict set proto -server_id $server_id
	set proto [dict merge $args $proto]
	Debug.httpd {proto: $proto}

	::Httpd addSock $sock [self]

	#variable coro [info object namespace [self]]::coro
	set corocnt [incr ::Httpd::coros::count]
	dict set proto -pipeline $corocnt	;# keep track of pipelines
	variable coro ::Httpd::coros::[string tolower [namespace tail [self]]]
	::coroutine $coro [self] reader
    }
}

namespace eval ::Httpd::coros {}

# format something to suspend this packet
oo::objdefine ::Httpd {
    # close? - should we close this connection?
    method close? {r} {
	# don't honour 1.0 keep-alives - why?
	set close [expr {[dict get $r -version] < 1.1}]
	Debug.httpdlow {version [dict get $r -version] implies close=$close}

	# handle 'connection: close' request from client
	foreach ct [split [dict get? $r connection] ,] {
	    if {[string tolower [string trim $ct]] eq "close"} {
		Debug.httpdlow {Tagging close at connection:close request}
		set close 1
		break	;# don't need to keep going
	    }
	}

	if {$close} {
	    # we're not accepting more input but defer actually closing the socket
	    # until all pending transmission's complete
	    catch {chan close $socket read}	;# close the read direction of socket
	    variable reading 0			;# we are no longer open for input
	}

	return $close
    }
    export close?

    # make GET/HEAD conditional
    # this will transform a request if there's a conditional which
    # applies to it.
    method Conditional {r} {
	set etag [dict get? $r etag]
	# Check if-none-match
	if {[Http any-match $r $etag]} {
	    # rfc2616 14.26 If-None-Match
	    # If any of the entity tags match the entity tag of the entity
	    # that would have been returned in the response to a similar
	    # GET request (without the If-None-Match header) on that
	    # resource, or if "*" is given and any current entity exists
	    # for that resource, then the server MUST NOT perform the
	    # requested method, unless required to do so because the
	    # resource's modification date fails to match that
	    # supplied in an If-Modified-Since header field in the request.
	    if {[string toupper [dict get $r -method]] in {"GET" "HEAD"}} {
		# if the request method was GET or HEAD, the server
		# SHOULD respond with a 304 (Not Modified) response, including
		# the cache-related header fields (particularly ETag) of one
		# of the entities that matched.
		Debug.cache {unmodified [dict get $r -uri]}
		#counter $cached -unmod	;# count unmod hits
		return [Http NotModified $r]
		# NB: the expires field is set in $r
	    } else {
		# For all other request methods, the server MUST respond with
		# a status of 412 (Precondition Failed).
		#return [Http PreconditionFailed $r]
	    }
	} elseif {![Http if-match $r $etag]} {
	    #return [Http PreconditionFailed $r]
	} elseif {![Http if-range $r $etag]} {
	    catch {dict unset r range}
	    # 14.27 If-Range
	    # If the entity tag given in the If-Range header matches the current
	    # entity tag for the entity, then the server SHOULD provide the
	    # specified sub-range of the entity using a 206 (Partial content)
	    # response. If the entity tag does not match, then the server SHOULD
	    # return the entire entity using a 200 (OK) response.
	}
	return $r
    }

    # CE - find and effect appropriate content encoding
    method CE {reply args} {
	# default to identity encoding
	set content [dict get $reply -content]
	variable ce_encodings	;# what encodings do we support?
	if {![info exists ce_encodings]} {
	    set ce_encodings {gzip}	;# support these char encodings
	}

	Debug.httpd {CE -encoding: $ce_encodings}
	set ct [dict get? $reply content-type]
	if {![dict exists $reply -gzip]
	    && ("gzip" in $ce_encodings)
	    && ![string match image/* $ct]
	    && ![string match binary/* $ct]
	} {
	    # prepend a minimal gzip file header:
	    # signature, deflate compression, no flags, mtime,
	    # xfl=0, os=3
	    set gztype [expr {[string match text/* [dict get $reply content-type]]?"text":"binary"}]
	    set gzip [::zlib gzip $content -header [list crc 0 time [clock seconds] type $gztype]]

	    dict set reply -gzip $gzip
            Debug.httpd {gzipping: [string length $gzip]/[string length $content]}
	}

	# choose content encoding - but not for MSIE
	if {[dict exists $reply accept-encoding]
	    && ![dict exists $reply content-encoding]
	} {
	    foreach en [split [dict get $reply accept-encoding] ","] {
		lassign [split $en ";"] en pref
		set en [string trim $en]
		if {$en in $ce_encodings} {
		    switch $en {
			"gzip" { # substitute the gzipped form
			    if {[dict exists $reply -gzip]} {
                                Debug.httpd {gzip acceptable}
				set content [dict get $reply -gzip]
				dict set reply content-encoding gzip
				#set reply [Http Vary $reply Accept-Encoding User-Agent]
				break
			    }
			}
		    }
		}
	    }
	}

	return [list $reply $content]
    }

    # Charset - ensure correctly encoded content in response
    method Charset {reply} {
	if {[dict exists $reply -chconverted]} {
	    return $reply	;# don't re-encode by charset
	}

	# handle charset for text/* types
	lassign [split [dict get? $reply content-type] {;}] ct
	if {[string match text/* $ct] || [string match */*xml $ct]} {
	    if {[dict exists $reply -charset]} {
		set charset [dict get $reply -charset]
	    } else {
		set charset [encoding system]	;# default charset (utf-8)
                dict set reply -charset $charset
	    }
            # ensure content is converted to correct charset,
            # flag conversion in response, to avoid double conversion
	    dict set reply -chconverted $charset
	    dict set reply content-type "$ct; charset=$charset"
	    dict set reply -content [encoding convertto $charset [dict get $reply -content]]
	}
	return $reply
    }

    # Format - format up a reply for sending.
    method Format {reply cache} {
	Debug.httpd {Format (cache: $cache) ([dict merge $reply {-content <ELIDED>}])}

	set file ""
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
	    dict set reply -code $code

	    # make reply conditional if requested
	    if {$code eq 200} {
		# non-OK responses aren't conditional (?)
		set reply [my Conditional $reply]
		set code [dict get $reply -code]
	    }

	    # Deal with content data by response type
	    set range {}	;# default no range
	    switch -glob -- $code {
		204 - 304 - 1* {
		    # 1xx (informational),
		    # 204 (no content),
		    # and 304 (not modified)
		    # responses MUST NOT include a message-body
		    Debug.httpdlow {Format: code is $code}
		    set reply [Http expunge $reply]	;#remove metadata from reply dict
		    set content ""
		    catch {dict unset reply -content}
		    catch {dict unset reply -file}
		    set cache 0	;# can't cache these
		    set empty 1	;# this is explicitly empty - no entity in reply
		}

		default {
		    # responses may include a message-body
		    set empty 0		;# assume non-empty
		    if {[dict exists $reply -content]} {
			# correctly charset-encode content
			set reply [my Charset $reply]

			#Debug.httpdlow {pre-CE content length [string length [dict get $reply -content]]}
			# also gzip content so cache can store that.
			# this is happening too soon ... what if there's a range?
			lassign [my CE $reply] reply content
			set file ""	;# this is not a file

			# ensure content-length is correct
			dict set reply content-length [string length $content]
			#Debug.httpdlow {post-CE content length [string length $content]}
		    } elseif {[dict exists $reply -file]} {
			# the app has returned the pathname of a file instead of content
			set file [dict get $reply -file]
			dict set reply content-length [file size $file]
			set content ""
		    } else {
			Debug.error {Format: contentless - response empty - no content in reply ($reply)}
			set content ""	;# there is no content
			set file ""	;# this is not a file
			set empty 1	;# it's empty
			dict set reply content-length 0
			#puts stderr "NOCACHE empty $code: $cache"
			set cache 0	;# can't cache no content
		    }

		    if {!$empty && [string match 2* $code] && $code ne 204} {
			# handle range for 200
			set ranges [dict get? $reply range]
			if {$ranges ne ""} {
			    Debug.httpd {ranges: $ranges}
			    set ranges [lindex [lassign [split $ranges =] unit] 0]
			    set ranges [split $ranges ,]
			    set ranges [lindex $ranges 0]	;# only handle one range
			    foreach rr $ranges {
				lassign [split $rr -] from to
				lassign [split $to] to
				set size [dict get $reply content-length]
				if {$from eq ""} {
				    set from [expr {$size-$to+1}]
				    set to $size
				} elseif {$to > $size || $to eq ""} {
				    set to [expr {$size-1}]
				}

				lappend range $from $to	;# remember range to send
			    }

			    # send appropriate content range and length fields
			    set code 206	;# partial content
			    dict set reply content-range "bytes $from-$to/$size"
			    dict set reply content-length [expr {$to-$from+1}]

			    Debug.httpd {range: [dict get $reply content-range] of length [dict get $reply content-length]}
			}
		    }
		}
	    }

	    # set the informational header error message
	    if {[dict exists $reply -error]} {
		set errmsg [dict get $reply -error]
	    }
	    if {![info exists errmsg] || ($errmsg eq "")} {
		set errmsg [Http ErrorMsg $code]
	    }

	    # format header
	    set header "HTTP/1.1 $code $errmsg\r\n"	;# note - needs prefix

	    # format up the headers
	    if {$code != 100} {
		append header "Date: [Http Now]" \r\n
		set si [dict get? $reply -server_id]
		if {$si eq ""} {
		    variable server_id
		    if {![info exists server_id]} {
			set server_id "Wub [package present Httpd]"
		    }
		    set si $server_id
		}
		append header "Server: $si" \r\n
	    }

	    # add in cookies already formatted up
	    foreach hdr {set-cookie} {
		if {[dict exists $reply set-cookie]} {
		    append header $hdr: " " [dict get $reply $hdr] \r\n
		}
	    }

	    # format up and send each cookie
	    if {[dict exists $reply -cookies]} {
		Debug.cookies {Http processing: [dict get $reply -cookies]}
		set c [dict get $reply -cookies]
		foreach cookie [Cookies format4server $c] {
		    Debug.cookies {Http set: '$cookie'}
		    append header "set-cookie: $cookie" \r\n
		}
	    } else {
		Debug.cookies {Http processing: no cookies}
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
	    if {$empty
                || [dict exists $reply content-range]
                || [dict get $reply content-length] == 0} {
		set cache 0	;# don't cache no content or range
	    } elseif {$cache} {
		# use -dynamic flag to avoid caching even if it was requested
		set cache [expr {![dict exists $reply -dynamic]
				 || ![dict get $reply -dynamic] }]

		if {$cache && [dict exists $reply cache-control]} {
		    set cacheable [split [dict get $reply cache-control] ,]
		    foreach directive $cacheable {
			set body [string trim [join [lassign [split $directive =] d] =]]
			set d [string tolower [string trim $d]]
			if {$d in {no-cache private}} {
			    set cache 0
			    break
			}
		    }
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

	    if {$code >= 500} {
		# Errors are completely dynamic - no caching!
		set cache 0
	    }

	    # strip http fields which don't have relevance in response
	    dict for {n v} $reply {
		set nl [string tolower $n]
		if {[string match x-* $nl]} {
		    append header "$n: $v" \r\n
		} elseif {$nl ni {server date}
			  && [info exists ::Http::headers($nl)]
			  && $::Http::headers($nl) ne "rq"
		      } {
		    append header "$n: $v" \r\n
		}
	    }
	} e eo]} {
	    if {![info exists code] || $code >= 500} {
		# Errors are completely dynamic - no caching!
		set cache 0
	    }

	    Debug.error {Sending Error: '$e' ($eo) Sending Error}
	} else {
	    Debug.httpdlow {Format: ($header)}
	}

	return [list $reply $cache $header $content $file [my close? $reply] $empty $range]
	# response ready for [response] to blast it out the socket:
	# reply - reply modified by Format
	# cache - cache the response?
	# header - fully serialized header
	# content - string content or ""
	# file - name of file to transmit or ""
	# close - has the client requested close? have we decided to close?
	# empty - is the reply empty of content/file?
	# range - what content ranges have been requested?
    }
    export Format

    # timestamp - record a uS timestamp in the r-dict
    method timestamp {r which {when 0}} {
	if {![Debug level? stamp]} {return $r}	;# only accumulate this stuff if Debug.stamp is on
	if {$when == 0} {
	    set when [clock microseconds]
	}

	if {![dict exists $r -time connected]} {
	    dict set r -time connected [clock microseconds]
	}
	dict set r -time $which [expr {$when - [dict get $r -time connected]}]
	return $r
    }
    export timestamp

    # dump - return a stripped request for printing
    method dump {req} {
        if {[string length [dict get? $req -content]] > 80} {
            dict set req -content [string range [dict get $req -content] 0 80]<ELIDED>
        }
	foreach f {-entity -gzip -headers} {
	    if {[dict exists $req $f]} {
		dict set req $f "<ELIDED [string length [dict get $req $f]]>"
	    }
	}

	return [regsub {([^[:print:]])} $req .]
    }
    export dump

    # Thread - suspend, and run this script in a background thread
    method Thread {script rvar r args} {
	if {[catch {package require Thread} e eo]} {
	    error "Httpd Thread requires package Thread, which is absent"
	}

	if {[llength $args]%2} {
	    # last arg is the thread to run.  What a complex API
	    set thread [lindex $args end]
	    set args [lrange $args 0 end-1]

	    if {[string match tid0x* [lindex $thread 0]]} {
		# passed in a TID - use that thread as an app-managed thread
		if {$thread ni [::thread::names]} {
		    error "Httpd Thread invocation of app-managed $thread, which doesn't exist."
		}
		set type $thread	;# this is an app-managed thread
	    } else {
		lassign $thread type init	;# get the thread type and creation code

		# pick a thread from idle queue of thread $type
		set cns [info object namespace ::Httpd]
		set thread ""

		if {![info exists ${cns}::threads]} {
		    set ${cns}::threads {}	;# set up empty idle threads by type dict
		}

		# loop through idle threads of this type
		while {1} {
		    set threads [dict get? [set ${cns}::threads] $type]
		    if {![llength $threads]} break	;# no idle threads of this type
		    dict set ${cns}::threads $type [lassign $threads thread]
		    if {$thread in [::thread::names]} break	;# found a running thread
		    # if defunct threads are in the thread type dict, then elide them
		    # and keep looping around looking for an idle thread
		}

		if {$thread eq ""} {
		    # no idle threads of this type.
		    # create a new thread with $init initialization and refcount 1
		    set thread [::thread::create -preserved [string map [list %AP% [list $::auto_path] %INIT% $init] {
			set auto_path %AP%
			#package require Thread
			package require Http
			proc ::terminate {} {incr ::forever}
			%INIT%	;# thread type specific initialization
			set ::forever 0; vwait ::forever	;# enter event loop, wait for requests
		    }]]
		}
	    }
	} else {
	    # default - one-shot anonymous thread
	    # create a new thread with a refcount of 0
	    set thread [::thread::create [string map [list %AP% [list $::auto_path]] {
		set auto_path %AP%
		#package require Thread
		package require Http
		set ::forever 0; vwait ::forever	;# enter event loop, wait for request
	    }]]
	    set type ""	;# a thread of type "" is not managed by ::Httpd::Thread
	}

	::thread::preserve $thread	;# ensure thread persists until we've run [my thread]

	# call the request's supervisor object to perform actual work
	return [[dict get $r -cid] thread $thread $type $script $rvar $r {*}$args]
    }

    export Thread

    # Suspend server-processing of this request
    method Suspend {r {grace -1}} {
	Debug.httpd {Suspending [Httpd dump $r]}
	dict set r -suspend $grace
	return $r
    }
    export Suspend

    # Resume this request
    method Resume {r} {
	Debug.httpd-resume {[Debug on httpd]}
	Debug.httpd {Resuming [Httpd dump $r]}
        # ask socket coro to send the response for us
	# inject the SEND event into the coro so Resume may be called from any
	# event, thread or coroutine
	catch {dict remove r -suspend}
	set r [Http timestamp $r resumed]
	set r [::Dispatcher post $r]
	Debug.httpd {Resuming to [dict get $r -send] send}
	set code [catch {{*}[dict get $r -send] send $r} e eo]
	if {$code != 0} {
	    Debug.error {Failed Resume '$e' ($eo)}
	} else {
	    Debug.httpd {Resume '$e' ($eo)}
	}
	Debug.httpd {Resume done - $code $e ($eo)}
	return [list $code $e $eo]
    }
    export Resume

    # active? is this request still active?
    method active? {r} {
	return [llength [info commands [dict get? $r -cid]]]
    }
    export active?

    # pipeline - return the -cid associated with this request
    # it will remain constant for each pipeline/connection
    method pipeline {r} {
	return [dict get? $r -cid]
    }
    export thread

    method addSock {sock what} {
	variable s2h
	if {![info exists s2h]} {
	    set s2h {}
	}
	if {[dict exists $s2h $sock]} {
	    error "addSock: $sock already exists [dict get $s2h $sock]"
	} else {
	    dict set s2h $sock $what
	}
    }
    export addSock

    method delSock {sock what} {
	variable s2h
	if {![dict exists $s2h $sock]} {
	    error "delSock: $sock doesn't exist"
	} elseif {[dict get $s2h $sock] ne $what} {
	    error "delSock: $sock was [dict get $s2h $sock], not $what"
	} else {
	    dict unset s2h $sock
	}
    }
    export delSock

    method s2h {{sock {}}} {
	variable s2h
	if {$sock eq ""} {
	    return $s2h
	} else {
	    return [dict get $s2h $sock]
	}
    }
    export s2h
}

proc ::checkObj {} {
    #Debug.log {Checking Objects}
    foreach o [info class instances ::Httpd] {
	if {[catch {info object namespace $o} ns]} {
	    Debug.error {$o is undead}
	} else {
	    if {![namespace exists $ns]} {
		Debug.error {$o namespace nonexistent}
	    }
	    if {![llength [info commands $o]]} {
		Debug.error {$o no command}
	    }
	}
    }
}

::watchdog every 1000 ::checkObj

# vim: ts=8:sw=4:noet
