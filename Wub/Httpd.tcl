# Httpd - near HTTP/1.1 protocol thread supervisor.
#
# This is the front-end top-level thread supervisor for
# a collection of HttpdWorker threads.

# fast read starts a series of tasks to cope with each listener.
if {[info exists argv0] && ($argv0 eq [info script])} {
    # test Httpd
    puts stderr "Httpd test"
    lappend auto_path [pwd] ../Utilities/ ../extensions/ ../Utilities/zlib/
    package require Http
}

package require Thread
package require Query
package require struct::queue
package require fileutil
package require Debug
#package require Access

package require Cache 2.0
package require Honeypot
package require Html

package provide Httpd 2.0

proc bgerror {args} {
    #puts stderr "bgERROR: $args"
    Debug.error {bgerror: $args}
}
interp bgerror {} bgerror

proc pest {req} {return 0}
catch {source [file join [file dirname [info script]] pest.tcl]}

namespace eval Httpd {
    variable me [::thread::id]
    variable rqCallOut {}
    variable thread_path $auto_path	;# path for worker threads
    lappend thread_path ../Utilities/ ../extensions/ ../Utilities/zlib/

    # assembling header per connection
    variable sockets
    array set sockets {}

    # activity array - packet activity per socket
    # log the last packet parse time per socket.
    variable activity
    array set activity {}
    variable actlog {}
    variable actsize 0

    # create a queue of free threads
    ::struct::queue threads

    # array of known threads
    variable worker
    array set worker {}
    variable max 20	;# maximum number of threads
    variable incr 5	;# number of threads to add on exhaustion

    # evalfor - evaluate args in thread for given $sock
    # (utility/debugging function)
    proc evalfor {sock args} {
	return [thread::send ::sockets($sock) $args]
    }

    # destroy - destroy the Httpd protocol stack
    proc destroy {} {
	threads destroy	;# destroy thread queue

	# release all known sockets
	variable sockets
	foreach {fd state} [array names sockets] {
	    # release each socket
	    catch {chan close $fd}
	}

	# synchronously destroy worker threads
	variable worker
	foreach thread [array names worker] {
	    ::thread::release $thread
	}
    }

    proc pool {cmd} {
	variable worker
	foreach tid [array names worker] {
	    ::thread::send -async $tid $cmd
	}
    }

    # initialisation script for worker threads
    variable script [::fileutil::cat [file join [file dirname [info script]] HttpdWorker.tcl]]

    # 
    proc socket {sock} {
	return $::sockets($sock)
    }

    proc state {} {
	package require Url
	variable activity; array set A [array get activity]
	variable sockets; array set S [array get sockets]
	variable worker; array set W [array get worker]
	variable sock2IP; array set S2I [array get sock2IP]
	
	set result {}
	foreach {sock tid} [array get S] {
	    # get backend thread if possible
	    if {[catch {Backend s2be $sock} be]} {
		set be ""
	    }

	    # socket to IP
	    if {[info exists S2I($sock)]} {
		set ip $S2I($sock)
	    } else {
		set ip ""
	    }

	    #if {![info exists W($tid)]} continue ;# race workaround

	    # check worker/socket
	    if {$W($tid) ne $sock} {
		set w $W($tid)
	    } else {
		set w ""
		unset W($tid)
	    }

	    if {[info exists A($sock)]} {
		set time [expr {[lindex $A($sock) 1 0] / 1000000}]
		set start [list [clock format $time -format {%d/%m/%Y}] [clock format $time -format {%T}]]

		set time [expr {[lindex $A($sock) end 0] / 1000000}]
		set end [list [clock format $time -format {%d/%m/%Y}] [clock format $time -format {%T}]]

		catch {unset urls}; array set urls {}
		set log {}
		foreach {n v} $A($sock) {
		    if {$n eq "parsed"} {
			set url [Url parse [lindex $v 2]]
			incr urls([dict get $url -path]) 1
			lappend log [dict get $url -path]
		    }
		}
		#set log [array names urls]
	    } else {
		set start ""; set end ""; set log ""
	    }
	    lappend result [list $sock $tid $be $ip $w $start $end $log]
	}
	return $result
    }

    proc dump {} {
	variable me
	set result "Httpd server running in $me\n"
	variable sockets; append result "sockets: [array get sockets]" \n
	variable worker; append result "worker threads: [array get worker]"
	append result " ([threads size] in idle queue,"
	variable max; variable incr; append result " [array size worker] in total, up to $max, increments of $incr)" \n
	variable connbyIP; append result "connbyIP: [array get connbyIP]" \n
	variable sock2IP; append result "sock2IP: [array get sock2IP]" \n
	variable activity; append result "activity: [array get activity]" \n
	variable dispatch; append result "dispatch: $dispatch" \n
	variable ignore;
	if {$ignore} {
	    variable quiescent
	    append result "Going Idle to perform '$quiescent'"
	}
	return $result
    }

    # a worker thread has completely processed input, or has hit a socket error
    proc disconnect {thread sock error {eo ""}} {
	Debug.socket {Done: $thread $sock '$error' - ($eo)}

	variable worker
	variable sockets

	if {$sock ne $worker($thread)} {
	    # it's already been closed and the socket reassigned,
	    # but the worker thread has just noticed
	    Debug.error {COLLISION: $thread socket '$worker($thread)' != '$sock'}
	    if {$worker($thread) eq ""} {
		return ""	;# this has been thoroughly expunged
	    }
	} elseif {$sockets($sock) eq $thread} {
	    # this is a genuine disconnection by sock->thread && thread->sock
	    # we undo the socket->ip and socket->thread mappings
	    variable connbyIP; variable sock2IP;
	    incr connbyIP($sock2IP($sock)) -1
	    unset sockets($sock)	;# we're done with this socket	

	    # log the disconnection
	    variable activity; variable actlog; variable actsize
	    lappend activity($sock) disconnect [clock microseconds]
	    if {$actsize > 0} {
		lappend actlog [list $sock {*}$activity($sock)]
		if {[llength $actlog] > $actsize} {
		    set actlog [lrange $actlog 10 end]
		}
	    }
	    Debug.activity {$activity($sock)}
	    unset activity($sock)
	} else {
	    # thread->sock is right, sock->thread is not
	    Debug.error {Socket/Thread mismatch '$sock': $sockets($sock) - $thread/$worker($thread)}
	}

	set worker($thread) {}	;# we're done with this thread
	set i [::thread::release $thread]
	if {$i != 1} {
	    Debug.error {release Thread $thread has been allocated $i times}
	}
	threads put $thread	;# we're done with this thread

	catch {Backend disconnect $sock} ;# inform backend of disconnection

	# perform quiescent callback if we're idle
	variable ignore
	if {$ignore && [array size sockets] == 0} {
	    # we're quiescent - perform after idle command
	    variable quiescent
	    Debug.log {Idle task: $quiescent}
	    if {$quiescent ne ""} {
		uplevel #0 $quiescent
		set quiescent ""
	    }
	}

	return $sock
    }

    # _reply - format up a reply if needed
    # (debugging/utility proc)
    proc _reply {req {content ""}} {
	if {[dict exists $req -content]} {
	    return $req
	}

	if {$content eq ""} {
	    set body ""
	    if {1} {
		append body "<table border='1' width='80%'>" \n
		append body <tr> <th> metadata </th> </tr> \n
		dict for {n v} $req {
		    if {[string match -* $n]} {
			append body <tr> <td> $n </td> <td> $v </td> </tr> \n
		    }
		}
		append body </table> \n
		
		append body "<table border='1' width='80%'>" \n
		append body <tr> <th> "HTTP field" </th> </tr> \n
		dict for {n v} $req {
		    if {![string match -* $n]} {
			append body <tr> <td> $n </td> <td> $v </td> </tr> \n
		    }
		}
		append body </table> \n
	    }

	    if {![dict exists $req -error]} {
		dict set req -error Success
	    }
	    if {![dict exists $req -code]} {
		dict set req -code 200
	    }

	    set title [dict get $req -error]
	    set content "
			<html>
			<head>
			<title>$title</title>
			</head>
			<body>
			<form method='post' action='/junk' enctype='multipart/form-data'>
			<input id='url2' type='file' value='' name='url2' />
			<input type='hidden' name='type' value='file' />
			<input value='Submit' name='submitForm' type='submit' />
			<input value='Reset' name='resetForm' type='submit' />
			</form>
			<h1>$title</h1>
			$body
			</body>
			</html>
		"
	}

	dict set req -content $content
	dict set req content-type "text/html"
	return $req
    }

    # send - send a request to its associated thread
    proc send {request {cacheit 1}} {
	variable sockets
	set tid $sockets([dict get $request -sock])
	::thread::send -async $tid [list send $request]
    }

    # got - worker thread has parsed one request
    # now the dispatcher process (this one) decides how to process
    # the request.
    # By default, it invokes the -dispatch script on the request.
    proc got {tid request} {
	Debug.http {got: $request} 1
	set sock [dict get $request -sock]

	variable activity;

	# check the incoming ip for blockage
	if {[blocked? [Dict get? $request -ipaddr]]} {
	    dict set request connection close
	    ::thread::send -async $tid [list send [Http NotFound $request]]
	    #::thread::send -async $tid [list disconnect "Blocked"]
	    lappend activity($sock) parsed [list [clock microseconds] *[Dict get? $request -ipaddr] [Dict get? $request -url]]
	    return
	}

	# check the incoming ip for bot detection
	set request [Honeypot bot? $request]

	if {[dict exists $request -bot]} {
	    lappend activity($sock) parsed [list [clock microseconds] @[Dict get? $request -ipaddr] [Dict get? $request -url]]
	} else {
	    lappend activity($sock) parsed [list [clock microseconds] [Dict get? $request -ipaddr] [Dict get? $request -url]]
	}

	# dict set request -Query [Query parse $request] ;# parse the query?
	# Cookie processing for Session
	# Session handling

	# provide for a request callout - called with each parsed request
	variable rqCallOut
	if {[llength $rqCallOut] != 0} {
	    catch {set request [{*}$rqCallOut $request]}
	}

	# check Cache for match
	if {![pest $request]
	    && [dict size [set cached [Cache check $request]]] > 0
	} {
	    # reply from cache
	    dict set cached -transaction [dict get $request -transaction]
	    dict set cached -generation [dict get $request -generation]
	    ::thread::send -async $tid [list send $cached 0]
	} else {
	    # dispatch for content
	    if {[dict exists $request -dispatch]} {
		{*}[subst [dict get $request -dispatch]] $request
	    } else {
		# just send the reply as we have it
		::thread::send -async $tid [list send $request]
	    }
	}
    }

    # NotFound - generic 'go away bot' page
    proc NotFound  {sock {eo {}}} {
	Debug.socket {Exhausted $sock $eo $retry}
	puts $sock "HTTP/1.1 404 Bot Be Gone\r"
	puts $sock "Date: [Http Now]\r"
	puts $sock "Server: $::server_id\r"
	puts $sock "Connection: Close\r"
	puts $sock "Content-Length: 0\r"
	puts $sock \r
    }

    # Exhausted - method called by Listener to report server exhaustion
    proc Exhausted {sock {eo {}} {retry 20}} {
	Debug.socket {Exhausted $sock $eo $retry}
	puts $sock "HTTP/1.1 503 Socket Exhaustion\r"
	puts $sock "Date: [Http Now]\r"
	puts $sock "Server: $::server_id\r"
	puts $sock "Connection: Close\r"
	puts $sock "Retry-After: $retry\r"
	puts $sock "Content-Length: 0\r"
	puts $sock \r
    }

    # mkthreads - make and enqueue $incr new threads,
    # up to a limit of $max
    proc mkthreads {} {
	variable max
	if {[array size threads] > $max} {
	    error "Thread exhaustion - max $max threads already allocated"
	} else {
	    # create $incr new threads, add them to queue
	    variable incr
	    variable script
	    variable me
	    variable thread_path
	    variable worker
	    set s "set ::auto_path [list $thread_path]
		set ::thread::parent $me
		array set ::env [list [array get ::env]]
		$script"
	    for {set i 0} {$i < $incr} {incr i} {
		set new [::thread::create $s]
		if {[::thread::preserve $new] != 1} {
		    error "Thread $thread has been allocated $i times"
		}

		set worker($new) {}
		threads put $new
	    }
	}
    }
    mkthreads	;# construct initial thread pool

    proc inactive {{time 60}} {
	set time [expr {$time * 1000000}]
	set result {}
	set now [clock microseconds]

	variable activity
	foreach {s v} [array get activity] {
	    set thread [lindex $v 1 2]
	    set start [lindex $v 1 0]
	    set last [lindex $v end 0]
	    if {($now - $last) > $time} {
		lappend result $thread
	    }
	}
	return $result
    }

    # act2list - make a nested list from an activity list
    proc act2list {act} {
	set headers {date start thread age ipaddr connected transfer parsed ripped disconnect errors}
	set result [list $headers]
	foreach a [lsort -index 2 -dictionary $act] {
	    set a [lassign $a name -> start]
	    lassign $start start ipaddr thread

	    catch {unset vals}
	    set vals(name) $name
	    set vals(ipaddr) $ipaddr
	    set vals(thread) $thread
	    set s [expr {$start / 1000000}]
	    set vals(date) [clock format $s -format {%d/%m/%Y}]
	    set vals(start) [clock format $s -format {%T}]
	    set last [clock microseconds]
	    foreach {n v} $a {
		switch $n {
		    parsed {
			#lappend vals(urls) [lindex $v 2]
			#set vals(ipaddr) [lindex $v 1]
		    }
		    transfer {
			if {[llength $v] > 1} {
			    lappend vals(errors) [lrange $v 1 end]
			}
		    }
		}
		lappend vals($n) [expr {(([lindex $v 0] - $start) / 1000)/1000.0}]s
		set last [lindex $v 0]
	    }
	    set vals(age) [expr {(([clock microseconds] - $last)/1000)/1000.0}]s

	    set row {}
	    foreach n $headers {
		if {[info exists vals($n)]} {
		    if {$n eq "parsed"} {
			lappend row "[llength $vals($n)]: [lindex $vals($n) 0]-[lindex $vals($n) end]"
		    } else {
			lappend row [armour $vals($n)]
		    }
		} else {
		    lappend row {}
		}
	    }
	    lappend result $row
	}
	return $result
    }

    proc activity_current {} {
	variable activity
	set act {}
	foreach {n v} [array get activity] {
	    lappend act [list $n {*}$v]
	}
	return [act2list $act]
    }

    proc activity_log {} {
	variable actlog
	return [act2list $actlog]
    }

    proc activity_clear {} {
	variable actlog
	set actlog {}
    }

    # junk - read and discard input from a blocked ipaddress
    proc junk {sock args} {
	Debug.block {BLOCKED: $sock [fconfigure $sock -peername]} 3
	close $sock
    }

    # blocked - this ip address is blocked
    # play around with it a little
    proc blocked {cmd args} {
	set sock [dict get $args -sock]
	chan configure $sock -blocking 0 -translation {binary binary} -encoding binary
	chan event $sock readable [list ::Httpd::junk $sock];# resume reading
    }

    variable blocked
    array set blocked {}
    proc block {ipaddr {reason ""}} {
	variable blocked
	set blocked($ipaddr) [list [clock seconds] $reason]
	Debug.block {BLOCKING: $ipaddr $reason}
    }

    proc blocked? {ipaddr} {
	variable blocked
	return [info exists blocked($ipaddr)]
    }

    # exhaustion control
    variable max_conn 10
    variable connbyIP; array set connbyIP {}
    variable sock2IP; array set sock2IP {}

    # get - get a thread
    # error if some problem arises, causing Listener
    # to report 'busy' to client.
    proc get {sock ipaddr rport} {
	Debug.socket {get thread for reading}

	# check blocked list
	variable blocked
	if {[blocked? $ipaddr]} {
	    return [list Httpd blocked]
	}

	# remember client IP for socket
	variable sock2IP
	variable connbyIP
	if {[info exists sock2IP($sock)] && $sock2IP($sock) != $ipaddr} {
	    # the socket's closed and been reused
	    # we have to reflect that
	    Debug.socket {socket $sock reused - $sock2IP($sock)}
	    #incr connbyIP($sock2IP($sock)) -1 ;# assume it's already accounted
	}
	set sock2IP($sock) $ipaddr

	# ensure that client is not spamming us.
	variable max_conn
	if {$ipaddr ne "127.0.0.1"
	    && [incr connbyIP($ipaddr)] > $max_conn
	} {
	    # sadly we can't do this if we're reverse-proxied
	    Debug.log {Too many connections for $ipaddr}
	    incr connbyIP($ipaddr) -1
	    error "Too many connections for $ipaddr - no more than $max_conn"
	}
	Debug.socket {$connbyIP($ipaddr) connections for $ipaddr}

	# grab a free thread
	if {[catch {threads get} thread]} {
	    mkthreads
	    set thread [threads get]	;# try again - propagate error
	}

	if {[::thread::preserve $thread] != 2} {
	    error "Thread $thread has been allocated $i times"
	}

	variable activity; variable actlog; variable actsize
	if {[info exists activity($sock)]} {
	    lappend activity($sock) ripped [clock microseconds]
	    if {$actsize > 0} {
		lappend actlog [list $socket {*}$activity($socket)]
		if {[llength $actlog] > $actsize} {
		    set actlog [lrange $actlog 10 end]
		}
	    }
	    Debug.activity {$activity($sock)}
	    unset activity($sock)
	}
	lappend activity($sock) allocated [list [clock microseconds] $ipaddr $thread]

	return [list Httpd threaded $thread]
    }

    # transfer - transfer control to a worker thread
    proc transfer {tid sock request vars} {
	if {[catch {
	    Debug.socket "Transferring $sock to $tid"
	    ::thread::transfer $tid $sock
	    ::thread::send -async $tid [list connect $request $vars $sock]
	} result eo]} {
	    Debug.error {Transfer Error: $result ($eo)}
	    variable activity; lappend activity($sock) transfer [list [clock microseconds] $result $eo]
	} else {
	    Debug.socket {Transferred: $result $eo}
	    variable activity; lappend activity($sock) transfer [clock microseconds]
	}
    }

    variable ignore 0
    variable quiescent ""

    # go_idle - stop accepting new connections, go idle
    # when idle, evaluate the script in variable quiescent
    proc go_idle {{then ""}} {
	variable quiescent $then
	variable ignore 1	;# no more connections

	variable sockets
	if {[array size sockets] == 0} {
	    uplevel #0 $quiescent
	    set quiescent ""
	}
    }

    # go_live - start accepting new connections again
    proc go_live {} {
	variable ignore 0
    }

    variable dispatch ""
    variable server_port ;# server's port (if different from Listener's)

    # threaded - cmd prefix for connecting Listener to thread
    proc threaded {tid connect args} {
	variable sockets
	variable worker

	set listener [dict get $args -listener]
	Debug.socket {Connecting $tid $args}

	# the socket must stay in non-block binary binary-encoding mode
	set sock [dict get $args -socket]
	chan configure $sock -blocking 0 -translation {binary binary} -encoding binary

	# clean up after sockets which have closed before a thread realises
	if {[info exists sockets($sock)]} {
	    # this can happen if the remote's closed the socket
	    # and a new connection has arrived on the same socket.
	    set otid $sockets($sock)

	}

	# connect the socket with its new thread
	set sockets($sock) $tid	;# bi-associate socket and thread
	set worker($tid) $sock

	if {[info exists otid]} {
	    # call disconnect on old socket owner.
	    # subsequent disconnects will notice this has occurred
	    # and abort gracefully.  Still some race potential.
	    # argh - what happens if the thread already knows?
	    thread::send $otid {disconnect forced}
	}

	variable ignore
	if {$ignore} {
	    # we're shutting down, so ignore new requests
	    Exhausted $sock
	    flush $sock
	    close $sock
	    return
	}

	variable activity; lappend activity($sock) connected [clock microseconds]

	set config {}
	foreach {n v} $args {
	    # reflect the configuration args in the thread's global
	    lappend config [string range $n 1 end] $v
	}

	lappend config host [$listener cget -host]
	lappend config server [$listener cget -server]

	# get port on which connection arrived
	# this may differ from Listener's port if reverse proxying
	# or transparent ip-level forwarding is performed
	variable server_port
	if {[info exists server_port]} {
	    # use defined server port
	    lappend config port $server_port
	} else {
	    # use listener's port
	    lappend config port [$listener cget -port]
	}
	lappend config server_id $::server_id

	variable dispatch
	if {$dispatch ne ""} {
	    dict set args -dispatch $dispatch
	}
	dict set args -version 1.1	;# HTTP/1.1

	after idle [list ::Httpd::transfer $tid $sock $args $config] ;# arrange for socket/task connection
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
    Debug off dispatch 10

    set listener [Listener %AUTO% -port 8080 -sockets Httpd -httpd {-dispatch "puts"}]
    set forever 0
    vwait forever
}
