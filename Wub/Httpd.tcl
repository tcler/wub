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

package require Cache 2.0

package provide Httpd 2.0

namespace eval Httpd {
    variable me [::thread::id]

    variable thread_path $auto_path	;# path for worker threads
    lappend thread_path ../Utilities/ ../extensions/ ../Utilities/zlib/

    # assembling header per connection
    variable sockets
    array set sockets {}

    # create a queue of free threads
    ::struct::queue threads

    # array of known threads
    variable worker
    array set worker {}
    variable max 257	;# maximum number of threads
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

    # initialisation script for worker threads
    variable script [::fileutil::cat [file join [file dirname [info script]] HttpdWorker.tcl]]

    # 
    proc socket {sock} {
	return $::sockets($sock)
    }

    # a worker thread has completely processed input, or has hit a socket error
    proc disconnect {thread error eo} {
	Debug.socket {Done: $thread '$error' - ($eo)}

	variable worker
	set socket $worker($thread);

	variable sockets
	if {$sockets($socket) ne $thread} {
	    puts stderr "Socket/Thread mismatch: $socket/$sockets($socket) - $thread/$worker($thread)"
	}

	unset sockets($socket)	;# we're done with this socket
	set worker($thread) {}	;# we're done with this thread

	if {[::thread::release $thread] != 1} {
	    puts stderr "release Thread $thread has been allocated $i times"
	}
	threads put $thread	;# we're done with this thread

	catch {Backend disconnect $socket} ;# inform backend of disconnection

	return $socket
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
    proc send {request} {
	variable sockets
	set tid $sockets([dict get $request -sock])
	::thread::send -async $tid [list send $request]
    }

    # got - worker thread has parsed one request
    proc got {tid request} {
	Debug.http {got: $request} 1
	set sock [dict get $request -sock]

	# dict set request -Query [Query parse $request]	;# parse the query?
	# Cookie processing for Session
	# Session handling
	# check Cache for match
	set cached [Cache check $request]
	if {[dict size $cached] > 0} {
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

    # Exhausted - method called by Listener to report server exhaustion
    proc Exhausted {sock {eo {}} {retry 200}} {
	puts $sock "HTTP/1.1 503 Socket Exhaustion" \r\n
	puts $sock "Date: [Http Now]" \r\n
	puts $sock "Server: Wub 1.0" \r\n
	puts $sock "Connection: Close" \r\n
	puts $sock "Retry-After: $retry" \r\n
	puts $sock \r\n
	close $sock
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
	    set s "set ::auto_path [list $thread_path] \n set ::thread::parent $me \n $script"
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

    # get - get a thread
    proc get {} {
	Debug.socket {get thread for reading}

	# grab a free thread
	if {[catch {threads get} thread]} {
	    mkthreads
	    set thread [threads get]	;# try again - propagate error
	}

	if {[::thread::preserve $thread] != 2} {
	    error "Thread $thread has been allocated $i times"
	}

	return [list Httpd threaded $thread]
    }

    # transfer - transfer control to a worker thread
    proc transfer {tid sock request vars} {
	if {[catch {
	    Debug.socket "Transferring $sock to $tid"
	    ::thread::transfer $tid $sock
	    ::thread::send -async $tid [list connect $request $vars $sock]
	} result eo]} {
	    puts stderr "Transfer Error: $result ($eo)"
	} else {
	    Debug.socket {Transferred: $result $eo}
	}
    }

    variable dispatch ""

    # threaded - cmd prefix for connecting Listener to thread
    proc threaded {tid connect args} {
	variable sockets
	variable worker

	Debug.socket {Connecting $tid $args}
	#array set argv [list -code 200 content-type text/html]
	array set argv {}

	set config {}
	foreach {n v} $args {
	    set argv($n) $v
	    # reflect the configuration args in the thread's global
	    lappend config [string range $n 1 end] $v
	}
	lappend config host [$argv(-listener) cget -host]
	lappend config port [$argv(-listener) cget -port]
	lappend config server [$argv(-listener) cget -server]

	set sock $argv(-sock)
	if {[info exists sockets($sock)]} {
	    puts stderr "Httpd RACE: sockets $sock already exists $sockets($sock)"
	} else {
	    set sockets($sock) $tid	;# bi-associate socket and thread
	}
	set worker($tid) $sock

	# the socket must stay in non-block binary binary-encoding mode
	#chan configure $sock -blocking 0 -translation {binary binary} -encoding binary
	chan configure $sock -blocking 0 -translation {binary binary} -encoding binary

	variable dispatch
	if {$dispatch ne ""} {
	    set argv(-dispatch) $dispatch
	}
	set argv(-version) 1.1

	after idle [list ::Httpd::transfer $tid $sock [array get argv] $config] ;# arrange for socket/task connection
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    package require Stdin
    package require Listener
    package require Debug

    interp bgerror {} bgerror
    proc bgerror {args} {
	puts stderr "Main: $args"
    }

    Debug off socket 10
    Debug on http 2
    Debug on cache 10
    Debug on dispatch 10

    set listener [Listener %AUTO% -port 8080 -sockets Httpd -httpd {-dispatch "puts"}]
    set forever 0
    vwait forever
}
