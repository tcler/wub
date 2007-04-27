package require Thread
package require struct::queue
package require fileutil
package require Debug; Debug off backend

package provide Backend 1.0

namespace eval Backend {
    variable me [::thread::id]

    variable thread_path $::auto_path	;# path for worker threads
    lappend thread_path ../Utilities/ ../extensions/ ../Utilities/zlib/

    variable sock2tid; array set sock2tid {}
    variable session2tid; array set session2tid {}

    # create a queue of free threads
    ::struct::queue threads

    # array of known threads
    variable worker
    array set worker {}
    variable max 257	;# maximum number of threads
    variable incr 20	;# number of threads to add on exhaustion

    # a script to get a thread going
    variable script ""

    proc destroy {} {
	threads destroy	;# destroy thread queue

	variable sock2tid
	foreach {fd state} [array names sock2tid] {
	    # release each socket
	    catch {chan close $fd}
	}

	# synchronously destroy worker threads
	variable worker
	foreach thread [array names worker] {
	    ::thread::release $thread
	}
    }

    # make and enqueue $incr new threads, up to a limit of $max
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
	    variable config
	    set s "set ::auto_path [list $thread_path] \n set ::thread::parent $me \n array set ::config [list [array get config]] \n $script"

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

    # process incoming
    proc incoming {req} {
	Debug.backend {get thread for work}

	# look for a thread tied to this session
	if {[dict exists $req -session_id]} {
	    # do we have a session tied thread?
	    set session [dict get $req -session_id]
	    if {[info exists ::session2tid($session)]} {
		set thread $::session2tid($session)
	    }
	}

	set sock [dict get $req -sock]
	if {![info exists thread] && [info exists ::sock2tid($sock)]} {
	    # do we have a socket tied thread?
	    set thread $::sock2tid($sock)
	} else {
	    # need to grab a free thread
	    if {[catch {threads get} thread]} {
		if {[catch {mkthreads}]} {
		    [Httpd Exhausted $sock]
		}
		set thread [threads get]	;# try again - propagate error
	    }
	    if {[::thread::preserve $thread] != 2} {
		error "Thread $thread has been allocated $i times"
	    }
	}

	set ::sock2tid($sock) $thread
	::thread::send -async $thread [list incoming $req]
    }

    variable thisdir [file dirname [file normalize [info script]]]
    variable scriptname "BackendWorker.tcl"
    variable config	;# a configuration array available to workers

    proc init {args} {
	variable config
	array set config $args
	# a script to get a thread going
	if {![info exists config(scriptdir)]} {
	    variable thisdir
	    set config(scriptdir) $thisdir
	}
	if {![info exists config(scriptname)]} {
	    variable scriptname
	    set config(scriptname) $scriptname
	}
	variable script [::fileutil::cat [file join $config(scriptdir) $config(scriptname)]]
	mkthreads	;# construct initial thread pool
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
	Debug.error {$args}
    }

    Debug off socket 10
    Debug on http 2
    Debug on cache 10
    Debug on dispatch 10

    set listener [Listener %AUTO% -port 8080 -sockets Httpd -httpd {-dispatch "Backend incoming"}]
    set forever 0
    vwait forever
}
