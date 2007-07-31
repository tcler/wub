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

    # return the be process associated with a socket
    proc s2be {sock} {
	variable sock2tid
	if {[info exists sock2tid($sock)]} {
	    return $sock2tid($sock)
	} else {
	    return ""
	}
    }

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

	    if {$script eq ""} {
		variable scriptdir
		variable scriptname
		set script [::fileutil::cat [file join $scriptdir $scriptname]]
	    }

	    # provide backend with some interface shims
	    set s {
		# Activity - fetch an activity log
		proc Activity {args} {
		    set args [linsert $args 0 Activity]
		    return [::thread::send $::thread::parent $args]
		}
		
		# Cache - interact with cache
		proc Cache {args} {
		    set args [linsert $args 0 Cache]
		    ::thread::send -async $::thread::parent $args
		}
		
		# Block - interact with block
		proc Block {args} {
		    set args [linsert $args 0 Block]
		    ::thread::send -async $::thread::parent $args
		}
		
		# Send a packet
		proc Send {rsp} {
		    ::thread::send -async [dict get $rsp -worker] [list HttpdWorker Send $rsp]
		}
	    }

	    append s [subst {
		set ::auto_path [list $thread_path]
		set ::thread::parent $me
		array set ::config [list [array get config]]
		$script
		thread::wait
	    }]

	    for {set i 0} {$i < $incr} {incr i} {
		set new [::thread::create $s]
		::thread::send $new "proc id {} {return $new}"
		if {[::thread::preserve $new] != 1} {
		    error "Thread $thread has been allocated $i times"
		}

		set worker($new) {}
		threads put $new
	    }
	}
    }

    variable connection

    # process incoming
    proc Incoming {req} {
	Debug.backend {get thread for work}

	variable connection
	set cid [dict get $req -cid]

	if {![info exists connection($cid)]} {
	    # need to grab a free thread
	    if {[catch {threads get} thread]} {
		if {[catch {mkthreads}]} {
		    Httpd Exhausted $sock
		}
		set thread [threads get]	;# try again - propagate error
	    }
	    if {[::thread::preserve $thread] != 2} {
		error "Thread $thread has been allocated $i times"
	    }
	    dict set connection($cid) thread $thread
	} else {
	    dict with connection($cid) {}
	}

	set sock [dict get $req -sock]
	dict set connection($cid) socket $sock
	variable sock2cid; set sock2cid($sock) $cid

	::thread::send -async $thread [list Incoming $req]
    }

    proc Disconnect {sock} {
	variable sock2cid
	variable connection
	if {[info exists sock2cid($sock)]} {
	    # remove the socket->thread association
	    set cid $sock2cid($sock)
	    unset connection($cid)

	    # inform the backend worker
	    dict unset req -worker
	    ::thread::send -async $thread [list Disconnected $req]

	    # replace the thread in its queue
	    if {[::thread::release $thread] != 1} {
		Debug.error {release Thread $thread has been overallocated}
	    }
	    threads put $thread	;# we're done with this thread
	}
    }

    variable scriptdir [file dirname [file normalize [info script]]]
    variable scriptname "BackendWorker.tcl"
    variable config	;# a configuration array available to workers

    proc configure {args} {
	variable config
	array set config $args
	# a script to get a thread going
	variable {*}$args
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
	#puts stderr "ERROR: $args"
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
