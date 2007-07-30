package require Thread
package require Httpd 3.0

package provide HttpdThread 1.0

namespace eval Httpd {
    variable me [::thread::id]	;# Httpd main thread
    variable thread_path $auto_path	;# path for worker threads
    lappend thread_path ../Utilities/ ../extensions/ ../Utilities/zlib/

    # array of known threads
    variable workers
    array set workers {}	;# set of all threads
    ::struct::queue pool	;# queue of free threads
    variable max 20	;# maximum number of threads
    variable incr 5	;# number of threads to add on exhaustion
    variable over 10	;# degree of thread overcommittment

    # send - send a request to its associated thread
    proc send {request {cacheit 1}} {
	variable connection
	variable {*}[dict get $connection([dict get $request -cid])]
	::thread::send -async $thread [list HttpdWorker Send $request $cacheit]
    }

    # workers - command each worker
    proc workers {args} {
	variable workers
	foreach tid [array names workers] {
	    ::thread::send -async $tid {*}$args
	}
    }

    # initialisation script for worker threads
    variable script [::fileutil::cat [file join [file dirname [info script]] HttpdWorker.tcl]]

    proc dump2 {} {
	variable workers; append result "worker threads: [array get workers]"
	append result " ([pool size] in idle queue,"
	variable max; variable incr; append result " [array size workers] in total, up to $max, increments of $incr)" \n
	variable dispatch; append result "dispatch: $dispatch" \n

	variable ignore;
	if {$ignore} {
	    variable quiescent
	    append result "Going Idle to perform '$quiescent'"
	}

	return $result
    }

    # mkthreads - make and enqueue $incr new threads,
    # up to a limit of $max
    proc mkthreads {} {
	variable workers
	variable max
	if {[array size workers] > $max} {
	    error "Thread exhaustion - max $max"
	} else {
	    # set up script for threads
	    variable me
	    variable thread_path
	    variable script
	    set s {
		proc indicate {args} {
		    thread::send -async $::thread::parent $args
		}
	    }
	    append s [subst {
		set ::auto_path [list $thread_path]
		set ::thread::parent $me
		array set ::env [list [array get ::env]]
		$script
	    }]

	    # create $incr new threads, add them to queue
	    variable incr
	    set nt {}
	    for {set i 0} {$i < $incr} {incr i} {
		set new [::thread::create $s]
		set workers($new) {}
		lappend nt $new
		::thread::preserve $new
	    }

	    # overcommmit each thread to some degree
	    variable over
	    for {set j 0} {$j < $over} {incr j} {
		foreach new $nt {
		    pool put $new
		}
	    }
	}
    }
    mkthreads	;# construct initial thread pool

    # transfer - transfer control to a worker thread
    proc transfer {id request} {
	variable connection
	variable {*}$connection($id)
	if {[catch {
	    Debug.socket "Transferring $socket to $thread"
	    ::thread::transfer $thread $socket
	    ::thread::send -async $thread [list HttpdWorker Connect $socket $request]
	} result eo]} {
	    Debug.error {Transfer Error: $result ($eo)}
	    Activity activity $id transfer $result $eo
	} else {
	    Debug.socket {Transferred: $result $eo}
	    Activity activity $id transfer
	}
    }

    # associate - a request with an Httpd worker
    proc associate {request} {
	# grab a free thread/worker
	if {[catch {pool get} thread]} {
	    mkthreads
	    set thread [pool get]	;# try again - propagate error
	}

	# arrange for socket/task connection
	# it's necessary to enter the event loop and wait for idle,
	# it's a known bug in Thread channel transfer
	variable over
	if {[::thread::preserve $thread] > $over} {
	    error "Thread $thread has been overallocated"
	}

	# add thread to connection state
	variable connection
	set id [dict get $request -cid]
	dict set connection($id) thread $thread
	set socket [dict get $connection($id) socket]

	after idle [namespace code [list transfer $id $request]]
    }

    # a worker thread has completed its task
    # recycle it into the pool for a new connection
    proc disassociate {id} {
	variable connection
	variable {*}[dict get $connection($id)]
	Debug.socket {disassociate: $socket $thread}

	set i [::thread::release $thread]
	pool put $thread	;# we're done with this thread
    }

    # disconnect - force a disconnection in worker thread
    proc disconnect {id} {
	variable connection
	variable {*}[dict get $connection($id)]
	::thread::send -async $thread [list HttpdWorker Disconnect $socket forced]
    }

    # cleanup - destroy resources
    proc cleanup {} {
	pool destroy	;# destroy thread queue

	# synchronously destroy worker threads
	variable workers
	foreach thread [array names workers] {
	    while {[::thread::release $thread] != 0} {}
	}
    }
}
