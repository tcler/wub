package require Httpd 3.0

package provide HttpdSingle 1.0

# construct a faux Worker for Backend to call as a shim
proc HttpdWorker {cmd req} {
    set interp [dict get $req -iworker]
    $interp eval HttpdWorker $cmd [list $req]
}

namespace eval Httpd {
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
	set cid [dict get $request -cid]
	variable {*}[dict get $connection($cid)]
	interp eval $thread [list HttpdWorker Send $request $cacheit]
	Activity activity sent $cid $request
    }

    # workers - command each worker
    proc workers {args} {
	variable workers
	foreach tid [array names workers] {
	    interp eval $tid $args
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

    proc indication {args} {
	{*}$args
    }

    # mkinterp - make and enqueue $incr new interps,
    # up to a limit of $max
    proc mkinterp {} {
	variable workers
	variable max
	if {[array size workers] > $max} {
	    error "Worker exhaustion - max $max"
	} else {
	    # set up script for threads
	    variable thread_path
	    variable script
	    set s {
	    }

	    append s [subst {
		set ::auto_path [list $thread_path]
		array set ::env [list [array get ::env]]
		$script
	    }]

	    # create $incr new threads, add them to queue
	    variable incr
	    set nt {}
	    for {set i 0} {$i < $incr} {incr i} {
		set new [interp create]
		set s1 $s
		append s1 \n "proc id {} {return $new}" \n
		$new eval $s1
		set workers($new) {}
		lappend nt $new

		$new alias indicate ::Httpd::indication
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
    #mkthreads	;# construct initial thread pool

    # transfer - transfer control to a worker thread
    proc transfer {id request} {
	variable connection
	variable {*}$connection($id)
	if {[catch {
	    Debug.socket "Transferring $socket to $thread"
	    interp transfer {} $socket $thread
	    $thread eval [list HttpdWorker Connect $socket $request]
	} result eo]} {
	    Debug.error {Transfer Error: $result ($eo)}
	    Activity activity transfer $id -error $result -eo $eo {*}$request
	} else {
	    Debug.socket {Transferred: $result $eo}
	    Activity activity transfer $id $request
	}
    }

    # associate - a request with an Httpd worker
    proc associate {request} {
	# grab a free thread/worker
	if {[catch {pool get} thread]} {
	    mkinterp
	    set thread [pool get]	;# try again - propagate error
	}

	# arrange for socket/task connection
	# it's necessary to enter the event loop and wait for idle,
	# it's a known bug in Thread channel transfer
	variable over

	# add thread to connection state
	variable connection
	set id [dict get $request -cid]
	dict set connection($id) thread $thread
	set socket [dict get $connection($id) socket]

	catch {
	    dict set request -worker [::thread::id]	;# where to Send
	}
	dict set request -iworker $thread

	transfer $id $request
    }

    # a worker thread has completed its task
    # recycle it into the pool for a new connection
    proc disassociate {id} {
	variable connection
	variable {*}[dict get $connection($id)]
	Debug.socket {disassociate: $socket $thread}

	pool put $thread	;# we're done with this thread
    }

    # disconnect - force a disconnection in worker thread
    proc disconnect {id} {
	variable connection
	variable {*}[dict get $connection($id)]
	$thread eval [list HttpdWorker Disconnect $socket forced]
    }

    # cleanup - destroy resources
    proc cleanup {} {
	pool destroy	;# destroy thread queue

	# synchronously destroy worker threads
	variable workers
	foreach thread [array names workers] {
	    $thread eval exit
	}
    }
}
