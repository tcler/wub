namespace eval Httpd {
    proc dispatch {sock request} {
	    HttpdWorker Connect $request $sock
    }

    # construct initial thread pool
    if {$max_threads > 0} {
    } else {
	# this is a single threaded site
	for {set j 0} {$j < $thread_fanout} {incr j} {
	    pool put $j
	}
    }
}
