# Http nearly-1.1 client in pure tcl.
package require Thread
package require struct::queue
package require fileutil

package provide HttpdClient 1.0

namespace eval HttpC {
    variable me [::thread::id]
    variable counter 0	;# unique prefix for thread commands

    # CWub 'home' directory, for finding scripts, etc.
    variable home [file dirname [info script]]

    # thread_path will be lappended to each worker thread's auto_path
    variable thread_path $auto_path	;# auto_path for worker threads
    lappend thread_path [file join $home Utilities] [file join $home extensions] [file join $home Utilities/zlib]

    # queue of free threads forming a worker thread pool.
    ::struct::queue threads

    # array of known threads
    variable worker
    array set worker {}
    variable max 257	;# maximum number of threads
    variable incr 20	;# number of threads to add on exhaustion

    variable callbacks	;# list of continuations for each thread
    array set callbacks {}

    variable host2thread	;# map of host to thread
    array set host2thread {}

    variable threadCmd	;# map from thread to counter
    array set threadCmd {}

    # a script to get a thread going
    # This script force-loads the HttpWorker.tcl script into each
    # worker thread.
    variable script [::fileutil::cat [file join [file dirname [info script]] HttpWorker.tcl]]

    # mkthreads - add threads to the pool of worker threads
    # $incr new threads are created at a time, up to a limit of $max
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

    # get_thread - get a worker thread from a pool
    # make new threads if the current pool's exhausted
    proc get_thread {} {
	Debug.socket {get thread for writing}

	# grab a free thread
	if {[catch {threads get} thread]} {
	    mkthreads
	    set thread [threads get]	;# try again - propagate error
	}

	if {[::thread::preserve $thread] != 2} {
	    # consistency check to ensure threads have been freed.
	    error "Thread $thread has been allocated $i times"
	}

	return $thread
    }

    # destroy - clean up all resources associated with CWub
    proc destroy {} {
	threads destroy	;# destroy thread queue

	# synchronously destroy worker threads
	variable worker
	foreach thread [array names worker] {
	    ::thread::release $thread
	}
    }

    # thread - command interface to worker thread
    # cmd and args will be passed to the eponymous thread
    proc thread {tid cmd args} {
	switch $cmd {
	    connect -
	    send {
		# send cmd is processed synchronously, so user knows
		# that the send has been accepted to be processed
		set extra {}
	    }
	    quote {
		# quote is a pseudo-command which passes through
		# without -async munging
		set cmd [lindex $args 0]
		set args [lrange $args 1 end]
	    }
	    default {
		# by default, worker commands are asynchronous
		set extra {-async}
	    }
	}

	# send the message to the worker thread
	return [::thread::send {*}$extra $tid [list $cmd {*}$args]]
    }

    # connect - construct a worker thread to connect
    # to host/port.  Returns the name of the worker thread.
    # Creates an alias for the worker thread.
    proc connect {host {port 80} args} {
	Debug.httpclient {host:$host port:$port args:$args}
	set tid [get_thread]

	# remember current unique counter for this thread
	set tidc "$tid,[incr counter]"
	variable threadCmd; set threadCmd($tid) $tidc
	variable host2thread; set host2thread($host,$port) $tidc; set host2thread($tid) "$host,$port"

	# create an alias for the thread
	interp alias {} $tidc {} HttpC::thread $tid
	$tidc connect $host $port {*}$args ;# open the connection

	return $tidc
    }

    # received - worker thread has received a response R
    proc received {thread R} {
	Debug.httpclient {received: ([set x $R; catch {dict unset x -entity}; dict unset x -header]) from $thread} 1
	Debug.httpclient {received content: ([dict get $R -entity])} 3

	# discover worker id from thread id
	variable host2thread; set id $host2thread($thread)

	variable callbacks
	Debug.httpclient {callbacks: [array get callbacks]} 3
	if {[info exists callbacks($id)]} {
	    # perform callback, passing HTTP response $R
	    # callbacks are called in a fifo order
	    set callback [lindex $callbacks($id) 0]
	    set callbacks($id) [lrange $callbacks($id) 1 end]
	    catch {{*}$callback $R}
	}
    }

    # disconnect - worker thread has disconnected from host
    # undo connection, clean up its resources
    #
    # thread: worker thread that's suffered disconnection
    # msg: reason, if any, given for the disconnection
    # eo: error info of error (if any) occasioning the disconnection
    proc disconnect {thread msg {eo ""}} {
	# delete the associated alias
	variable threadCmd;
	if {[info exists threadCmd($thread)]} {
	    Debug.httpclient "disconnect: $msg ($eo) from $thread"
	    interp alias {} $threadCmd($thread) {}
	    unset threadCmd($thread)

	    variable host2thread
	    variable callbacks

	    set id $host2thread($thread)

	    # send error callbacks
	    set errRsp [list -code 500 -message $msg -error $eo]
	    if {[info exists callbacks($id)]} {
		foreach callback $callbacks($id) {
		    catch {{*}$callback $errRsp}
		}
		unset callbacks($id)
	    }

	    # clean up host/thread mapping
	    unset host2thread($id)
	    unset host2thread($thread)

	    # release worker back to pool
	    if {[::thread::release $thread] != 1} {
		puts stderr "release Thread $thread has been allocated $i times"
	    }
	    threads put $thread	;# we're done with this thread
	}
    }

    # chanwrite - built-in callback to write HTTP responses
    # to a file/stream/chan - can use rchan for this.
    proc chanwrite {chanid R} {
	puts -nonewline $chanid [dict get $R -entity]
    }

    # send - send the command cmd to the url with given args
    proc send {cmd url args} {
	# parse out url components
	set R [dict merge [list -port 80] [Url parse $url]]
	set host [dict get $R -host]
	set port [dict get $R -port]
	set id "$host,$port"	;# connection id

	# ensure there's a thread for this host
	variable host2thread
	if {![info exists host2thread($id)]} {
	    # no connection to this host/port - make one
	    connect $host $port
	}

	# handle the -chan arg by recording a callback to write contents
	# consider using the rchan package.
	if {[dict exists $args -chan]} {
	    set chanid [dict get $args -chan]
	    dict set args -callback [list HttpCd chanwrite $chanid]
	}

	# remember any callback for this host
	variable callbacks
	if {[dict exists $args -callback]} {
	    lappend callbacks($id) [dict get $args -callback]
	} else {
	    lappend callbacks($id) {}
	}

	# send cmd to thread, to perform transaction
	set result [$host2thread($id) $cmd $url {*}$args]

	return $result
    }

    # get - issue a GET command
    proc get {url args} {
	return [send get $url {*}$args]
    }

    # head - issue a HEAD command
    proc head {url args} {
	return [send head $url {*}$args]
    }

    # post - issue a POST command
    proc post {url body args} {
	return [send post $url {*}$args -entity $body]
    }

    # put - issue a PUT command
    proc put {url body args} {
	return [send put $url {*}$args -entity $body]
    }

    # delete - issue a DELETE command
    proc delete {url args} {
	return [send delete $url {*}$args]
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    foreach d {Utilities extensions Utilities/zlib} {
	lappend auto_path [file join $HttpC::home $d]
    }

    package require Stdin
    package require Debug
    package require Url

    Debug on httpclient 3
    Debug off socket 10

    interp bgerror {} bgerror
    proc bgerror {args} {
	puts stderr "Main: $args"
    }

    set forever 0
    vwait forever

    HttpC get http://localhost/
    HttpC get http://localhost/ connection close	;# close the connection
    HttpC get http://localhost/ -callback $fn		;# call {*}fn on reception

    set con [HttpC open localhost]; $con get http://localhost

    package require rchan
    set fd [chan create {read write} rchan]
    HttpC get http:://localhost/ -chan $fd
    set content [read $fd]
}
