# Async - asynchronous thread wrapper
#
# Example:
#
# -- asynchronously calls the '::thread::id' command in the thread, prints the result
# Async create async
# async call ::thread::id puts
# async destroy

if {0 && [info exists argv0] && $argv0 eq [info script]} {
    lappend ::auto_path .	;# for unit tests (at end)
}

package require Thread
#package require Debug
#Debug define async

oo::class create ::Async {
    # response - process response from next call
    method response {var count op} {
	upvar 1 $var result
	variable id 

	# get the async response
	lassign $result($count) code e eo
	unset result($count)

	# get the scripts associated with this response (by $count)
	variable responder
	variable next
	lassign $responder([incr next]) response error
	unset responder($next)

	#Debug.async {response $next: $var $op -> code:$code e:$e eo:($eo)}

	# invoke the appropriate script to process result
	switch -- $code {
	    return - 2 -
	    ok - 0 {
		if {$response ne ""} {
		    #Debug.async {DO: $response $e}
		    {*}$response $e
		} else {
		    # discard result
		}
	    }
	    default {
		if {$error eq ""} {
		    ::return -code $code -options $eo $e	;# fall through to [interp bgerror]
		} else {
		    {*}$error $code $e $eo
		}
	    }
	}
    }

    # call - asynchronously send call script to thread
    # callback $response on success, $error on error
    method call {call {response {}} {error {}}} {
	variable id
	variable responder
	variable rcount
	set responder([incr rcount]) [list $response $error]

	#Debug.async {$id call$rcount ($call) response:($response) error:($error)}

	::thread::send -async $id [list ::_thread::call $call] [namespace current]::waiter($rcount)
    }

    # construct some pass through commands - their use is not generally recommended
    foreach n {preserve release configure exists broadcast join transfer} {
	method $n {args} [string map [list %N% $n] {
	    variable id
	    thread::%N% $id {*}$args
	}]
    }

    destructor {
	my release	;# just delete the thread
    }

    constructor {args} {
	if {[llength $args]%2} {
	    variable script [lindex $args end]
	    set args [lrange $args 0 end-1]
	} else {
	    variable script {}
	}

	variable prescript {
	    namespace eval ::_thread {
		# call - run the script, return the full result
		proc call {script} {
		    list [catch {uplevel #0 $script} e eo] $e $eo
		}
	    }
	}

	variable postscript {
	    ::thread::wait
	}

	variable {*}$args
	variable next	;# next expected response
	variable rcount	;# last sent request

	variable id [::thread::create -preserved $prescript$script$postscript]
	::thread::configure $id -eventmark 3

	trace add variable [namespace current]::waiter write [list [self] response]
    }
}

if {[info exists argv0] && $argv0 eq [info script]} {
    # Unit Test
    #Debug on thread
    set max 10
    for {set i 0} {$i < $max} {incr i} {
	set thread($i) [Async new {
	    proc terror {args} {
		puts stderr [::thread::id]:$args
	    }
	    #::thread::errorproc terror
	    #interp bgerror "" terror
	}]
    }
    interp bgerror "" {output BGERROR}

    proc output {args} {
	puts stderr $args
    }

    # test error handling
    $thread(0) call {expr {1/0}} output {output ERROR:}
    $thread(0) call {expr {2/0}} output	;# this one gets picked up by bgerror

    after idle {
	time {
	    set i [expr {int(rand() * $max)}]
	    $thread($i) call ::thread::id [list output [incr count] $i:]
	} 100
    }

    vwait forever

}
