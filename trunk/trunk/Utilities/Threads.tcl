# Threads -- wrappers around tcl threads

package provide Threads 1.0

package require Thread
package require snit

# Thread --
#
#	class wrapper for tcl thread package
#
# provides centralised Wait.
# allows callback on async completion.
#

::snit::type Thread {
    option -label	;# human readable label
    option -init ""		;# initial code to jam into thread
    option -logger ""		;# logger
    option -main 0		;# is this the main thread?

    #variable thread		;# thread
    variable completion ""	;# completion command

    # wrap the tcl thread package
    component thread
    delegate method {thread *} to thread using "::thread::%m %c"

    # delegation support - the request object may extend
    # Thread with useful functionality
    option -super ""
    component super -inherit yes

    typevariable pending -array {}	;# pending thread completions
    typevariable id2thread -array {}	;# mapping id<->Thread

    # heartbeat - a periodic pulse
    typevariable heartbeat ""		;# heartbeat
    typevariable pulse -array {}		;# scripts to run at heartbeat
    typevariable pulse_rate 50000	;# heartbeat rate

    typevariable log
    typeconstructor {
	set log [logger::init Threads]
	#${log}::enable debug
	${log}::disable debug
    }

    typemethod loadavg {} {
	foreach t [Thread info instances] {
	    if {[$t cget -main]} {
		break
	    }
	}
	append result "$t - '[$t cget -label]'" \n
	
	foreach t [Thread info instances] {
	    append result "$t - '[$t cget -label]'" \n
	}
	return $result
    }

    # Pulse --
    #
    #	register a script to run at heartbeat frequency

    method Pulse {script} {
	# determine next available response
	set next [lsort -integer -decreasing [array names pulse]]
	if {$next ne {}} {
	    set next 0
	} else {
	    incr next
	}
	set pulse($next) $script
	return $next
    }
    method dePulse {$next} {
	catch {unset pulse($next)}
    }

    # Pending --
    #
    #	returns array of pending threads and their completion information
    #
    # Arguments:
    #
    # Results:
    #	dict of per-thread {async result, completion command}
    #
    # Side Effects:

    typemethod Pending {} {
	set result {}
	foreach {n v} [array get pending] {
	    lappend result [list $n $v]
	}
	return $result
    }

    # procWait --
    #
    #	handle completion of any pending thread
    #
    # Arguments:
    # Results:
    #
    # Side Effects:
    #	all completed threads' callback commands are evaluated

    typemethod procWait {} {
	${log}::debug "procWait: [array get pending]"

	foreach thread [array names pending] {

	    # the thread is no longer pending
	    set result $pending($thread)
	    unset pending($thread)

	    # handle heartbeat
	    if {$thread eq {}} {
		# heartbeat
		set heartbeat [after $pulse_rate "set [mytypevar pending]() 1"]
		foreach p [lsort -integer [array names pulse]] {
		    catch {eval $pulse($p)}
		}
		continue	;# heartbeat completed
	    }

	    # async call completed - invoke thread's callback
	    ${log}::debug "complete $thread '$result'"

	    lassign $result result eo
	    set code [dict get $eo -code]

	    if {[catch {
		$thread complete $code $result $eo
	    } result eo]} {
		${log}::error "thread $thread callback Error: $eo"
	    } else {
		${log}::debug "callback $thread done"
	    }
	}
    }

    # wait for the completion of one or more
    # of the pending async commands
    # return the completed threads

    # Wait --
    #
    #	Wait for thread completions
    #
    # Arguments:
    # Results:
    #
    # Side Effects:
    #	all completed threads' callback commands are evaluated

    typemethod Wait {} {
	set pp [mytypevar pending]	;# get pending thread array

	if {$heartbeat eq ""} {
	    # start up a heartbeat
	    set heartbeat [after $pulse_rate "set [mytypevar pending]() 1"]
	}

	# process already-completed threads
	${log}::debug "\nWait: [array get $pp]"
	Thread procWait

	# wait for new thread to complete
	${log}::debug "Woken1: [array get $pp]"
	vwait [mytypevar pending]

	# process newly-completed threads
	${log}::debug "Woken2: [array get $pp]"
	Thread procWait

	${log}::debug "unWait: [array get $pp]"
    }

    # complete --
    #
    #	run thread's completion callback
    #
    # Arguments:
    # Results:
    #
    # Side Effects:
    #	completion callback is evaluated

    method complete {code result eo} {
	{*}$completion $code $result $eo	;# run completion command
	unset completion
    }

    # async --
    #
    #	send an async command to thread
    #
    # Arguments:
    #	command	completion to be run when an async completes
    #	args	command to be evaluated in thread
    #
    # Results:
    #
    # Side Effects:
    #	async command is evaluated in thread
    #	completion callback is scheduled

    method async {command args} {
	$options(-logger)::debug "async '$args' -> $command"
	set completion $command	;# record completion in self

	# invoke command asynchronously
	thread::send -async $thread \
	    [list ::thread::rx $args] \
	    [mytypevar pending($self)]
    }

    # send --
    #
    #	send a sync command to thread
    #
    # Arguments:
    #	args	command to be evaluated in thread
    #
    # Results:
    #
    #	command thread evaluation result
    #
    # Side Effects:
    #	async command is evaluated in thread
    #	completion callback is scheduled

    method send {script} {
	# invoke the command synchronously via rx thread wrapper
	$options(-logger)::debug "send '$script'"
	thread::send $thread [list ::thread::rx $script] result

	# recover components of result
	lassign $result result eo
	set code [dict get $eo -code]

	# return options and result to caller
	$options(-logger)::debug "send result $code: '$result' ($eo)"
	return -code $code -options $eo $result 0
    }

    option -dispatch Dispatch
    variable request

    # TRespond --
    #
    #	handle response dispatch
    #
    # Arguments:
    #
    #	code	error code of the dispatch
    #	result	HTTP response from dispatch
    #	eo	error options dict
    #
    # Results:
    # Side-Effects:
    #

    method TRespond {code result eo} {

	# get the transformed request (if any)
	if {[dict exists $eo -response]} {
	    # explicit response
	    set rsp [dict get $eo -response]
	} elseif {[dict exists $eo -request]} {
	    # no explicit response, use request
	    set rsp [dict get $eo -request]
	} else {
	    # ignorant process failed to transform request
	    set rsp $request
	}

	dict unset eo -response
	dict set rsp -http [lindex [dict get $rsp -http] end]

	# rethrow the response
	return -code $code -response $rsp -options $eo $result
    }

    method Dispatch {req} {
	# wrap the request's -http arg in a thread wrapper
	# this allows transparent interaction with Httpd object
	set http [dict get $req -http]
	dict set req -http [list ::thread::pthread [lindex $http end]]

	set request $req	;# record this in case

	# asynchronously invoke thread Dispatch, with TRespond callback
	$self async [list $self TRespond] {*}$options(-dispatch) $req
    }

    constructor {args} {
	if {[catch {
	    $self configurelist $args	;# initialise self and thread
	    if {$options(-main)} {
		set thread [::thread::id]	;# the parent thread
	    } else {
		set thread [::thread::create]	;# create a tcl thread
	    }
	    # bissociate thread id and self
	    set id2thread($thread) $self
	    set id2thread($self) $thread

	    # install a handling script in the thread
	    # the handler ensures that we return all return options
	    thread::send $thread \
		[string map [list %PARENT $self %PTHREAD [thread::id]] {
		    namespace eval ::thread {
			proc rx {script} {
			    catch {
				uplevel \#0 $script
			    } result eo
			    return [list $result $eo]
			}

			variable parent %PARENT
			variable pthread %PTHREAD

			# request a service from parent thread object
			proc parent {args} {
			    set code [send %PTHREAD \
					  [list %PARENT {*}$args] \
					  result]

			    return -code $code $result
			}
		    }
		}]

	    if {$options(-logger) eq ""} {
		set options(-logger) $log
	    }

	    set super $options(-super)

	    if {$options(-init) ne ""} {
		$self send $options(-init)
	    }
	} result eo]} {
	    ${log}::error "$self Err: $result / $eo"
	}
    }

    destructor {
	catch {$thread release}
	unset id2thread(id2thread($self))
	unset id2thread($self)
    }
}

Thread MainThread -main 1 -label "Main Thread"
