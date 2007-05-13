# timer - a timer object
package provide Timer 1.0

package require snit

::snit::type Timer {
    variable timer ""	;# after timer

    # cancel any timers
    method cancel {} {
	#upvar 1 self owner
	#Debug.http {Timer $self for '$owner' timer $timer}
	if {$timer != ""} {
	    catch {after cancel $timer}
	    set timer ""
	}
    }

    method dump {} {
	return [list $timer]
    }

    method running? {} {
	if {[catch {after info $timer} info]} {
	    return 0
	} else {
	    return 1
	}
    }

    variable cmd

    # start a new timer
    method after {when what} {
	uplevel 1 $self cancel
	if {$timer ne ""} {
	    # still have a timer running - cancel it
	    $self cancel
	}
	set cmd $what
	set timer [after $when $what]

	#upvar 1 self owner
	#Debug.http {Timer $self for '$owner' after $when '$what' -> $timer}
	#Debug.http {Timer $self [info level -1]}
    }
}
