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
    variable at

    # restart timer
    method restart {} {
	$self after $at {*}$cmd
    }

    # start a new timer
    method after {when args} {
	uplevel 1 $self cancel
	if {$timer ne ""} {
	    # still have a timer running - cancel it
	    $self cancel
	}
	set at $when
	set cmd $args
	set timer [after $when {*}$args]
    }
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    Timer T
    T after 3000 {puts moop}
    T cancel
    T after 3000 {puts moop}
    puts [T dump]
    set forever 0
    vwait forever
}
