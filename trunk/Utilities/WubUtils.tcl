package require extend 1.0
package require file
package require Dict
package require memoize
package require Debug
Debug off wubutils 10

package provide WubUtils 1.0

set ::debug 0
#set ::debug 100

# dict_probe - search a dict for matches
proc dict_probe {probe args} {
    if {[llength $args] == 1} {
	set args [lindex $args 0]
    }
    Debug.wubutils {probe:$probe args: $args}
    while {[llength $args] > 1 && [llength $probe] > 0} {
	# we still have a dict to search and a probe to find
	foreach {key val} $args {
	    set found 0
	    set p [lindex $probe 0]	;# next probe element
	    # probe each element of key 
	    foreach m $key {
		if {[string match $m $p]} {
		    # found a matching element
		    if {[llength $probe] == 1} {
			# finished the probe
			Debug.wubutils {probe done: $probe probe:$val}
			return [list $probe $val]
		    } else {
			# repeat search at next level
			set probe [lrange $probe 1 end]
			set args $val	;# probe this dict element
			Debug.wubutils {probe repeat: $probe in $val}
			set found 1
			break
		    }
		}
		if {$found} break
	    }
	    if {$found} break
	}
    }
    Debug.wubutils {probe none: $args probe:$probe}
    return [list $probe $args]
}

proc caller {} {
    return [string map {"Snit_method" ""} [namespace tail [lindex [info level -2] 0]]]
}

proc dumpMsg {req {short 1}} {
    catch {
	if {$short} {
	    if {[dict exists $req -content]} {
		dict set req -content <ELIDED>
	    }
	    if {[dict exists $req -gzip]} {
		dict set req -gzip <ELIDED>
	    }
	    if {[dict exists $req -template contents]} {
		dict set req -template contents <ELIDED>
	    }
	    if {[dict exists $req -original]} {
		dict set req -original <ELIDED>
	    }
	}
    }

    return $req
}

# Msg - return a shortened request dict
proc Msg {req args} {
    foreach a $args {
	catch {dict unset req $a}
    }
    return $req
}

proc namecheck {name} {
    set valid {^[A-Za-z][A-Za-z0-9_ @.%]*$}
    set invalid {[^A-Za-z0-9_ @.%]}
    if {![regexp $valid $name]} {
	error "Name '$name' is invalid [regexp -all -inline $invalid $name]"
    }
}

# Calls an instance method for an object given its
# instance namespace and remaining arguments (the first of which
# will be the method name.
#
# selfns		The instance namespace
# args			The arguments
#
# Uses the selfns to determine $self, and calls the method
# in the normal way.
#
# This is used to implement the "mymethod" command.
package require snit
proc ::snit::RT.CallInstance {selfns args} {
    upvar ${selfns}::Snit_instance self
    return [uplevel 1 $self $args]
}

# convert a bastardised emacs httms timestamp to something useful
proc hhmts {time} {
    set ::httmts [string trim $time "<!->\n"]
    return $time
}

proc alias {alias args} {
    interp alias {} $alias {} {*}$args
}

alias tclarmour string map {\[ "&#x5B;" \] "&#x5D;" \{ "&#x7B;" \} "&#x7D;" $ "&#x24;"}
