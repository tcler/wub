package require extend 1.0
package require file
package require Dict
package require memoize

package provide WubUtils 1.0

set ::debug 0
#set ::debug 100

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

alias tclarmour string map {\[ "&5B;" \] "&5D;" \{ "&7B;" \} "&7D;" $ "&24;"}
