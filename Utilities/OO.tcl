# OO.tcl - helpers for tclOO
package require TclOO
namespace import oo::*

package provide OO 1.0

set ::API(Utilities/OO) {
    {
	Helpers for TclOO - adds some nice to have functionality
    }
}

proc ::oo::Helpers::classvar {name args} {
    set self [uplevel 1 self]
    set ns [info object namespace [info object class $self]]
    foreach v [list $name {*}$args] {
	uplevel 1 [list namespace upvar $ns $v $v]
    }
}

proc ::oo::Helpers::next? {args} {
    if {[llength [uplevel self next]]} {
	uplevel 1 [list next {*}$args]
    }
}

proc ::oo::define::classmethod {name {args {}} {body {}}} {
    # Create the method on the class if
    # the caller gave arguments and body
    set argc [llength [info level 0]]
    if {$argc == 4} {
        uplevel 1 [list self method $name $args $body]
    } elseif {$argc == 3} {
        return -code error "wrong # args: should be \"[lindex [info level 0] 0] name ?args body?\""
    }
    
    # Get the name of the current class
    set cls [lindex [info level -1] 1]
    
    # Get its private “my” command
    set my [info object namespace $cls]::my
    
    # Make the connection by forwarding
    tailcall forward $name $my $name
}

proc oo::define::Variable args {
    set currentclass [lindex [info level 1] 1]
    set existing [uplevel 1 [list info class variables $currentclass]]
    switch [lindex $args 0] {
        -append {
            set vars $existing
            lappend vars {*}[lrange $args 1 end]
        }
        -prepend {
            set vars [lrange $args 1 end]
            lappend vars {*}$existing
        }
        -remove {
            set vars $existing
            foreach var [lrange $args 1 end] {
                set idx [lsearch -exact $vars $var]
                if {$idx >= 0} {
                    set vars [lreplace $vars $idx $idx]
                }
            }
        }
        -set - default {
            set vars [lrange $args 1 end]
        }
    }
    uplevel 1 [list variables {*}$vars]
    return
}

# lets you do this:
# oo::class create foo {
#     Variable x y
#     Variable -append p d q
#     method bar args {
#         lassign $args x y p d q
#     }
#     method boo {} {
#         return $x,$y|$p,$d,$q
#     }
# }

proc oo::define::Var {args} {
    uplevel 1 [list Variable -append {*}[dict keys $args]]
    # now have to arrange to have these values assigned at constructor time.
}

# varOf - make a var in the class given
proc ::oo::Helpers::varOf {class name args} {
    set self [uplevel 1 self]
    set ns [info object namespace $class]
    foreach v [list $name {*}$args] {
	uplevel 1 [list namespace upvar $ns $v $v]
    }
}

if {[info commands Debug] eq ""} {
    proc Debug.hierarchy {args} {}
    proc Debug.refcount {args} {}
    Debug define hierarchy
    Debug define refcount
}

# ::oo::Hierarchy - parents and children
class create ::oo::Hierarchy {
    method parent {} {
	variable parents;
	return [lindex [dict keys $parents] 0]
    }

    method parents {args} {
	if {[llength $args]} {
	    return [dict filter $parents {*}$args]
	} else {
	    return [dict keys $parents]
	}
    }

    method add_parent {parent args} {
	variable parents;
	dict set parents $parent $args
    }
    
    method del_parent {parent} {
	variable parents;
	dict unset parents $parent
    }
    
    method children {args} {
	variable children
	if {[llength $args]} {
	    return [dict filter $children {*}$args]
	} else {
	    return [dict keys $children]
	}
    }

    method child {name} {
	variable children
	if {[dict exists $children $name]} {
	    return [dict get $children $name]
	} else {
	    return ""
	}
    }

    method child? {child} {
	variable children
	return [dict exists $children $child]
    }
    
    method add_child {child args} {
	variable children
	dict set children $child $args
    }
    
    method del_child {child} {
	variable children
	[dict get $children $child] del_parent [self]
	dict unset children $child
    }
    
    method descendants {{flat 1}} {
	set descendants {}
	if {$flat} {
	    foreach child [my children] {
		lappend descendants $child {*}[$child descendants $flat]
	    }
	} else {
	    foreach child [my children] {
		lappend descendants $child [$child descendants $flat]
	    }
	}
	Debug.hierarchy {[self] descendants: $descendants}
	return $descendants
    }
    
    method ancestors {{flat 1}} {
	set ancestors {}
	if {$flat} {
	    foreach p [my parents] {
		lappend ancestors $p {*}[$p ancestors $flat]
	    }
	} else {
	    foreach p [my parents] {
		lappend ancestors $p [$p ancestors $flat]
	    }
	}
	return $ancestors
    }
    
    method create {name args} {
	set child [[info object class [self]] create $name {*}$args]
	my add_child $child; $child add_parent [self]
	return $child
    }
    
    destructor {
	foreach parent [my parents] {
	    $parent del_child [self]
	}
	foreach child [my children] {
	    $child del_parent [self]
	}
	next?
    }
    
    constructor {args} {
	Debug.hierarchy {[self] constructing Hierarchy $args}
	variable children {}
	variable parents {}

	if {[dict exists $args parents]} {
	    foreach p [dict get $args parents] {
		my add_parent $p
	    }
	    dict unset args parents
	}
	if {[dict exists $args parent]} {
	    my add_parent [dict get $args parent]
	    dict unset args parent
	}

	foreach parent [dict keys $parents] {
	    $parent add_child [self]
	}

	next? {*}$args
    }
}

class create ::oo::RefCount {
    method protect {} {
	variable refcount
	incr refcount
    }
    method release {} {
	variable refcount
	if {[incr refcount -1] <= 0} {
	    [self] destroy
	}
    }

    constructor {args} {
	Debug.refcount {[self] RefCount constructed $args}
	variable refcount 0
	next? {*}$args
    }
}
