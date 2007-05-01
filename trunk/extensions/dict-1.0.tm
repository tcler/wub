package require extend

package provide dict 1.0

# Extra useful dict commands
extend dict {

    # return a dict element, or {} if it doesn't exist
    proc get? {dict args} {
	if {$args eq {}} {
	    return {}
	}
	if {[dict exists $dict {*}$args]} {
	    return [dict get $dict {*}$args]
	} else {
	    return {}
	}
    }

    # set a dict element, only if it doesn't already exist
    proc set? {var args} {
	upvar 1 $var dvar
	set val [lindex $args end]
	set name [lrange $args 0 end-1]
	
	if {![dict exists $dvar {*}$name]} {
	    dict set dvar {*}$name $v
	    return $args
	} else {
	    return {}
	}
    }

    # modify a dict var with the args-dict given
    proc modify {var args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	upvar 1 $var dvar
	set dvar [dict merge $dvar $args]
    }

    proc defaults {var args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	upvar 1 $var dvar
	foreach {name val} $args {
	    dict set? dvar $name $val
	}
    }

    # trim the given chars from a dict's keyset
    proc trimkey {dict {trim -}} {
	dict for {key val} $dict {
	    set nk [string trim $key $trim]
	    if {$nk ne $key} {
		dict set dict $nk $val
		dict unset dict $key
	    }
	}
	return $dict
    }

    # return dict keys, sorted by some subkey value
    proc subkeysort {dict subkey args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	# build a key/value list where value is extracted from subdict
	set kl {}
	dict for {key val} $dict {
	    lappend kl [list $key [dict get $val $subkey]]
	}

	# return keys in specified order
	set keys {}
	foreach el [lsort -index 1 {*}$args $kl] {
	    lappend keys [lindex $el 0]
	}

	return $keys
    }

    # strip a set of keys from a dict
    proc strip {var args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	upvar 1 $var dvar
	foreach key $args {
	    dict unset dvar $key
	}
    }

    # cache - use a dict as a cache for the value of the $args-expression
    proc cache {dict name args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	upvar dict dict
	if {[info exists $dict $name]} {
	    return [dict get $dict $name]
	} else {
	    dict set dict [set retval [uplevel $args]]
	    return $retval
	}
    }

    # subset - return the dict subset specified by args
    proc subset {dict args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	return [dict filter $dict script {k v} {
	    expr {$k in $args}
	}]
    }
}
