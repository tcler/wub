# Extra useful dict commands

package provide Dict 1.0

namespace eval Dict {

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

    # unset a dict element if it exists
    proc unset? {var args} {
	upvar 1 $var dvar
	set val [lindex $args end]
	set name [lrange $args 0 end-1]
	if {[dict exists $dvar {*}$name]} {
	    dict unset dvar $name
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

    # fill a dict with default key/value pairs as defaults
    # if a key already exists, it is unperturbed.
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

    # return dict as list sorted by key
    proc keysorted {dict args} {
	set result {}
	foreach key [lsort {*}$args [dict keys $dict]] {
	    lappend result $key [dict get $dict $key]
	}
	return $result
    }

    # strip a set of keys from a dict
    proc strip {var args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	upvar 1 $var dvar
	foreach key $args {
	    if {[dict exists $dvar $key]} {
		dict unset dvar $key
	    }
	}
    }

    # use a dict as a cache for the value of the $args-expression
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

    # return the dict subset specified by args
    proc subset {dict args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	return [dict filter $dict script {k v} {
	    expr {$k in $args}
	}]
    }

    # return the values specified by args
    proc getall {dict args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	return [dict values [dict filter $dict script {k v} {
	    expr {$k in $args}
	}]]
    }

    # convert directory to dict
    proc dir {dir {glob *}} {
	set content {}
	foreach file [glob -nocomplain -directory $dir $glob] {
	    while {[file type $file] eq "link"} {
		set file [file readlink $file]
	    }

	    switch -- [file type $file] {
		directory {
		    set extra "/"
		}
		file {
		    set extra ""
		}
		default {
		    continue
		}
	    }

	    catch {unset attr}
	    file stat $file attr
	    set name [file tail $file]
	    dict set content $name [array get attr]
	    foreach {x y} [file attributes $file] {
		dict set content $name $x $y
	    }
	    foreach {x y} [dict get $content $name] {
		if {[string match *time $x]} {
		    dict set content $name $x [clock format $y -format {%Y-%m-%d %H:%M:%S}]
		}
		if {$x eq "size"
		    && [file type $file] eq "directory"
		} {
		    dict set content $name $x [expr {[dict get $content $name nlink] - 2}]
		}
	    }
	    dict set content $name name [<a> href $name $name$extra]
	}
	return $content
    }

    proc vars {dvar args} {
	set script [list dict with $dvar]
	foreach a $args {
	    lappend script $a $a
	}
	uplevel 1 $script {{}}
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
