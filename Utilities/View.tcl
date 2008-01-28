# View.tcl - Wrapper around Metakit

if {[info exists argv0] && ($argv0 eq [info script])} {
    lappend auto_path /usr/lib/tcl8.5/Mk4tcl ../extensions/
}

lappend auto_path /usr/lib/tcl8.5/Mk4tcl
package require Mk4tcl
package require functional
package require Debug

Debug off view 10

package provide View 1.0
proc ::echo {args} {
    puts stderr "ECHO: $args"
}

namespace eval View {
    variable cmd2v; array set cmd2v {}

    # _unknown - map commands to underlying mktoo view command
    proc _unknown {cmd subcommand args} {
	variable cmd2v
	#puts stderr "unknown: [array get cmd2v]"
	#set scmd [list {*}$cmd2v($cmd) {*}$args]
	set scmd [list $cmd2v($cmd) $subcommand]
	#puts stderr "unknown: $scmd from '$cmd' '$args'"
	return $scmd
    }

    # incr a field
    proc _incr {view index field {qty 1}} {
	Debug.view {incr $view!$index $field $qty}
	mk::set $view!$index $field [expr {[mk::get $view!$index $field] + $qty}]
    }

    # lappend to a field
    proc _lappend {view index field args} {
	Debug.view {lappend $view!$index $field $args}
	set value [mk::get $view!$index $field]
	lappend value {*}$args
	mk::set $view!$index $field $value
    }

    # append values to field as dict/value pairs
    proc _dlappend {view index field args} {
	Debug.view {dlappend $view!$index $field $args}
	set value [mk::get $view!$index $field]
	dict set value {*}$args
	mk::set $view!$index $field $value
	return $value
    }

    # get a field's value
    proc _get {view index args} {
	Debug.view {get $view!$index $args}
	return [mk::get $view!$index {*}$args]
    }

    # set a field's value
    proc _set {view index args} {
	if {[llength $args] eq 1} {
	    set args [lindex $args 0]
	}
	Debug.view {set $view!$index keys: '[dict keys $args]'}
	#Debug.view {set $view!$index args: '[string range $args 0 80]...'} 20
	return [mk::set $view!$index {*}$args]
    }

    # lselect - perform a select over view yielding a list of indices
    proc _lselect {view args} {
	if {[llength $args] eq 1} {
	    set args [lindex $args 0]
	}
	Debug.view {lselect $view $args}
	return [mk::select $view {*}$args]
    }

    # update selected rows with a script's values
    proc _update {_view _select _script args} {
	set _kv {}
	if {$args eq {}} {
	    set args [map [lambda {prop} {
		lindex [split $prop :] 0
	    }] [split [mk::view info $_view]]]
	}
	foreach arg $args {
	    lappend _kv $arg $arg
	}

	foreach _index [mk::select $_view {*}$args] {
	    set _record [mk::get $_view!$_index]
	    switch [catch {
		dict update _record {*}$_kv $_script
	    } _result _eo] {
		1 {# error - re-raise the error
		    return -options $_eo $_result
		}

		3 {# break
		    break
		}

		4 {# continue
		    continue
		}

		2 {# return
		    return $_result
		}

		default {# normal
		    mk::set $_view!$_index {*}[dict filter $_record {_k _v} {
			expr {$_k in $args}
		    }]
		}
	    }
	}
    }

    # with - perform script over selected records
    proc _with {view script args} {
	if {[llength $args] eq 1} {
	    set args [lindex $args 0]
	}
	set results {}
	foreach _index [mk::select $view {*}$args] {
	    switch [catch {
		uplevel 1 [string map [list %R [mk::get $view!$_index]] {
		    set _ [list %R]
		}]
		uplevel 1 [list dict with _ $script]
	    } result eo] {
		1 {# error - re-raise the error
		    return -options $eo $result
		}

		3 {# break
		    break
		}

		4 {# continue
		    continue
		}

		2 {# return
		    return $result
		}

		default {# normal
		    lappend results $_index $result
		}
	    }
	}
	return $results
    }

    # store updates in db
    proc _store {view args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}

	# we now have a list of index/value pairs
	foreach {i v} $args {
	    mk::set $view!$i {*}$v
	}
    }

    # append record to view
    proc _append {view args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	set cursor [mk::row append $view {*}$args]
	lassign [split $cursor !] row result
	Debug.view {append $view keys: '[dict keys $args]' -> $result}
	return $result
    }

    # set metakit layout from pretty form
    proc pretty {layout} {
	# remove comments
	set r ""
	foreach l [split $layout \n] {
	    set l [string trim [lindex [regexp -inline {[^\#]*} $l] 0]]
	    if {$l ne {}} {
		#Debug.dblayout {layout l-ing '$l'} 3
		append r $l " "
	    }
	}

	set pretty ""
	set accum ""
	#set r [string map [list \{\  \{ \ \} \}] $r]
	#Debug.dblayout {layout r: $r} 2
	
	foreach el $r {
	    set el [string trim $el]
	    if {$el eq ""} continue
	    set el [string trim $el "\""]
	    lappend accum $el
	    #Debug.dblayout {layout accumulating: '$accum'} 3
	    if {![info complete $accum]} {
		continue
	    }

	    if {[llength [lindex $accum 0]] == 1} {
		#Debug.dblayout {layout name: '$el' / $accum} 3
		
		lassign [split $accum :] name t
		set accum ""
		
		if {$t eq ""} {
		    set t ""
		} else {
		    set t ":$t"
		}
		
		lappend pretty ${name}${t}
	    } else {
		#Debug.dblayout {layout subview: '$el' / $accum} 3
		
		# this is a subview declaration
		set name $accum
		lappend pretty [lindex $accum 0]
		set accum ""
	    }
	}
	
	#Debug.dblayout {LAYOUT: '$pretty'}
	return $pretty
    }

    # return field names within view
    proc _names {view} {
	set result {}
	foreach f [split [$view properties]] {
	    lappend result [split $f :]
	}
	return $result
    }

    # initialize view ensemble
    proc init {cmd vpath} {
	lassign [split $vpath .] db view
	set vcmd _V$cmd
	set cmd [uplevel 1 namespace current]::$cmd
	variable cmd2v;
	#set cmd2v($cmd) [namespace code $vcmd]
	set cmd2v($cmd) ::View::$vcmd
	set cmd2v([namespace current]::$vcmd) $cmd
	rename [mk::view open $db.$view] $vcmd	;# create mktoo cmd
	namespace export -clear {[A-Za-z]*}
	Debug.view {init: $cmd - $vpath}

	set map [subst {
	    lselect "_lselect $vpath"
	    with "_with $vpath"
	    update "_update $vpath"
	    store "_store $vpath"
	    append "_append $vpath"
	    get "_get $vpath"
	    set "_set $vpath"
	    incr "_incr $vpath"
	    dlappend "_dlappend $vpath"
	    names "_names $cmd"
	}]

	namespace ensemble create \
	    -command $cmd \
	    -map $map \
	    -unknown ::View::_unknown \
	    -subcommands {}

	trace add command $vcmd delete ::View::_uninit
	return $cmd
    }

    # destroy metakit view.
    proc _uninit {old new op args} {
	variable cmd2v
	#puts stderr "_uninit $old $new $op $args ([array get cmd2v])"
	catch {
	    set ulc $cmd2v($old)
	    unset cmd2v($old); unset cmd2v($ulc)
	} r eo
	rename $ulc ""
	#puts stderr "uninit: $old $ulc - '$r' ($eo)"
    }

    namespace export -clear {[A-Za-z]*}
    namespace ensemble create -subcommands {}
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    package require Mk4tcl

    if {[catch {
	mk::file views sdb
    } views]} {
	mk::file open sdb sdb.mk -shared
    }

    if {"v" ni [mk::file views sdb]} {
	mk::view layout sdb.v {time:I value date}
    }

    foreach v {foot head neck dangly_bits} {
	mk::row append sdb.v time [clock seconds] value $v
    }
    mk::file commit sdb
    #puts stderr "size: [mk::view size sdb.v]"
    puts stderr "select: [time {mk::select sdb.v value head}]"
    View init vv sdb.v
    set vp [mk::view open sdb.v]
    puts stderr "vp:[time {$vp select value head}]"
    puts stderr "vv: [time {vv lselect value head}]"
    set x [vv with {
	if {$date ne ""} continue
	list date [clock format $time]
    } {value head}]
    vv store $x
    puts stderr "0: [vv get 0]"
    vv close
    #vv moop
    puts stderr "DONE"
}
