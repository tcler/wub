package require Mk4tcl
package require functional

package provide View 1.0

namespace eval View {
    variable cmd2v; array set cmd2v {}

    # _unknown - map commands to underlying mktoo view
    proc _unknown {cmd args} {
	variable cmd2v
	#puts stderr "unknown: [array get cmd2v]"
	set scmd [list {*}$cmd2v($cmd) {*}$args]
	puts stderr "unknown: $scmd"
	return $scmd
    }

    proc _get {view inode} {
	return [mk::get $view!$inode]
    }
    proc _set {view inode args} {
	if {[llength $args] eq 1} {
	    set args [lindex $args 0]
	}
	return [mk::set $view!$inode {*}$args]
    }

    # lselect - perform a select over view yielding a list
    proc _lselect {view args} {
	if {[llength $args] eq 1} {
	    set args [lindex $args 0]
	}
	return [mk::select $view {*}$args]
    }

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

	foreach _inode [mk::select $_view {*}$args] {
	    set _record [mk::get $_view!$_inode]
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
		    mk::set $_view!$_inode {*}[dict filter $_record {_k _v} {
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
	foreach _inode [mk::select $view {*}$args] {
	    set _record [mk::get $view!$_inode]
	    switch [catch {
		dict with _record $script
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
		    lappend results $_inode $result
		}
	    }
	}
	return $results
    }

    proc _store {view args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	# we now have a list of inode/value pairs
	foreach {i v} $args {
	    mk::set $view!$i {*}$v
	}
    }

    # append record to view
    proc _append {view args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	return [mk::row append $view {*}$args]
    }

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

    # initialize view ensemble
    proc init {cmd vpath} {
	lassign [split $vpath .] db view
	set vcmd _V$cmd
	set cmd [uplevel 1 namespace current]$cmd
	variable cmd2v;
	set cmd2v($cmd) [namespace code $vcmd]
	set cmd2v([namespace current]::$vcmd) $cmd
	rename [mk::view open $db.$view] $vcmd	;# create mktoo cmd
	namespace export -clear {[A-Za-z]*}

	set map [subst {
	    lselect "_lselect $vpath"
	    with "_with $vpath"
	    update "_update $vpath"
	    store "_store $vpath"
	    append "_append $vpath"
	    get "_get $vpath"
	    set "_set $vpath"
	}]

	namespace ensemble create \
	    -command $cmd \
	    -map $map \
	    -unknown ::View::_unknown \
	    -subcommands {}

	trace add command $vcmd delete ::View::_uninit
    }

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
