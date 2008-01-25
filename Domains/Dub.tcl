# Dub - Db toy

package provide Dub 1.0

package require Form
package require View
package require Cookies
package require Responder

namespace eval Dub {
    variable db /tmp/dub.db
    variable toplevel
    variable prefix ..

    proc /default {r args} {
	variable prefix
	return [Http NotFound $r [subst {
	    [<h1> "Dub - A Metakit Toy"]
	    [<a> href ${prefix}/create "Create View"]
	    [<ul> [Foreach view [mk::file views db] {
		[<li> [<a> href ${prefix}/view?view=$view $view]]
	    }]]
	}]]
    }

    proc /create {r {view ""} {names {}} {layouts {}}} {
	if {$view eq "" || [lindex $names 0] eq ""} {
	    set ltype [<select> layouts title "type" {
		[<option> value I integer]
		[<option> value S string]
		[<option> value L long]
		[<option> value F float]
		[<option> value D double]
		[<option> value B binary]
	    }]
	    variable prefix
	    return [Http Ok $r [subst {
		[<form> create action ${prefix}/create {
		    [<fieldset> "Create A View" {
			[<text> view legend "View Name:" $view]
			[<submit> create "Create View"]
			<table>
			[string repeat "<tr><td>[<text> names]</td><td>${ltype}</td></tr>\n" 10]
			</table>
		    }]
		}]
	    }] x-text/html-fragment]
	}

	foreach name $names layout $layouts {
	    if {$name ne ""} {
		if {$layout eq ""} {
		    set layout S
		}
		lappend l "$name:$layout"
	    }
	}

	mk::view layout db.$view $l
	variable views
	catch {View init v$view db.$view}
	set views($view) v$view

	mk::file commit db
	return [/default $r]
    }
    variable views; array set views {}

    proc /del {r view id} {
	mk::row delete db.$view!$id
	mk::file commit db
	return [/view $r $view]
    }

    proc /add {r view {name {}} {value {}}} {
	variable views
	if {![info exists views($view)]} {
	    return [/create $r $view]
	}
	set fields {}
	foreach n $name v $value {
	    lappend fields $n $v
	}
	v$view append {*}$fields

	mk::file commit db

	return [/view $r $view]
    }

    proc /view {r view {op {}} {select {}}} {
	variable views
	if {![info exists views($view)]} {
	    return [/create $r $view]
	}

	variable prefix
	switch -- $op {
	    default {
		# view
		set len [v$view size]
		set dict {}
		for {set i 0} {$i < $len} {incr i} {
		    dict set dict $i [v$view get $i]
		    dict set dict $i edit [<a> href ${prefix}/edit?view=$view&id=$i Edit]
		    dict set dict $i delete [<a> href ${prefix}/del?view=$view&id=$i Del]
		}

		set layout {}
		foreach l [mk::view layout db.$view] {
		    lappend layout [lindex [split $l :] 0]
		}

		set table [Html dict2table $dict [list {*}$layout]]

		# use default view for new record
		set form [<form> record action ${prefix}/add {
		    [<fieldset> record {
			[<hidden> view $view]
			[Foreach l $layout {
			    [<hidden> name $l]
			    [<text> value legend $l:]
			}]
		    }]
		}]
		set rest [<a> href ${prefix}/ Back]
		return [Http Ok $r "$table\n$form\n$rest" x-text/html-fragment]
	    }
	}
    }

    proc init {args} {
	if {$args ne {}} {
	    variable {*}$args
	}

	# try to open wiki view
	if {[catch {
	    mk::file views db
	    # we expect threads to be created *after* the db is initialized
	    variable toplevel 0
	} vlist]} {
	    # this is the top level process, need to create a db
	    variable toplevel 1
	    variable db
	    mk::file open db $db -shared
	} else {
	    variable views
	    foreach v $vlist {
		set views($v) [View init v$v db.$v]
	    }
	}
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
