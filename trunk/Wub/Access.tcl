# Access.tcl - log access dicts into a metakit view.

package require Mk4tcl
package require Debug
package provide Access 1.0

namespace eval Access {
    variable db
    variable count 0

    proc open {{_db access}} {
	variable db $_db
	mk::view open $db.log logv
	if {[logv size]} {
	    variable count [logv get end -logged]
	}
    }

    proc init {name {db access}} {
	if {[lsearch -exact [mk::file open] $db] == -1} {
	    mk::file open $db $name -shared
	    mk::view layout $db.log {
		_logged:I
		_code:I
		_received:I
	    }
	}
	return [open $db]
    }

    proc log {r} {
	variable db
	dict for {key val} $r {
	    if {[string match -* $key]} {
		dict set r1 _[string range $key 1 en] $val
	    } else {
		dict set r1 $key $val
	    }
	}
	foreach f {_content _entity _query {_Query C} _logged _header} {
	    catch {dict unset r1 {*}$f}
	}

	if {![dict exists $r -logged]} {
	    variable count
	    set cnt [logv size]
	    dict set r -logged $cnt
	    set i [logv insert end {*}$r1]
	    puts stderr "Access: logged $i"
	} else {
	    logv set [dict get $r -logged] {*}$r1
	    puts stderr "Access: relogged [dict get $r -logged]"
	}
	return $r
    }

    proc dump {file args} {
	::fileutil::appendToFile $file [join [select $args] \n]
    }

    proc select {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	set result {}
	set select [logv select {*}$args]
	$select loop r {
			lappend result [string map {\n \\n} [logv get [$select get $r index]]]
	}
	$select close
	return $result
    }

    proc commit {} {
	variable db
	mk::view layout $db.log [logv properties]
	mk::file commit $db
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    Access init access.db

    Access log {a 1 b 2 c 3}
    Access log {b 3 c 2 d 3}
    puts [Access select]
    Access commit
}
