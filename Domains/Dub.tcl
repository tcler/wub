# Dub - database thingo for Wub
package require TclOO
namespace import oo::*

package require Debug
Debug off dub 10

package require Form
package require Report
package require jQ
package require View

package provide Dub 2.0

set API(Domains/Dub) {
    {
	provides a database view for Wub
    }
}

class create Dub {
    method toplevel {r} {
	foreach v [::mk::file views [self]] {
	    lappend result [<li> [<a> $v.info $v]]
	}
	return [Http Ok $r [<ul> [join $result \n]] x-text/html-fragment]
    }

    method /info {r cmd {rec ""}} {
	if {[string is integer -strict $rec]} {
	    error "Can't get .info on a single record"
	}
	set view [my {*}$cmd]
	dict for {n v} [$view info2dict] {
	    lappend result "[<dt> $n] [<dd> $v]"
	}
	return [Http Ok $r [<dl> [join $result \n]] x-text/html-fragment]
    }

    method / {r cmd {rec ""}} {
	set view [my {*}$cmd]
	set range [split $rec -]
	if {[string is integer -strict $rec]} {
	    # single record
	    return [my record $view [$view get $rec]]
	} elseif {[llength $range]} {
	    # subrange
	    lassign $range start end
	    for {set r $start} {$r < $end} {incr r} {
		dict set result $r [$view get $r]
	    }
	} else {
	    # whole view
	    set result [$view dict]
	}

	set r [jQ $r #dubtable]
	set params {}
	return [Http Ok [Report html $result {*}$params class dubtable] x-text/html-fragment]
    }

    method /append {r cmd {rec ""}} {
	corovars query
	set q [Query flatten $query]
	set view [my {*}$cmd]
	if {[string is integer -strict $rec]} {
	    $view insert $rec {*}$q
	} else {
	    set rec [$view append]
	    $view set $rec {*}$q
	}
	return [my / $r $cmd $rec]
    }

    method /set {r cmd {rec ""}} {
	corovars query
	set q [Query flatten $query]
	set view [my {*}$cmd]
	if {[string is integer -strict $rec]} {
	    $view set $rec {*}$q
	} else {
	    error "Can't set an entire view (yet)"
	    set rec [$view append]
	    $view set $rec {*}$q
	}
	return [my / $r $cmd $rec]
    }

    method view {view} {
	if {![info exists names($view)]} {
	    set names($view) [View create [self].$view file $db]
	}
	return $names($view)
    }

    method open {view rec field} {
	if {![info exists names("$view open $rec $field")]} {
	    set names("$view open $rec $field") [[my {*}$view] open $rec $field]
	}
	return $names("$view open $rec $field")
    }

    foreach n {join union concat different intersect minus map pair product hash indexed} {
	{*}[string map [list %M $n] {
	    method %M {lhs rhs} {
		if {![info exists names("$lhs %M $rhs")]} {
		    set names("$lhs %M $rhs") [[my {*}$lhs] %M [my {*}$rhs]]
		}
		return $names("$lhs %M $rhs")
	    }
	}]
    }

    foreach n {flatten groupby ordered range} {
	{*}[string map [list %M $n] {
	    method %M {view args} {
		if {![info exists names("%M $view {*}$args")]} {
		    set names("$view %M {*}$args") [[my {*}$view] %M {*}$args]
		}
		return $names("$view %M {*}$args")
	    }
	}]
    }

    foreach n {select find} {
	{*}[string map [list %M $n] {
	    method %M {view} {
		corovars query
		set q [Query flatten $query]
		if {![info exists names("%M $view {*}$q")]} {
		    set names("$view %M {*}$q") [[my {*}$view] %M {*}$q]
		}
		return $names("$view %M {*}$q")
	    }
	}]
    }


    # V == /$rec/$field+
    # FR == (/$field/$frec)*
    # RF == /$rec/$field* /$rec

    # VIEW == /$view
    #	| VIEW V
    #	| VIEW (/(join|union|concat|different|intersect|minus|map|pair|product|hash|indexed...) VIEW)*
    #	| VIEW (/blocked|clone|copy|dup|readonly|unique|...)
    #	| VIEW /select ?$select

    # VIEW /flatten/$property
    # VIEW /groupby (/$property*)
    # VIEW /range/$from/$to
    # VIEW /ordered (/$property*)

    # VIEW /name/$name - create a named temporary

    # VIEW RF ... - get a record
    # VIEW RF.edit ?$fields - get a given record in an editable form
    # VIEW - display range of view
    # VIEW RF /save ?$fields - save content
    # VIEW RF /insert ?$fields - insert content
    # VIEW /append ?$fields - append new content
    
    # VIEW RF /edit ?$fields - get a given record in an editable form
    
    # /$view/ /$el ?$search - element of a given search

    # /report/$report/ - get the results of a canned report over a canned selection
    # /report/$report/$search - get the results of a canned report over a specified selection

    # /next - next record in a given search order
    # /prev - previous record in a given search order

    method parse1 {path {parsed {}}} {
	# collect a run of record,fieldname
	set path [lassign $path view]
	set parsed [list view $view]
	while {[llength $path]} {
	    #puts stderr "parse: '$path' ($parsed)"
	    catch {unset field}
	    catch {unset rec}
	    if {[string is integer -strict [lindex $path 0]]} {
		set path [lassign $path rec field]
		if {$field eq ""} {
		    # field eq "" ... so we have a trailing rec
		    lappend path $rec
		    return [list $path $parsed]
		} else {
		    # accumulate VIEW
		    set parsed [list open $parsed $rec $field]
		}
	    } elseif {[llength [split [lindex $path 0] -]] == 2} {
		# range of results
		set path [lassign $path rec]
		lappend path $rec
		return [list $path $parsed]
	    } else {
		set path [lassign $path op]
		switch -glob -- [string tolower $op] {
		    join - union - concat - different -
		    intersect - minus - map - pair -
		    product - hash - indexed {
			set lhs $parsed
			lassign [my parse1 $path] path rhs
			set parsed [list $op $lhs $rhs]
		    }

		    pop {
			return [list $path $parsed]
		    }

		    find - 
		    search -
		    select {
			set parsed [list $op $parsed]
		    }

		    flatten {}
		    groupby {}
		    ordered {
			set parsed [list $op $parsed [lindex $path 0]]
			set path [lrange $path 1 end]
		    }
		    range {
			set parsed [list $op $parsed [lindex $path 0] [lindex $path 1]]
			set path [lrange $path 2 end]
		    }
		    name {
			set parsed [list name [lindex $path 0] $parsed]
			set path [lrange $path 1 end]
		    }
		    default {}
		}
	    }
	}

	return [list $path $parsed]
    }

    method parse {suffix} {
	set fop /[string trim [file extension $suffix] .]
	set path [lrange [file split [file rootname $suffix]] 1 end]
	while {[llength $path] > 1} {
	    lassign [my parse1 $path] path parsed
	}

	return [list $fop $parsed {*}$path]
    }

    method do {r} {
	# calculate the suffix of the URL relative to $mount
	lassign [Url urlsuffix $r $mount] result r suffix
	if {!$result} {
	    return $r	;# the URL isn't in our domain
	}
	if {$suffix eq "/"} {
	    return [my toplevel $r]	;# special case for top level
	}

	lassign [my parse $suffix] cmd parsed rec
	return [my $cmd $r $parsed {*}$rec]
    }

    variable db mount View

    constructor {args} {
	foreach {n v} $args {
	    set [string trimleft $n -] $v
	}
    }

    destructor {
    }
}

if {0} {
    set test [Dub new]
    foreach x {
	/view/1
	/view/1/subview/.append
	/view/1/subview/2
	/view/join/view2
	/view/select
	/view/select/5
	/view/select/join/view1
	/view/ordered/3
	/view/join/view1/select
	/view/1/subview/join/view1/2/subview2/select/5
	/view/1/subview/ordered/1/select/join/view1/2/subview2/select/5
	/view/1/subview/ordered/1/select/join/view1/2/subview2/pop/select/5
	/view/1/subview/select/name/fred/join/view1/2/subview2/select/5
	/view/1/subview/select/name/fred/join/view1/2/subview2/select/5-20
    } {
	puts stderr "PARSE($x) -> [$test parse $x]"
    }
}
