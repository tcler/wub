# db_tie.tcl --
#
#	Tie arrays to Db
#
# Copyright (c) Colin McCormack 15Nov2005
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# 
# RCS: @(#) $Id: db_tie.tcl,v 1.3 2005/09/28 04:51:24 andreas_kupries Exp $

# ### ### ### ######### ######### #########
## Requisites

package provide db_tie 1.0

package require snit
package require tie
package require Db

# ### ### ### ######### ######### #########
## Implementation

snit::type db_tie {

    component view

    pragma -hastypemethods no
    pragma -hasinfo        no
    pragma -simpledispatch yes

    # ### ### ### ######### ######### #########
    ## API : Construction & Destruction

    constructor {v} {
	set view $view
    }

    destructor {}

    # ### ### ### ######### ######### #########
    ## API : Data source methods

    method get {} {
	set result {}
	foreach rec {} {
	    lappend result [dict get $rec name] [dict get $rec value]
	}
	return $result
    }

    method set {dict} {
	for dict {n v} $dict {
	    if {[catch {$view find name $n} cursor]} {
		$view append name $n value $v
	    } else {
		$view set $cursor name $n value $v
	    }
    }

    method unset {{pattern *}} {
	[$view select name $pattern] as select
	set recs {}
	$select loop cursor {
			     lappend recs $cursor
			 }
	foreach cursor [lsort -descending $recs] {
	    $view delete $cursor
	}
    }

    method names {} {
	set names {}
	$self foreach v {} {
	    lappend names [dict get $v name]
	}
	return $names
    }

    method size {} {
	return [$view size]
    }

    method getv {index} {
	return [$self get [$self find name $index] value]
    }

    method setv {index value} {
	if {[catch {$view find name $index} cursor]} {
	    $view append name $index value $value
	} else {
	    $view set $cursor name $index value $value
	}
    }

    method unsetv {index} {
	$view delete [$view find name $index]
    }

}

::tie::register db_tie as db_tie
