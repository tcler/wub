# Metakit --
#
# A Wub domain to return contents of a metakit.

package require TclOO
namespace import oo::*

package require Debug
Debug off metakit 10

package require csv
package require View

package require Html
package require Report

package provide Metakit 1.0

set API(Metakit) {
    {
	Domain to present metakit views

	== Note: the views themselves are not constructed by this domain ==
    }
}

class create Metakit {
    variable db views csv limit rparams limit

    method do {req} {
	if {[dict exists $req -suffix]} {
	    # caller has munged path already
	    set suffix [dict get $req -suffix]
	} else {
	    # assume we've been parsed by package Url
	    # remove the specified prefix from path, giving suffix
	    set suffix [Url pstrip $mount [string trimleft [dict get $req -path] /]]
	    Debug.metakit {suffix:$suffix url:$mount}
	    if {($suffix ne "/") && [string match "/*" $suffix]} {
		# path isn't inside our domain suffix - error
		Debug.metakit {[dict get $req -path] is outside domain suffix $suffix}
		return [Http NotFound $req]
	    }
	    dict set req -suffix $suffix
	}

	# use suffix to determine which view
	lassign [split $suffix .] view ext

	# ensure the view is permitted
	if {$view ni $views} {
	    return [NotFound $req]
	}

	# use query to determine fieldset
	set display {}
	set select {}
	set flags {}

	# get relevant field descriptors from the query
	set q [Query parse $req]
	foreach {n v m} [Query nvmlist $q] {
	    catch {unset meta}
	    array set meta $m
	    if {[info exists meta(unassigned)]} {
		if {[string match {-*} $n]} {
		    # set flag
		    dict set flags $n 1
		} else {
		    # just a display field
		    dict set display $meta(-count) $n
		}
	    } else {
		# select clause element
		dict set select $n $v
	    }
	}

	# get display fields in query order
	set d_fields {}
	foreach {n} [lsort -integer [dict keys display]] {
	    lappend d_fields [dict get $display $n]
	}

	# calculate the selector from select clause elements
	set selector {}
	dict for {n v} $select {
	    switch -glob -- $n {
		*% {
		    # keyword match
		    lappend selector -keyword [string trim $n %] $v
		}
		*[*] {
		    # glob match
		    lappend selector -globnc [string trim $n *] $v
		}
		default {
		    lappend selector $n $v
		}
	    }
	}
	
	$view select {*}$selector as sV
	set dict [$sV dict {*}$d_fields]

	switch -- [string tolower $ext] {
	    html {
		set result [Report html $dict {*}$rparams headers $d_fields]
		return [Http Ok $req $result text/x-html-fragment]
	    }

	    default {
		set result [::csv::join $d_fields]\n
		dict foreach {n v} $dict {
		    append result [::csv::join [dict values $v]] \n
		}
		return [Http Ok $req $result text/csv]
	    }
	}
    }

    constructor {args} {
	set db ""	;# Metakit db name
	set views {}	;# views which can be accessed by the domain
	set rparams {
	    sortable 1
	    evenodd 0
	    class table
	    tparam {title ""}
	    hclass header
	    hparam {title "click to sort"}
	    thparam {class thead}
	    fclass footer
	    tfparam {class tfoot}
	    rclass row
	    rparam {}
	    eclass el
	    eparam {}
	    footer {}
	}
	set limit 0

	foreach {n v} $args {
	    set [string trimleft $n -] $v
	}
    }
}
