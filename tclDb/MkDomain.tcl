# MkDomain --
#
# A Wub domain to return contents of a metakit.

package require snit
package require csv
package require Db

package provide MkDomain 1.0

snit::type MkDomain {
    option -db ""	;# Metakit db name
    option -views {}	;# views which can be accessed by the domain
    option -csv ,	;# csv separator

    method fetch {req} {
	# use suffix to determine which view
	set suffix [dict get $req -suffix]
	lassign [split $suffix .] view ext

	# ensure the view is permitted
	if {$options(-views) ne {}} {
	    if {$view ni $options(-views)} {
		do respond NotFound $req
	    }
	}

	set layout [mk::view layout $view]
	Debug.db {$view - $layout}

	# use query to determine fieldset
	array set display {}
	array set select {}
	array set flags {}

	# get relevant field descriptors from the query
	set q [Query parse $req]
	foreach {n v m} [Query nvmlist $q] {
	    catch {unset meta}
	    array set meta $m
	    if {[info exists meta(unassigned)]} {
		if {[string match {-*} $n]} {
		    # set flag
		    set flags($n) 1
		} else {
		    # display field
		    set display($meta(-count)) $n
		}
	    } else {
		# select clause element
		set select($n) $v
	    }
	}

	# get display fields in query order
	set d_fields {}
	foreach {n} [lsort -integer [array names display]] {
	    lappend d_fields $display($n)
	}

	# calculate the selector from select clause elements
	set selector {}
	foreach {n v} [array get select] {
	    switch -glob -- $n {
		*% {
		    # keyword
		    lappend selector -keyword [string trim $n %] $v
		}
		*[*] {
		    # glob
		    lappend selector -globnc [string trim $n *] $v
		}
		default {
		    lappend selector $n $v
		}
	    }
	}

	switch -- [string tolower $ext] {
	    html {
		set result "<html><head></head>"
		append result "<body>"
		append result "<table>"
		append result "<tr><th>"
		append result [join $d_fields </th><th>]
		append result "</th></tr>" \n
		foreach idx [$db.$view lselect {*}$selector] {
		    append result <tr><td>
		    set fields [$db.$view get {*}$d_fields]
		    append result [join $field </td><td>]
		    append result "</td></tr>" \n
		}
		append result "</table>"
		append result "</body>"

		return [dict merge $req \
			    content-type text/html \
			    -content $result]
	    }

	    default {
		set result [::csv::join $d_fields]
		foreach idx [$db.$view lselect {*}$selector] {
		    lappend result [::csv::join [$db.$view get {*}$d_fields]]
		}
		return [dict merge $req \
			    content-type text/x-csv \
			    -content [join $result \n]]
	    }

	}
    }

    constructor {args} {
	$self configurelist $args
    }
}
