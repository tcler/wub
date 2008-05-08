# Report - convert a dict or csv into an HTML table

if {[info exists argv0] && ([info script] eq $argv0)} {
    lappend auto_path [file dirname [file normalize [info script]]] ../Utilities/ ../extensions/
}

package require Debug
Debug off report 10
package require Html

package provide Report 1.0

namespace eval Report {
    variable defaults {
	rc 0
	sortable 0
	armour 0
	evenodd 1
	odd odd
	even even
	rowp {}
    }

    # header: process header args in report dict into HTML within report dict
    # header - list of report column headers
    # hclass - report header CSS class
    # headerp - dict mapping header to parameters for that element,
    #	including optonal title to display for that header
    proc header {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	variable defaults; set args [dict merge $defaults $args]
	Debug.report {header $args}
	if {![dict exists $args header]} {
	    if {![dict exists $args headers]} {
		return $args
	    }
	    dict set args header [dict get $args headers]
	}
	
	set h {}
	foreach t [dict get $args header] {
	    if {[dict exists $args hclass]} {
		set params [list class [dict get $args hclass]]
	    } else {
		set params {}
	    }
	    
	    if {[dict exists $args headerp] && [dict exists $args headerp $t]} {
		set thead [dict get $args headerp $t]
		if {[dict exists $thead title]} {
		    set htext [dict get $thead title]
		    dict unset thead title
		} else {
		    set htext $t
		}
		lappend params {*}$thead
	    } else {
		set htext $t
	    }
	    
	    lappend h [<th> {*}$params [string totitle $htext]]
	}
	dict append args _header [<thead> \n[<tr> \n[join $h \n]\n]\n]
	dict unset args header

	return $args
    }

    # footer: process footer args in report dict into HTML within report dict
    # footer - list of report column footers
    # fclass - report footer CSS class
    # footerp - dict mapping footer to parameters for that element,
    #	including optonal title to display for that footer
    proc footer {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	variable defaults; set args [dict merge $defaults $args]

	if {[dict exists $args footer]} {
	    set f {}
	    foreach t [dict get $args footer] {
		if {[dict exists $args fclass]} {
		    set params [list class [dict get $args fclass]]
		} else {
		    set params {}
		}

		if {[dict exists $args footerp] && [dict exists $args footerp $t]} {
		    set tfoot [dict get $args footerp $t]
		    if {[dict exists $tfoot title]} {
			set htext [dict get $tfoot title]
			dict unset tfoot title
		    } else {
			set htext $t
		    }
		    lappend params {*}$tfoot
		}

		lappend f [<th> {*}$params [string totitle $t]]
	    }
	    dict append args _footer [<tfoot> \n[<tr> \n[join $f \n]\n]\n]
	    dict unset args footer
	}
	return $args
    }

    # body: append some elements to the report
    # rclass - CSS class for body rows
    # eclass - CSS class for body elements
    # datap - parameters for body elements
    # armour - HTML armour elements?
    # evenodd - mark even and odd rows differently?
    # even - CSS class for even rows
    # odd - CSS class for odd rows
    # rowp - dict map of "columnheader,glob" to parameters for matching elements
    # 
    proc body {data args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	variable defaults; set args [dict merge $defaults $args]

	dict for {k v} $data {
	    set row {}
	    if {[dict exists $args rclass]} {
		set rparams [list class [dict get $args rclass]]
	    } else {
		set rparams {}
	    }
	    
	    if {[dict exists $args evenodd] && [dict get $args evenodd]} {
		dict incr args rc
		if {[dict get $args rc] % 2} {
		    lappend rparams class [dict get $args even]
		} else {
		    lappend rparams class [dict get $args odd]
		}
	    }
	    # do column content string match for row parameters
	    dict for {spec val} [Dict get? $args rowp] {
		set match [lassign [split $spec ,] col]
		if {[dict exists $v $col]
		    && [string match $match [dict get $v $col]]
		} {
		    lappend rparams {*}[dict get $args rowp $spec]
		}
	    }
	    
	    # now traverse the value as a dict
	    foreach th [dict get $args headers] {
		if {[dict exists $args eclass]} {
		    set params [list class [dict get $args eclass]]
		} else {
		    set params {}
		}
		
		if {[dict exists $args datap] && [dict exists $args datap $th]} {
		    lappend params {*}[dict get $args datap $th]
		}

		if {[dict exists $v $th]} {
		    if {[dict exists $args armour] && [dict get $args armour]} {
			set datum [armour [dict get $v $th]]
		    } else {
			set datum [dict get $v $th]
		    }
		    lappend row [<td> {*}$params $datum]
		} else {
		    lappend row [<td> {*}$params {}]
		}
	    }
	    dict append args body [<tr> {*}$rparams \n[join $row \n]] \n
	}

	return $args
    }

    # interpolate some raw text into the body of a report
    # armour - HTML armour interpolation?
    proc interpolate {text args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	variable defaults; set args [dict merge $defaults $args]
	dict append args body $text
	return $args
    }

    # returns a table with all kinds of options
    # class - table CSS class
    # header - headers for table
    # footer - footers for table
    # sortable - will table be marked as sortable?
    proc html {data args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	variable defaults; set args [dict merge $defaults $args]

	if {![dict exists $args _header]} {
	    set args [header {*}$args]
	}

	if {![dict exists $args _footer]
	    && [dict exists $args footer]
	} {
	    set args [footer {*}$args]
	}

	set args [body $data $args]

	set classT {}
	if {[dict exists $args class]} {
	    lappend classT [dict get $args class]
	}
	if {[dict get $args sortable]} {
	    lappend classT sortable
	}
	
	return [<table> {*}$classT "\n[dict get $args _header]\n[dict get $args body]\n[Dict get? $args _footer]\n"]
    }

    # convert a text formatted suitably for csv into a list containing:
    # header for Report and data for Report
    proc csv2dict {csv args} {
	package require csv
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	if {[dict exists $args -alternate]} {
	    set alt "-alternate"
	} else {
	    set alt ""
	}

	if {[dict exists $args sep]} {
	    set sep [dict get $args sep]
	} else {
	    set sep ,
	}
	if {[dict exists $args quote]} {
	    set quote [dict get $args quote]
	} else {
	    set quote \"
	}

	set x {}
	foreach line [split $csv \n] {
	    if {[string trim $line] eq ""} continue
	    lappend x [::csv::split {*}$alt $line $sep $quote]
	}
	set data [lassign $x h1]
	foreach h $h1 {
	    lappend header [string trim $h]
	}
	if {[dict exists $args key]} {
	    set key [lsearch $header [dict get $args key]]
	    if {$key == -1} {
		set key 0
	    }
	} else {
	    set key 0
	}

	set result {}
	foreach r $data {
	    set row {}
	    foreach h $header el $r {
		set h [string trim $h]
		set el [string trim $el]
		lappend row $h $el
	    }
	    lappend result [string trim [lindex $r $key]] $row
	}
	return [list $result headers $header]
    }

    proc init {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args]
	}
	if {[llength $args]} {
	    variable {*}$args
	}
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

if {[info exists argv0] && ([info script] eq $argv0)} {
    Report init 
    set csv {name,address,phone
	fred, 11 stone drive, 123
	wilma, 11 stone drive, 123
	barney, 13 stone drive, 345
    }
    set r [Report csv2dict $csv]
    #puts stderr $r
    set params {
	sortable 1
	class table
	hclass header
	fclass footer
	rclass row
	eclass el
	evenodd 1
	footer {name address phone}
    }
    set params1 {
	footerp {name {class fname}}
	headerp {name {class hname}}
	rowp {name,fred {class fred}}
    }
    puts [Report html {*}$r {*}$params {*}$params1]
}
