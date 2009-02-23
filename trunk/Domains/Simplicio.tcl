package require Debug
Debug on simplicio 10

package require Url
package require Query
package require Http
package require Html
package require Color
package require fileutil

package provide Simplicio 1.0
set API(Simplicio) {
    {experimental SVG iconset}
}

namespace eval ::Simplicio {
    variable xml {<?xml version="1.0" encoding="UTF-8" standalone="no"?>}
    variable svg {<svg xmlns="http://www.w3.org/2000/svg" width="64px" height="64px">}

    variable style "fill:#%color;fill-opacity:1;fill-rule:nonzero;stroke:none"
    variable icons [fileutil::cat [file join [file normalize [file dirname [info script]]] simplicio-icons.tcl]]

    proc render {name args} {
	set width 64; set height 64
	if {$args ne {}} {
	    dict with args {}
	}
	variable style
	variable icons
	variable ignore
	set content ""
	foreach {color path} [dict get $icons $name] {
	    #if {$color in $ignore} continue
	    append content [<path> style [string map [list %color $color] $style] d $path {}] \n
	}

	set xscale [expr {$width/64.0}]
	set yscale [expr {$height/64.0}]
	lappend gargs transform "scale($xscale,$yscale)"

	set content [<g> {*}$gargs $content]
	set content [<svg> xmlns http://www.w3.org/2000/svg width ${width}px height ${height}px $content]
	Debug.simplicio {width:$width height:$height / $args / $content}
	return $content
    }

    variable ignore {ec2024 f1b326 279f48 5ebb67 292f6d a7cae3 a7cae3 292f6d 000000}

    proc get {icon} {
	variable icons
	return [dict get $icons $icon]
    }

    proc all {r} {
	variable icons
	variable ignore
	set content {}
	foreach n [lsort -dictionary [dict keys $icons]] {
	    set line $n
	    lappend line [<object> width 64px height 64px data $n ""]
	    foreach {col svg} [dict get $icons $n] {
		if {$col in $ignore} {
		    lappend line *$col
		    continue
		} else {
		    lappend line [lindex [Color webToHsv $col] 0]/$col
		}
	    }
	    lappend content [<td> [join $line </td><td>]]
	}
	return [Http Ok $r [<table> "<tr>[join $content </tr>\n</tr>]</tr>"]]
    }

    proc do {r} {
	# compute suffix
	variable mount
	if {[dict exists $r -suffix]} {
	    # caller has munged path already
	    set suffix [dict get $r -suffix]
	    Debug.simplicio {-suffix given $suffix}
	} else {
	    # assume we've been parsed by package Url
	    # remove the specified prefix from path, giving suffix
	    set path [dict get $r -path]
	    set suffix [Url pstrip $mount $path]
	    Debug.simplicio {-suffix not given - calculated '$suffix' from '$mount' and '$path'}
	    if {($suffix ne "/") && [string match "/*" $suffix]} {
		# path isn't inside our domain suffix - error
		return [Http NotFound $r]
	    }
	}

	# catalog
	if {$suffix eq "/"} {
	    return [all $r]
	}

	# check existence of icon
	variable icons
	set iname [file rootname [file tail $suffix]]
	if {![dict exists $icons $iname]} {
	    # path isn't inside our domain suffix - error
	    return [Http NotFound $r]
	}

	set query [Query parse $r]
	dict set r -Query $query
	Debug.simplicio {args: ([Query flatten $query]) '$query'}
	set icon [render $iname {*}[Query flatten $query]]

	return [Http Ok $r $icon image/svg+xml]
    }

    variable mount /simplicio/
    proc new {args} {
	variable {*}$args
	return ::Simplicio
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
