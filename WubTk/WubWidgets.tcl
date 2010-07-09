package require TclOO
package require Debug
Debug on wubwidgets 10

package provide WubWidgets 1.0

namespace eval ::WubWidgets {
    oo::class create widget {
	method widget {} {
	    return [string trim [namespace tail [self]] .]
	}

	method command {} {
	    ::apply {{} [my cget command] [uplevel 1 {namespace current}]}
	}

	# style - construct an HTML style form
	method style {} {
	    variable background; variable foreground
	    return "background-color: $background; color: $foreground;"
	}

	# cget - get a variable's value
	method cget {n} {
	    set n [string trim $n -]
	    variable $n
	    return [set $n]
	}

	# configure - set variables to their values
	method configure {args} {
	    dict for {n v} $args {
		set n [string trim $n -]
		variable $n $v
	    }
	}

	constructor {args} {
	    Debug.wubwidgets {construct [self] ($args)}
	    my configure {*}$args
	}
    }

    oo::class create buttonC {
	method render {id} {
	    set command [my cget command]
	    if {$command ne ""} {
		set class {class cmd}
	    } else {
		set class {}
	    }
	    return [<button> [my widget] id $id {*}$class style [my style] [my cget -text]]
	}

	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge {text ""  textvariable ""
		foreground black background white justify left
		command ""
	    } $args]
	}
    }

    oo::class create labelC {
	method render {id} {
	    set var [my cget textvariable]
	    if {$var ne ""} {
		corovars $var
		set val [set $var]
	    } else {
		set val [my cget text]
	    }
	    return [<div> id $id style [my style] $val]
	}
	
	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge {text ""  textvariable ""
		foreground "black" background "white" justify "left"
	    } $args]
	}
    }
    
    oo::class create entryC {
	method render {id} {
	    set var [my cget -textvariable]
	    corovars $var
	    
	    if {[info exists $var]} {
		set val [set $var]
		set class {class var}
	    } else {
		set val ""
		set class {}
	    }

	    set disabled ""
	    if {[my cget -state] ne "normal"} {
		set disabled disabled
	    }

	    return [<text> [my widget] id $id {*}$class {*}$disabled style [my style] $val]
	}

	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge {text ""  textvariable ""
		foreground black background white justify left
		state normal
	    } $args]
	}
    }
    
    oo::class create textC {
	method get {{start 0} {end end}} {
	    return [string range [my cget text] $start $end]
	}
	
	method delete {{start 0} {end end}} {		
	    set text [my cget text]
	    set text [string range $text 0 $start-1][string range $text $end end]
	    my configure text $text
	    return $text
	}

	method insert {start newtext} {
	    set start 0
	    set text [my cget text]
	    set text [string range $text 0 $start]${newtext}[string range $text ${start}+1 end]
	    my configure text $text
	    return $text
	}
	
	method render {widget} {
	    set state [my cget -state]
	    set val [my get]
	    
	    set disabled ""
	    if {[my cget -state] ne "normal"} {
		set disabled disabled
	    }
	    
	    return [<textarea> [my widget] {*}$disabled style [my style] rows [my cget -height] cols [my cget -width] $val]
	}
	
	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge {text ""  textvariable ""
		foreground black background white justify left
		state normal height 10 width 40
	    } $args]
	}
    }

    oo::class create wmC {
	method title {{widget .} args} {
	    if {$widget != "."} {return}
	    variable title
	    if {[llength $args]} {
		set title [lindex $args 0]
	    }
	    return $title
	}
    }

    # grid store grid info in an x/y array gridLayout(column.row)
    oo::class create gridC {
	method render {id} {
	    global args sessid

	    variable maxheight; variable maxwidth
	    variable grid
	    set rows {}
	    set interaction {};
	    for {set row 0} {$row < $maxwidth} {incr row} {
		set cols {}
		for {set col 0} {$col < $maxheight} {} {
		    if {[dict exists $grid $row $col]} {
			set el [dict get $grid $row $col]
			dict with el {
			    lappend cols [<td> id colspan $colspan [uplevel 1 [list $widget render grid_${row}_$col]]]
			}
			incr col $colspan
		    } else {
			lappend cols [<td> "&nbsp;"]
			incr col
		    }
		}
		# now we have a complete row - accumulate it
		lappend rows [<tr> align center valign middle [join $cols \n]]
	    }
	    
	    set content [<form> $id [<table> [join $rows \n]]]
	    
	}
	
	method configure {widget args} {
	    #set defaults
	    set column 0
	    set row 0
	    set colspan 1
	    set rowspan 1
	    set sticky ""
	    set in ""
	    
	    foreach {var val} $args {
		set [string trim $var -] $val
	    }
	    
	    set width [expr {$column + $colspan}]
	    set height [expr {$row + $rowspan}]
	    
	    variable maxwidth
	    if {$width > $maxwidth} {
		set maxwidth $width
	    }
	    
	    variable maxheight
	    if {$height > $maxheight} {
		set maxheight $height
	    }
	    
	    variable grid
	    dict set grid $row $column [list widget $widget colspan $colspan rowspan $rowspan sticky $sticky in $in]
	}

	constructor {args} {
	    variable maxwidth 0
	    variable maxheight 0
	    variable {*}$args
	    variable grid {}
	}
    }

    # make shims for each kind of widget
    foreach n {button label entry text} {
	proc $n {name args} [string map [list %T% $n] {
	    set ns [uplevel 1 {namespace current}]
	    return [%T%C create ${ns}::$name {*}$args]
	}]
    }

    # add some shims to make things look a little like an interp
    proc exit {args} {
	rename [info coroutine] {}
    }
    proc global {args} {
	foreach n $args {lappend v $n $n}
	uplevel 1 [list upvar #1 {*}$v]
    }
    
    namespace export -clear *
    namespace ensemble create -subcommands {}
}
