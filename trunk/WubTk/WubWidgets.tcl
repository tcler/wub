package require TclOO
package require Debug
Debug define wubwidgets 10

package provide WubWidgets 1.0

namespace eval ::WubWidgets {
    oo::class create widget {
	method type {} {
	    set class [string range [namespace tail [info object class [self]]] 0 end-1]
	}
	method grid {grid} {
	    variable _grid $grid
	}

	method change {{to ""}} {
	    variable change
	    if {$to eq ""} {
		incr change
		variable _grid
		if {$_grid ne ""} {
		    $_grid prod
		}
	    } else {
		set change $to
	    }
	}

	method reset {} {
	    variable change
	    set change 0
	}

	method changed? {} {
	    variable change
	    return $change
	}
	method changevar {args} {
	    my change	;# signal that a variable has changed value
	}

	# record widget id
	method id {{_id ""}} {
	    variable id
	    if {$_id eq ""} {
		return $id
	    } elseif {![info exists id]} {
		set id $_id
	    } else {
		return $id
	    }
	}

	method widget {} {
	    return [string trim [namespace tail [self]] .]
	}

	method js {r} {
	    return $r
	}

	method command {} {
	    if {[my cexists command]} {
		set cmd [my cget command]
		if {$cmd ne ""} {
		    set cmd [string map [list %W .[my widget]] $cmd]
		    Debug.wubwidgets {[self] calling command ($cmd)}
		    ::apply [list {} $cmd [uplevel 1 {namespace current}]]
		}
	    }
	}

	method var {value} {
	    if {[my cexists textvariable]} {
		set var [my cget textvariable]
		corovars $var
		Debug.wubwidgets {[self] var $var <- '$value'}
		set $var $value
	    } elseif {[my cexists text]} {
		variable text
		set text $value
	    }
	}

	method scale {value} {
	    set var [my cget variable]
	    corovars $var
	    Debug.wubwidgets {[self] scale $var <- '$value'}
	    set $var $value
	    my command
	}

	method cbutton {value} {
	    variable variable
	    corovars $variable
	    Debug.wubwidgets {[self] cbutton: setting '$variable' to '$value'}
	    if {$value} {
		set $variable 1
	    } else {
		set $variable 0
	    }
	    my command
	    Debug.wubwidgets {[self] cbutton: post-command '$variable' is '[set $variable]'}
	}

	# style - construct an HTML style form
	method style {} {
	    variable background; variable foreground; variable justify
	    return "background-color: $background; color: $foreground; text-align: $justify;"
	}

	# cget - get a variable's value
	method cget {n} {
	    set n [string trim $n -]
	    variable $n
	    return [set $n]
	}
	method cexists {n} {
	    set n [string trim $n -]
	    variable $n
	    return [info exists $n]
	}

	# configure - set variables to their values
	method configure {args} {
	    Debug.wubwidgets {[info coroutine] configure [self] ($args)}
	    variable change
	    variable _grid ""
	    set vars {}
	    dict for {n v} $args {
		set n [string trim $n -]
		incr change	;# remember if we've changed anything

		variable $n $v
		dict set vars $n $v

		# some things create external dependencies ... variable,command, etc
		switch -glob -- $n {
		    textvariable {
			# should create variable if necessary
			# set write trace on variable, to record change.
		    }
		    text {
		    }
		    default {
		    }
		}
	    }

	    Debug.wubwidgets {configured: $vars}
	    if {[info exists textvariable]
		&& $textvariable ne ""
	    } {
		variable text
		if {![info exists text]} {
		    set text ""
		}
		Debug.wubwidgets {[info coroutine] setting -textvariable $textvariable to '$text'}
		corovars $textvariable
		set $textvariable $text
		trace add variable $textvariable write [list [self] changevar]
	    }
	}

	method copytext {varname op value} {
	    variable textvariable
	    variable text
	    corovars $textvariable
	    set $textvariable $text
	    variable change
	    incr change 
	}

	method copytextvar {varname op value} {
	    variable textvariable
	    variable text
	    corovars $textvariable
	    set text $textvariable
	    variable change
	    incr change 
	}

	constructor {args} {
	    variable _refresh ""
	    my configure {*}$args
	    if {[my cexists text]} {
		variable text
		if {![my cexists textvariable]} {
		    trace add variable text write [list [self] changevar]
		} else {
		    trace add variable text write [list [self] copytext]
		}
	    } elseif {[my cexists textvariable]} {
		variable textvariable
		corovars $textvariable
		trace add variable $textvariable write [list [self] copytextvar]
	    }
	}
    }

    oo::class create buttonC {
	method render {{id ""}} {
	    set id [my id $id]
	    set command [my cget command]
	    if {$command ne ""} {
		set class {class button}
	    } else {
		set class {}
	    }
	    my reset
	    return [<button> [my widget] id $id {*}$class style [my style] [armour [my cget -text]]]
	}

	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge {text ""
		foreground black background white justify left
		command ""
	    } $args]
	}
    }

    oo::class create checkbuttonC {
	method render {{id ""}} {
	    set id [my id $id]

	    if {[my cexists textvariable]} {
		set lvar [my cget textvariable]
		corovars $lvar
		set label [set $lvar]
	    } else {
		set label [my cget text]
	    }
	    set var [my cget variable]
	    corovars $var
	    if {[set $var]} {
		set checked 1
	    } else {
		set checked 0
	    }
	    Debug.wubwidgets {[self] checkbox render: checked:$checked, var:[set $var]}
	    my reset
	    return [<checkbox> [my widget] id $id class cbutton style [my style] checked $checked [armour $label]]
	}

	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge [list -variable [my widget]] {
		text ""
		foreground black background white justify left
		command ""
	    } $args]
	}
    }

    oo::class create labelC {
	method render {{id ""}} {
	    set id [my id $id]
	    if {[my cexists textvariable]} {
		set var [my cget textvariable]
		corovars $var
		set val [set $var]
	    } else {
		set val [my cget text]
	    }

	    my reset
	    return [<div> id $id style [my style] [armour $val]]
	}
	
	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge {text ""
		foreground "black" background "white" justify "left"
	    } $args]
	}
    }
    
    oo::class create scaleC {
	method render {{id ""}} {
	    set id [my id $id]
	    my reset
	    set result ""

	    if {[my cget label] ne ""} {
		set result [<label> [my cget label]]
	    }

	    set var [my cget -variable]
	    corovars $var
	    set val [set $var]
	    append result [<div> id $id class slider style [my style] [armour $val]]

	    return $result
	}

	method js {r} {
	    # need to generate the slider interaction
	    foreach {n v} [list orientation '[my cget orient]' min [my cget from] max [my cget to]] {
		lappend args $n $v
	    }
	    lappend args change [string map [list %ID% [my widget]] {
		function (event, ui) {
		    $("#Spinner_").show();
		    $.ajax({
			context: this,
			type: "GET",
			url: "slider",
			data: {id: '%ID%', val: ui.value},
			dataType: "script",
			success: function (data, textStatus, XMLHttpRequest) {
			    $("#Spinner_").hide();
			    //alert("slider: "+data);
			},
			error: function (xhr, status, error) {
			    alert("ajax fail:"+status);
			}
		    });
		}
	    }]
	    return [jQ slider $r #[my id] {*}$args]
	    #$( ".selector" ).slider({
	    #change: function(event, ui) { ... }
	    #});
	    # getter - var values = $( ".selector" ).slider( "option", "values" );
	    # setter - $( ".selector" ).slider( "option", "values", [1,5,9] );
	}

	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge [list -variable [my widget]] {
		foreground black background white justify left state active
		label "" command "" -length 100 -width 10 -showvalue 0
		-from 0 -to 100 -tickinterval 0
		-orient horizontal
	    } $args]
	    set variable [my cget variable]
	    corovars $variable
	    if {![info exists $variable]} {
		set $variable [my cget from]
	    }
	    trace add variable variable write [list [self] changevar]
	}
    }
    
    oo::class create entryC {
	method render {{id ""}} {
	    Debug.wubwidgets {[info coroutine] rendering Entry [self]}
	    set id [my id $id]
	    if {[my cexists textvariable]} {
		set var [my cget -textvariable]
		corovars $var
		set val [set $var]
	    } else {
		set val ""
	    }
	    set class {class variable}

	    set disabled ""
	    if {[my cget -state] ne "normal"} {
		set disabled disabled
	    }

	    my reset
	    return [<text> [my widget] id $id {*}$class {*}$disabled style [my style] size [my cget -width] [armour $val]]
	}

	method js {r} {
	    if {![my cexists type]} {
		return $r
	    }
	    Debug.wubwidgets {entry js: [my cget type] - [my id]}
	    switch -- [my cget type] {
		date {
		    set r [jQ datepicker $r #[my id]]
		    Debug.wubwidgets {entry js: [dict get $r -script]}
		}
	    }
	    return $r
	}

	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge {text ""
		foreground black background white justify left
		state normal width 16
	    } $args]
	}
    }
    
    oo::class create textC {
	method get {{start 1.0} {end end}} {
	    set text [my cget text]

	    if {$start == "1.0" && $end == "end"} {
		return $text
	    }

	    #separate indices into line and char counts
	    foreach {sline schar} [split $start .] {}
	    foreach {eline echar} [split $end .] {}

	    set text [my cget text]
	    set linecount [regexp -all \n $text]

	    #convert end indicies into numerical indicies
	    if {$schar == "end"} {incr sline; set schar 0}
	    if {$eline == "end"} {set eline $linecount; set echar 0; incr linecount}

	    #compute deletion start and ending points
	    set nlpos 0
	    set startpos 0
	    set endpos 0

	    for {set linepos 1} {$linepos <= $linecount} {incr linepos}    {

		if {$linepos == $sline} {
		    set startpos $nlpos
		    incr startpos $schar
		    #now we got the start position
		}

		if {$linepos == $eline} {
		    set endpos $nlpos
		    incr endpos $echar
		    #now we got the end point, lets blow this clam bake
		    break
		}

		incr nlpos
		set nlpos [string first \n $text $nlpos]
	    }

	    set text [string range $text $startpos+1 $endpos]
	    return $text
	}

	method delete {{start 1.0} {end end}} {
	    if {$start == "1.0" && $end == "end"} {
		set text ""
	    } else {
		#separate indices into line and char counts
		foreach {sline schar} [split $start .] {}
		foreach {eline echar} [split $end .] {}

		set text [my cget text]
		set linecount [regexp -all \n $text]

		#convert end indicies into numerical indicies
		if {$schar == "end"} {incr sline; set schar 0}
		if {$eline == "end"} {set eline $linecount; set echar 0; incr linecount}

		#compute deletion start and ending points
		set nlpos 0
		set startpos 0
		set endpos 0
		for {set linepos 1} {$linepos <= $linecount} {incr linepos}    {
		    Debug.wubwidgets {*** $linepos $nlpos}
		    if {$linepos == $sline} {
			set startpos $nlpos
			incr startpos $schar
			#now we got the start position
		    }

		    if {$linepos == $eline} {
			set endpos $nlpos
			incr endpos $echar
			#now we got the end point, so finish
			break
		    }

		    incr nlpos
		    set nlpos [string first \n $text $nlpos]
		}

		set text [string range $text 0 $startpos][string range $text $endpos+1 end]
	    }

	    my configure text $text
	    return $text
	}

	method insert {{start end} newtext} {
	    set text [my cget text]

	    if {$start == "end"} {
		#just tack the new text on the end
		append text $newtext
	    }    else {
		#we got work to do
		foreach {sline schar} [split $start .] {}
		set linecount [regexp -all \n $text]

		#compute insertion point
		set nlpos 0
		set startpos 0
		set endpos 0
		for {set linepos 1} {$linepos <= $linecount} {incr linepos}    {

		    if {$linepos == $sline} {
			set startpos $nlpos
			incr startpos $schar
			#now we got the start position
			break
		    }

		    incr nlpos
		    set nlpos [string first \n $text $nlpos]
		}

		#insett newtext at the char pos calculated in insertpos
		set text [string range $text 0 $startpos]${newtext}[string range $text ${startpos}+1 end]
	    }

	    my configure text $text
	    return $text
	}

	method render {{id ""}} {
	    set id [my id $id]
	    set state [my cget -state]
	    
	    if {[my cexists textvariable]} {
		set var [my cget -textvariable]
		corovars $var
		set val [set $var]
	    } else {
		set val [my get]
	    }
	    set class {class variable}
	    
	    set disabled ""
	    if {[my cget -state] ne "normal"} {
		set disabled disabled
	    }
	    
	    my reset
	    return [<textarea> [my widget] id $id {*}$class {*}$disabled style [my style] rows [my cget -height] cols [my cget -width] [armour $val]]
	}
	
	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge {text ""
		foreground black background white justify left
		state normal height 10 width 40
	    } $args]
	}
    }

    oo::class create wmC {
	# TODO - make title change dynamically ... blerk.
	method title {{widget .} args} {
	    if {$widget != "."} {return}
	    variable title
	    if {[llength $args]} {
		set title [lindex $args 0]
	    }
	    return $title
	}
	constructor {args} {
	    variable title "WubTk"
	}
    }

    # grid store grid info in an x/y array gridLayout(column.row)
    oo::class create gridC {
	method exiting? {} {
	    variable exiting
	    return [info exists exiting]
	}
	method redirect {} {
	    variable exiting
	    return $exiting
	}
	method exit {value} {
	    if {[string is integer -strict $value]} {
		variable exiting ""
	    } else {
		variable exiting $value
	    }
	}

	# traverse grid looking for changes.
	method changes {r} {
	    variable grid
	    set changes {}
	    dict for {row rval} $grid {
		dict for {col val} $rval {
		    dict with val {
			if {[uplevel 1 [list $widget changed?]]} {
			    Debug.wubwidgets {changed ($row,$col) ($val)}
			    lappend changes [uplevel 1 [list $widget id]] [uplevel 1 [list $widget render]] [uplevel 1 [list $widget type]]
			    set r [uplevel 1 [list $widget js $r]]
			}
		    }
		}
	    }

	    return [list $r $changes]	;# return the dict of changes by id
	}

	# something has changed us
	method prod {{prod ""}} {
	    variable interest
	    if {$prod eq ""} {
		if {$interest} {
		    catch {uplevel 1 {connection prod}}
		}
	    } else {
		set interest $prod
	    }
	}

	method render {} {
	    global args sessid
	    variable maxrows; variable maxcols; variable grid
	    Debug.wubwidgets {GRID render rows:$maxrows cols:$maxcols ($grid)}
	    set rows {}
	    set interaction {};
	    for {set row 0} {$row < $maxrows} {incr row} {
		set cols {}
		for {set col 0} {$col < $maxcols} {} {
		    if {[dict exists $grid $row $col]} {
			set el [dict get $grid $row $col]
			dict with el {
			    set id grid_${row}_$col
			    Debug.wubwidgets {render $widget $id}
			    uplevel 1 [list $widget grid [self]]	;# record grid
			    lappend cols [<td> colspan $columnspan [uplevel 1 [list $widget render $id]]]
			}
			incr col $columnspan
		    } else {
			lappend cols [<td> "&nbsp;"]
			incr col
		    }
		}
		# now we have a complete row - accumulate it
		lappend rows [<tr> align center valign middle [join $cols \n]]
	    }
	    
	    set content [<table> [join $rows \n]]
	}

	method js {r {w ""}} {
	    variable grid
	    if {$w ne ""} {
		return [uplevel 1 [list $w js $r]]
	    }

	    dict for {rc row} $grid {
		dict for {cc col} $row {
		    set r [uplevel 1 [list [dict get $col widget] js $r]]
		}
	    }

	    return $r
	}
	
	method configure {widget args} {
	    Debug.wubwidgets {grid configure $widget $args}
	    #set defaults
	    set column 0
	    set row 0
	    set columnspan 1
	    set rowspan 1
	    set sticky ""
	    set in ""
	    
	    foreach {var val} $args {
		set [string trim $var -] $val
	    }
	    
	    variable maxcols
	    set width [expr {$column + $columnspan}]
	    if {$width > $maxcols} {
		set maxcols $width
	    }
	    
	    variable maxrows
	    set height [expr {$row + $rowspan}]
	    if {$height > $maxrows} {
		set maxrows $height
	    }
	    
	    variable grid
	    dict set grid $row $column [list widget $widget columnspan $columnspan rowspan $rowspan sticky $sticky in $in]

	    set id grid_${row}_$column
	    uplevel 1 [list $widget grid [self]]	;# record grid
	    uplevel 1 [list $widget id $id]	;# inform widget of its id
	}

	constructor {args} {
	    variable maxcols 0
	    variable maxrows 0
	    variable {*}$args
	    variable grid {}
	    variable interest 0
	}
    }

    # make shims for each kind of widget
    foreach n {button label entry text checkbutton scale} {
	proc $n {name args} [string map [list %T% $n] {
	    set ns [uplevel 1 {namespace current}]
	    return [%T%C create ${ns}::$name {*}$args]
	}]
    }

    # add some shims to make things look a little like an interp
    proc exit {args} {
	uplevel 1 [list grid exit $args]	;# informs coroutine we want to exit
    }

    proc global {args} {
	foreach n $args {lappend v $n $n}
	uplevel 1 [list upvar #1 {*}$v]
    }
    
    namespace export -clear *
    namespace ensemble create -subcommands {}
}
