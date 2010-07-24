package require TclOO
package require Debug
Debug define wubwidgets 10
Debug define wubwinterp 10

package provide WubWidgets 1.0

namespace eval ::WubWidgets {
    oo::class create widget {
	# cget - get a variable's value
	method cget {n} {
	    set n [string trim $n -]
	    variable $n
	    return [set $n]
	}
	method cget? {n} {
	    set n [string trim $n -]
	    variable $n
	    if {[info exists $n]} {
		return [set $n]
	    } else {
		return ""
	    }
	}
	method cexists {n} {
	    set n [string trim $n -]
	    variable $n
	    return [info exists $n]
	}

	# access interp variables
	method iset {n v} {
	    variable interp
	    Debug.wubwinterp {iset '$n' <- '$v' ([info level -1])}
	    return [{*}$interp [list set $n $v]]
	}

	method iget {n} {
	    variable interp
	    try { 
		set result [{*}$interp [list set $n]]
	    } on error {e eo} {
		set result ""
	    }

	    Debug.wubwinterp {iget '$n' -> '$result'}
	    return $result
	}
	method iexists {n} {
	    variable interp
	    return [{*}$interp [list info exists $n]]
	}
	method itrace {what args} {
	    variable interp
	    {*}$interp [list trace add variable $what write $args]
	}

	# variable tracking
	method changevar {args} {
	    Debug.wubwinterp {[namespace tail [self]] changevar $args}
	    my change	;# signal that a variable has changed value
	}

	# copy from text to textvariable
	method copytext {varname op .} {
	    variable textvariable
	    set value [my iget $varname]
	    my iset $textvariable $value
	    Debug.wubwinterp {[namespace tail [self]] copytext $textvariable <- '$value'}

	    variable change
	    incr change 
	}

	# copy from textvariable to text
	method copytextvar {varname op value} {
	    variable textvariable
	    variable text
	    variable interp
	    set text [my iget $textvariable]
	    Debug.wubwinterp {[namespace tail [self]] copytextvar text <- '$text' from '$textvariable'}
	    variable change
	    incr change 
	}

	# configure - set variables to their values
	method configure {args} {
	    if {$args eq {}} {
		Debug.wubwidgets {[info coroutine] fetching configuration [self]}
		set result {}
		foreach var [info object vars [self]] {
		    if {![string match _* $var]} {
			variable $var
			lappend result $var [set $var]
		    }
		}
		return $result
	    }

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
	    if {[info exists grid]} {
		# the widget needs to be gridded
		lassign $grid r c rs cs
		set ga {}
		foreach {v1 v2} {r row c column rs rowspan cs columnspan} {
		    if {[info exists $v1] && [set $v1] ne ""} {
			lappend ga -$v2 [set $v1]
		    }
		}
		Debug.wubwidgets {option -grid: 'grid configure .[my widget] $ga'}
		uplevel 3 [list grid configure .[my widget] {*}$ga]
	    }

	    if {[info exists textvariable]
		&& $textvariable ne ""
	    } {
		if {![my iexists $textvariable]} {
		    variable text
		    if {![info exists text]} {
			set text ""
		    }
		    Debug.wubwidgets {[info coroutine] setting -textvariable $textvariable to '$text'}
		    my iset $textvariable $text
		}
		my itrace $textvariable .[my widget] changevar
	    }
	}

	method interp {{i ""}} {
	    variable interp
	    if {$i eq ""} {
		return $interp
	    } else {
		set interp $i
	    }
	}

	method compound {text} {
	    set image [my cget? -image]
	    if {$image ne ""} {
		set image [uplevel 2 [list $image render]]
	    }

	    switch -- [my cget? compound] {
		left {
		    return $image$text
		}
		right {
		    return $text$image
		}
		center -
		top {
		    return "$image[my connection <br>]$text"
		}
		bottom {
		    return "$text[my connection <br>]$image"
		}
		none -
		default {
		    # image instead of text
		    if {$image ne ""} {
			return "$image"
		    } else {
			return "$text"
		    }
		}
	    }
	}

	method type {} {
	    set class [string range [namespace tail [info object class [self]]] 0 end-1]
	}
	method gridder {grid} {
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

	# calculate name relative to widget's gridder
	method relative {} {
	    return [lindex [split [namespace tail [self]] .] end]
	}
	method gridname {} {
	    return [join [lrange [split [namespace tail [self]] .] 0 end-1] .]
	}

	method js {r} {
	    if {[set js [my cget? -js]] ne ""} {
		set r [Html postscript $r $js]
	    }
	    return $r
	}

	method command {args} {
	    set cmd [my cget? command]
	    if {$cmd ne ""} {
		set cmd [string map [list %W .[my widget]] $cmd]
		Debug.wubwidgets {[self] calling command ($cmd)}
		variable interp
		{*}$interp $cmd
	    }
	}

	method var {value} {
	    if {[my cexists textvariable]} {
		set var [my cget textvariable]
		Debug.wubwidgets {[self] var $var <- '$value'}
		my iset $var $value
	    } elseif {[my cexists text]} {
		variable text
		set text $value
	    }
	}

	method slider {value} {
	    set var [my cget variable]
	    Debug.wubwidgets {[self] scale $var <- '$value'}
	    my iset $var $value
	    my command
	}

	method cbutton {value} {
	    variable variable
	    variable interp
	    Debug.wubwidgets {[self] cbutton: setting '$variable' to '$value'}
	    if {$value} {
		my iset $variable 1
	    } else {
		my iset $variable 0
	    }
	    my command
	}

	# style - construct an HTML style form
	method style {} {
	    set result {}
	    foreach {css tk} {
		background-color background
		color foreground
		text-align justify
		vertical-align valign
		border borderwidth
		width wwidth
	    } {
		variable $tk
		if {[info exists $tk] && [set $tk] ne ""} {
		    if {$tk eq "background"} {
			lappend result "background: none [set $tk] !important"
			lappend result "border: none !important"
		    } else {
			foreach n $css {
			    lappend result "$n: [set $tk]"
			}
		    }
		}
	    }

	    # todo - padding
	    return [join $result ";"]
	}

	constructor {args} {
	    Debug.wubwidgets {Widget construction: self-[self] ns-[namespace current] path:([namespace path])}
	    oo::objdefine [self] forward connection [namespace qualifiers [self]]::connection
	    
	    variable _refresh ""
	    my configure {*}$args
	    if {[my cexists text]} {
		variable text
		if {![my cexists textvariable]} {
		    trace add variable text write [list [self] changevar]
		} else {
		    variable textvariable
		    my itrace $textvariable .[my widget] copytext
		}
	    } elseif {[my cexists textvariable]} {
		variable textvariable
		my itrace $textvariable .[my widget] copytextvar
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
	    set text [tclarmour [armour [my cget -text]]]
	    return [my connection <button> [my widget] id $id {*}$class style [my style] [my compound $text]]
	    
	    #<button class="ui-button ui-widget ui-state-default ui-corner-all ui-button-text-only" role="button" aria-disabled="false"><span class="ui-button-text">A button element</span></button>
	}

	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge {text ""
		justify left
	    } $args]

	    if {[set var [my cget? variable]] ne ""} {
		if {![my iexists $var]} {
		    my iset $var ""
		}
	    }
	}
    }

    oo::class create checkbuttonC {
	method render {{id ""}} {
	    set id [my id $id]

	    if {[my cexists textvariable]} {
		set label [my iget [my cget textvariable]]
	    } else {
		set label [my cget text]
	    }

	    Debug.wubwidgets {checkbutton render: getting '[my cget variable]' == [my iget [my cget variable]]}
	    set val [my iget [my cget variable]]
	    if {$val ne "" && $val} {
		set checked 1
	    } else {
		set checked 0
	    }

	    Debug.wubwidgets {[self] checkbox render: checked:$checked}
	    my reset
	    return [my connection <checkbox> [my widget] id $id class cbutton style [my style] checked $checked [my compound $label]]
	}

	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge [list -variable [my widget]] {
		text ""
		justify left
	    } $args]

	    set var [my cget variable]
	    Debug.wubwidgets {checkbutton construction: setting $var} 
	    my iset $var 0
	}
    }

    oo::class create labelC {
	method render {{id ""}} {
	    set id [my id $id]
	    if {[my cexists textvariable]} {
		set var [my cget textvariable]
		set val [my iget $var]
	    } else {
		set val [my cget text]
	    }

	    my reset
	    set text [tclarmour [armour $val]]
	    return [my connection <div> id $id style [my style] [my compound $text]]
	}
	
	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge {text ""
		justify "left"
	    } $args]
	}
    }
    
    oo::class create scaleC {
	method render {{id ""}} {
	    set id [my id $id]
	    my reset
	    set result ""

	    if {[my cget label] ne ""} {
		set result [my connection <label> [my cget label]]
	    }

	    append result [my connection <div> id $id class slider style [my style] {}]

	    return $result
	}

	method js {r} {
	    # need to generate the slider interaction
	    foreach {n v} [list orientation '[my cget orient]' min [my cget from] max [my cget to]] {
		lappend args $n $v
	    }
	    set var [my cget -variable]
	    lappend args value [my iget $var]

	    lappend args change [string map [list %ID% [my widget]] {
		function (event, ui) {
		    $("#Spinner_").show();
		    $.ajax({
			context: this,
			type: "GET",
			url: ".",
			data: {id: '%ID%', val: ui.value, _op_: "slider"},
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
	}

	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge [list -variable [my widget]] {
		justify left state active
		label "" command "" -length 100 -width 10 -showvalue 0
		-from 0 -to 100 -tickinterval 0
		-orient horizontal
	    } $args]
	    set variable [my cget variable]
	    if {![my iexists $variable]} {
		my iset $variable [my cget from]
	    }
	    my itrace $variable .[my widget] changevar
	}
    }
    
    oo::class create entryC {
	method render {{id ""}} {
	    Debug.wubwidgets {[info coroutine] rendering Entry [self]}
	    set id [my id $id]

	    if {[my cexists textvariable]} {
		set val [my iget [my cget -textvariable]]
	    } else {
		set val ""
	    }

	    set disabled ""
	    if {[my cget -state] ne "normal"} {
		set disabled {disabled 1}
	    }

	    my reset
	    set cmd <text>
	    set extra {}
	    if {[my cexists type]} {
		switch -- [my cget type] {
		    password {
			set cmd <password>
		    }
		    date -
		    default {
		    }
		}
	    }

	    return [my connection $cmd [my widget] id $id class variable {*}$disabled style [my style] size [my cget -width] [tclarmour [armour $val]]]
	}

	method js {r} {
	    if {![my cexists type]} {
		return $r
	    }
	    Debug.wubwidgets {entry js: [my cget type] - [my id]}
	    switch -- [my cget type] {
		date {
		    set r [jQ datepicker $r #[my id]]
		    Debug.wubwidgets {entry js: [dict get? $r -script]}
		}
	    }
	    return [next? $r]
	}

	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge {text ""
		justify left
		state normal width 16
	    } $args]
	    if {[my cexists -show] && ![my cexists -type]} {
		my configure -type password
	    }
	}
    }

    # Html widget
    oo::class create htmlC {
	# render widget
	method render {{id ""}} {
	    if {[my cexists textvariable]} {
		set var [my cget -textvariable]
		set val [my iget $var]
	    } else {
		set val [my cget? -text]
	    }
	    return $val
	}

	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge {} $args]
	}
    }

    # cookie widget - manipulate a cookie
    # can set/get/clear cookie
    # options -path -domain -expires, etc.
    oo::class create cookieC {
	# render widget
	method render {{id ""}} {
	    error "Can't render cookie widgets"
	}

	superclass ::WubWidgets::widget
	constructor {args} {
	    if {[string match *.* [namespace tail [self]]]} {
		error "cookie names must be plain identifiers"
	    }
	    next {*}[dict merge {} $args]

	    my connection cookie construct [self]

	    oo::objdefine [self] forward get my connection cookie get [self]
	    oo::objdefine [self] forward clear my connection cookie clear [self]
	    oo::objdefine [self] forward set my connection cookie set [self]
	}
    }

    # widget template
    oo::class create junkC {
	# render widget
	method render {{id ""}} {
	}

	# optional - add per-widget js
	method js {r} {
	    return $r
	}

	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge {default args} $args]
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
		set val [my iset $var]
	    } else {
		set val [my get]
	    }
	    set class {class variable}
	    
	    set disabled ""
	    if {[my cget -state] ne "normal"} {
		set disabled {disabled 1}
	    }
	    
	    my reset
	    return [my connection <textarea> [my widget] id $id {*}$class {*}$disabled style [my style] rows [my cget -height] cols [my cget -width] [tclarmour [armour $val]]]
	}
	
	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge {text ""
		justify left
		state normal height 10 width 40
	    } $args]
	}
    }

    oo::class create wmC {
	foreach n {title header css background} {
	    method $n {widget args} [string map [list %N% $n] {
		if {$widget eq "."} {
		    variable %N%
		    if {[llength $args]} {
			set %N% [lindex $args 0]
		    }
		    if {[info exists %N%]} {
			return $%N%
		    } else {
			return ""
		    }
		} elseif {[llength $args]} {
		    return [$widget configure %N% [lindex $args 0]]
		} else {
		    return [$widget cget? %N%]
		}
	    }]
	}

	foreach n {stylesheet} {
	    method $n {widget args} [string map [list %N% $n] {
		if {$widget eq "."} {
		    variable %N%
		    if {[llength $args]} {
			set %N% $args
		    }
		    if {[info exists %N%]} {
			return $%N%
		    } else {
			return ""
		    }
		} elseif {[llength $args]} {
		    return [$widget configure %N% $args]
		} else {
		    return [$widget cget? %N%]
		}
	    }]
	}

	constructor {args} {
	    variable title "WubTk"
	    variable header ""
	    variable {*}$args
	    oo::objdefine [self] forward connection [namespace qualifiers [self]]::connection
	    oo::objdefine [self] forward site my connection site
	    oo::objdefine [self] forward redirect my connection redirect
	}
    }

    oo::class create imageC {
	method changed? {} {return 0}

	# record widget id
	method style {} {
	    set result {}
	    foreach a {alt longdesc height width usemap ismap} {
		if {[my cexists $a]} {
		    lappend result $a [my cget $a]
		}
	    }
	    return $result
	}
	
	method fetch {r} {
	    if {[my cexists -data]} {
		return [Http Ok $r [my cget -data] [my cget -format]]
	    } else {
		return [Http File $r [my cget -file] [my cget -format]]
	    }
	}

	method render {{junk ""}} {
	    set url [my cget? url]
	    if {$url eq ""} {
		set url [my widget]
	    }
	    return [my connection <img> {*}[my style] src $url]
	}
	
	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge [list id [my widget]] $args]

	    # TODO - add a -url option to deliver content
	    set fmt [my cget? -format]
	    switch -glob -nocase -- $fmt {
		gif* {
		    set fmt image/gif
		}
		png {
		    set fmt image/png
		}
		jpeg -
		jpg {
		    # no format specified
		    set fmt image/jpeg
		}
		default {
		    set fmt ""
		}
	    }

	    if {[my cexists -data]} {
		if {$fmt eq ""} {
		    set fmt image/jpeg
		}
	    } elseif {[my cexists -file]} {
		set fmt [Mime MimeOf [file extension [my cget -file]] $fmt]
	    } elseif {![my cexists url]} {
		error "Must specify either -data or -file"
	    }

	    next -format $fmt
	}
    }

    proc image {op args} {
	switch -- $op {
	    create {
		set ns [uplevel 1 {namespace current}]
		Debug.wubwidgets {image creation: $args ns: $ns}
		set args [lassign $args type]
		if {[llength $args]%2} {
		    set args [lassign $args name]
		    set result [imageC create ${ns}::$name type $type {*}$args]
		} else {
		    set result [uplevel 1 [list imageC new type $type {*}$args]]
		}
		Debug.wubwidgets {image created: $result}
		return $result
	    }
	}
    }

    # grid store grid info in an x/y array gridLayout(column.row)
    oo::class create gridC {
	# traverse grid looking for changes.
	method changes {r} {
	    if {[dict exists $r -repaint]} {
		Debug.wubwidgets {Grid '[namespace tail [self]]' repainting}
		return [list $r {}]
	    }
	    variable grid;variable oldgrid
	    Debug.wubwidgets {Grid '[namespace tail [self]]' changes}

	    # look for modified grid entries, these will cause a repaint
	    dict for {row rval} $grid {
		dict for {col val} $rval {
		    if {$val ne [dict oldgrid.$row.$col?]} {
			# grid has changed ... force reload
			Debug.wubwidgets {[namespace tail [self]] repainting}
			dict set r -repaint 1
			return [list $r {}]	;# return the dict of changes by id
		    }
		}
	    }
	    if {[dict size [dict ni $oldgrid [dict keys $grid]]]} {
		# a grid element has been deleted
		dict set r -repaint 1
		return [list $r {}]	;# return the dict of changes by id
	    }

	    set changes {}
	    dict for {row rval} $grid {
		dict for {col val} $rval {
		    dict with val {
			if {[uplevel 1 [list $widget changed?]]} {
			    set type [uplevel 1 [list $widget type]]
			    switch -- $type {
				accordion -
				notebook -
				frame {
				    set changed [lassign [uplevel 1 [list $widget changes $r]] r]
				    if {[dict exists $r -repaint]} {
					Debug.wubwidgets {Grid '[namespace tail [self]]' repainting because of changes in $type '$widget'}
					return [list $r {}]	;# repaint
				    } else {
					Debug.wubwidgets {Grid '[namespace tail [self]]' changes to [string totitle $type] '$widget' at ($row,$col) ($val) -> ($changed)}
				    }
				}
				default {
				    set changed [list [uplevel 1 [list $widget id]] [uplevel 1 [list $widget render]] [uplevel 1 [list $widget type]]]
				    Debug.wubwidgets {Grid '[namespace tail [self]]' changes to [string totitle $type] '$widget' at ($row,$col) ($val) -> ($changed)}
				}
			    }

			    lappend changes {*}$changed

			    set r [uplevel 1 [list $widget js $r]]
			}
		    }
		}
	    }

	    return [list $r {*}$changes]	;# return the dict of changes by id
	}

	# something has changed us
	method prod {{prod ""}} {
	    variable interest
	    if {$prod eq ""} {
		if {$interest} {
		    my connection prod
		}
	    } else {
		set interest $prod
	    }
	}

	method id {row col} {
	    variable name
	    return [join [list grid {*}[string map {. _} $name] $row $col] _]
	}

	method render {} {
	    variable name
	    variable maxrows; variable maxcols; variable grid
	    Debug.wubwidgets {Grid '[namespace tail [self]]' render rows:$maxrows cols:$maxcols ($grid)}
	    set rows {}
	    set interaction {};
	    for {set row 0} {$row < $maxrows} {incr row} {
		set cols {}
		for {set col 0} {$col < $maxcols} {} {
		    set columnspan 1
		    if {[dict exists $grid $row $col]} {
			set el [dict get $grid $row $col]
			dict with el {
			    set id [my id $row $col]
			    Debug.wubwidgets {Grid '[namespace tail [self]]' render $widget ($id)}
			    uplevel 1 [list $widget gridder [self]]	;# record grid
			    set rendered [uplevel 1 [list $widget render $id]]

			    set wid .[string map {" " .} [lrange [split $id _] 1 end-2]]
			    for {set rt $row} {$rt < $rowspan} {incr rt} {
				set rspan($wid,[expr {$row + $rt}].$col) 1
				for {set ct $col} {$ct < $columnspan} {incr ct} {
				    set rspan($wid,$rt.[expr {$col + $ct}]) 1
				}
			    }

			    if {$rowspan != 1} {
				set rowspan [list rowspan $rowspan]
			    } else {
				set rowspan {}
			    }
			    lappend cols [my connection <td> colspan $columnspan {*}$rowspan $rendered]
			}
			incr col $columnspan
		    } else {
			if {[info exists wid] && ![info exists rspan($wid,$row.$col)]} {
			    lappend cols [my connection <td> "&nbsp;"]
			}
			incr col $columnspan
		    }
		}

		# now we have a complete row - accumulate it
		lappend rows [my connection <tr> align center valign middle [join $cols \n]]
	    }

	    variable oldgrid $grid	;# record the old grid
	    set content [my connection <tbody> [join $rows \n]]

	    variable width
	    if {[info exists width]} {
		set w [list style "width: $width"]
	    } else {
		set w {}
	    }
	    set content [my connection <table> class grid {*}$w $content]
	    Debug.wubwidgets {Grid '[namespace tail [self]]' rendered ($content)}
	    return $content
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
	    variable name
	    if {[string match .* $widget]} {
		set frame [lrange [split $widget .] 1 end]
		set widget $name$widget
	    } else {
		set frame [split $widget .]
		set widget $name.$widget
	    }
	    
	    if {[llength $frame] > 1} {
		Debug.wubwidgets {SubGrid '[namespace tail [self]]'/$frame gridding .[join $frame .]: '$name.[lindex $frame 0] grid configure [join [lrange $frame 1 end] .] $args'}
		uplevel 1 [list $name.[lindex $frame 0] grid configure [join [lrange $frame 1 end] .] {*}$args]
		return $widget
	    } else {
		Debug.wubwidgets {'[namespace tail [self]]' grid configure ($frame) $widget $args}
	    }

	    # set defaults
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

	    variable name
	    Debug.wubwidgets {[namespace tail [self]] configure gridding $widget in [uplevel 1 {namespace current}]}
	    uplevel 1 [list $widget gridder [self]]	;# record grid

	    set id [my id $row $column]
	    uplevel 1 [list $widget id $id]	;# inform widget of its id

	    return $widget
	}

	constructor {args} {
	    Debug.wubwidgets {GRID [self] constructed}
	    variable maxcols 0
	    variable maxrows 0
	    variable name ""

	    variable {*}$args

	    variable grid {}
	    variable interest 0
	    oo::objdefine [self] forward connection [namespace qualifiers [self]]::connection
	}
    }

    # frame widget
    oo::class create frameC {
	method grid {args} {
	    variable fgrid
	    Debug.wubwidgets {Frame [namespace tail [self]] gridding: $fgrid $args}
	    uplevel 1 [list $fgrid {*}$args]
	}

	# render widget
	method render {{id ""}} {
	    variable fgrid
	    Debug.wubwidgets {Frame [namespace tail [self]] render gridded by $fgrid}

	    if {[my cexists -div]} {
		set id [my id $id]
		append content \n [uplevel 1 [list $fgrid render]]
		return [my connection <div> id $id $content]
	    } else {
		set label [my cget? -text]
		if {$label ne ""} {
		    set content [my connection <legend> $label]
		}
		variable fgrid
		append content \n [uplevel 1 [list $fgrid render]]
		return [my connection <fieldset> [my widget] -raw 1 $content]
	    }
	}

	method changed? {} {return 1}

	method changes {r} {
	    variable fgrid
	    Debug.wubwidgets {Frame '[namespace tail [self]]' sub-grid changes}
	    set changes [lassign [uplevel 1 [list $fgrid changes $r]] r]
	    Debug.wubwidgets {Frame '[namespace tail [self]]' sub-grid changed: ($changes)}
	    return [list $r {*}$changes]
	}

	method js {r} {
	    variable fgrid
	    return [uplevel 1 [list $fgrid js $r]]
	}

	destructor {
	    # TODO destroy all child widgets

	    variable fgrid
	    catch {$fgrid destroy}
	}

	superclass ::WubWidgets::widget
	constructor {args} {
	    set args [dict merge {} $args] 
	    if {[dict exists $args -width]} {
		variable width [dict get $args -width]
		dict unset args -width
	    }
	    next {*}$args

	    # create a grid for this frame
	    set name [self]..grid
	    if {[dict exists $args -width]} {
		set w [list style "width:$width"]
	    } else {
		set w {}
	    }
	    variable fgrid [WubWidgets gridC create $name {*}$w name .[my widget]]
	    Debug.wubwidgets {created Frame [self] gridded by $fgrid}
	}
    }

    # toplevel widget
    oo::class create toplevelC {
	method grid {args} {
	    variable tgrid
	    Debug.wubwidgets {Toplevel [namespace tail [self]] gridding: $tgrid $args}
	    uplevel 1 [list $tgrid {*}$args]
	}

	# render widget
	method fetch {r} {
	    variable tgrid
	    Debug.wubwidgets {[namespace tail [self]] toplevel render gridded by $tgrid}
	    set r [my connection prep $r]

	    set title [my cget? -title]
	    if {$title eq ""} {
		set title [my widget]
	    }
	    dict set r -title $title

	    set header [my cget? -header]
	    if {$header ne ""} {
		dict lappend r -headers $header
	    }

	    # cascade in style
	    set css [my cget? css]
	    if {$css ne ""} {
		set content [<style> $css]
	    } else {
		set content [<style> [uplevel 1 [list wm css .]]]
	    }

	    # cascade in stylesheet
	    set style [my cget? stylesheet]
	    if {$style ne ""} {
		set r [Html postscript $r [<stylesheet> {*}$style]]
	    } else {
		set style [uplevel 1 [list wm stylesheet .]]
		if {$style ne ""} {
		    set r [Html postscript $r [<stylesheet> {*}$style]]
		}
	    }

	    variable tgrid
	    append content \n [uplevel 1 [list $tgrid render]]
	    return [Http Ok $r $content x-text/html-fragment]
	}

	method changed? {} {return 1}

	method changes {r} {
	    variable tgrid
	    Debug.wubwidgets {Toplevel '[namespace tail [self]]' sub-grid changes}
	    set changes [lassign [uplevel 1 [list $tgrid changes $r]] r]
	    Debug.wubwidgets {Toplevel '[namespace tail [self]]' sub-grid changed: ($changes)}
	    return [list $r {*}$changes]
	}

	method js {r} {
	    variable tgrid
	    return [uplevel 1 [list $tgrid js $r]]
	}

	destructor {
	    # TODO destroy all child widgets

	    variable tgrid; catch {$tgrid destroy}
	    catch {my connection tl delete [self]}
	}

	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge {titlebar 1 menubar 1 toolbar 1 location 1 scrollbars 1 status 1 resizable 1} $args]

	    # create a grid for this toplevel
	    set name [self]..grid
	    variable tgrid [WubWidgets gridC create $name name .[my widget]]
	    Debug.wubwidgets {created Toplevel [self] gridded by $tgrid - alerting}
	    my connection tl add [self] $args
	}
    }

    # upload template
    oo::class create uploadC {
	# render widget
	method render {{id ""}} {
	    set id [my id $id]
	    set content [my connection layout form_$id enctype multipart/form-data class upload_form [subst {
		file $id upload

		submit send_$id Upload
		hidden id [my widget]
		hidden _op_ upload
	    }]]
	    return $content
	}
	method changed? {args} {return 0}
	method upload {args} {
	    return [my command {*}$args]
	}

	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge {} $args]
	}
    }

    # notebook widget
    oo::class create notebookC {

	method grid {cmd w args} {
	    if {$cmd eq "configure"} {
		set w [lassign [split $w .] frame]
		Debug.wubwidgets {notebook grid: '.[my widget].$frame grid $cmd [join $w .] $args'}
		return [uplevel 1 [list .[my widget].$frame grid $cmd [join $w .] {*}$args]]
	    }
	}
	
	# render widget
	method render {{id ""}} {
	    set id [my id $id]
	    variable tabs
	    set body {}; set js {}; set cnt 0
	    set li {}
	    foreach tab $tabs {
		set tid ${id}_$cnt
		lappend body [uplevel 1 [list $tab render $tid]]
		set cnf [uplevel 1 [list $tab configure]]
		lappend li [my connection <li> [my connection <a> href "#$tid" [dict cnf.text]]]
		incr cnt
	    }
	    set content [my connection <ul> [join $li \n]]
	    append content [join $body \n]
	    
	    return [my connection <div> id $id class notebook style [my style] $content]
	}

	method changed? {} {return 1}
	
	method changes {r} {
	    variable tabs
	    set changes {}
	    foreach tab $tabs {
		set changed [lassign [uplevel 1 [list $tab changes $r]] r]
		Debug.wubwidgets {Notebook '[namespace tail [self]]' tab [string totitle [uplevel 1 [list $tab type]]] '$tab' changes: ($changed)}
		lappend changes {*}$changed
	    }
	    return [list $r {*}$changes]
	}

	# optional - add per-widget js
	method js {r} {
	    set id [my id]
	    variable tabs
	    variable set
	    set cnt 0
	    foreach tab $tabs {
		set r [uplevel 1 [list $tab js $r]]
		if {!$set} {
		    set cnf [uplevel 1 [list $tab configure]]
		    switch -- [dict cnf.state] {
			normal {
			    set r [Html postscript $r "\$('#$id').tabs('enable',$cnt)"]
			}
			disabled {
			    set r [Html postscript $r "\$('#$id').tabs('disable',$cnt)"]
			}
		    }
		    incr cnt
		}
	    }

	    if {$set} {
		return $r
	    } else {
		incr set
		return [jQ tabs $r "#[my id]"]
	    }
	}

	method add {w args} {
	    set type [uplevel 1 [list $w type]]
	    if {$type ne "frame"} {
		error "Can only add Frames to Notebooks.  $w is a $type"
	    }
	    variable tabs
	    set text [uplevel 1 [list $w cget? -text]]
	    if {$text eq ""} {
		set text "Tab[llength $tabs]"
	    }
	    uplevel 1 [list $w configure {*}[dict merge [list -state normal -text $text] $args {-div 1}]]
	    lappend tabs $w
	}

	method insert {index w args} {
	    set type [uplevel 1 [list $w type]]
	    if {$type ne "frame"} {
		error "Can only add Frames to Notebooks.  $w is a $type"
	    }
	    variable tabs
	    set text [uplevel 1 [list $w cget? -text]]
	    if {$text eq ""} {
		set text "Tab$index"
	    }
	    uplevel 1 [list $w configure {*}[dict merge [list -state normal -text $text] $args] {-div 1}]
	    set tabs [linsert $tabs $index $w]
	}

	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge {} $args]
	    variable tabs {}
	    variable set 0
	}
    }

    oo::class create accordionC {
	method grid {cmd w args} {
	    if {$cmd eq "configure"} {
		set w [lassign [split $w .] frame]
		return [uplevel 1 [list .[my widget].$frame grid $cmd [join $w .] {*}$args]]
	    }
	}

	# render widget
	method render {{id ""}} {
	    set id [my id $id]
	    variable panes
	    set body {}; set cnt 0
	    foreach pane $panes {
		set tid ${id}_$cnt
		set cnf [uplevel 1 [list $pane configure]]
		lappend body [my connection <h3> [my connection <a> href # [dict cnf.text]]]
		lappend body [uplevel 1 [list $pane render $tid]]
		incr cnt
	    }
	    set content [join $body \n]
	    
	    return [my connection <div> id $id class style [my style] accordion $content]
	}

	method changed? {} {return 1}

	method changes {r} {
	    variable panes
	    set changes {}
	    foreach pane $panes {
		set changed [lassign [uplevel 1 [list $pane changes $r]] r]
		lappend changes {*}$changed
	    }
	    return [list $r {*}$changes]
	}

	# optional - add per-widget js
	method js {r} {
	    set id [my id]
	    variable panes
	    set cnt 0
	    foreach pane $panes {
		set r [uplevel 1 [list $pane js $r]]
	    }

	    variable set
	    if {$set} {
		return $r
	    } else {
		incr set
		return [jQ accordion $r #[my id]]
	    }
	}

	method add {args} {
	    set ws {}
	    set options {}
	    set wmode 1
	    foreach n $args {
		if {$wmode && [string match .* $n]} {
		    lappend ws $n
		} else {
		    set wmode 0
		    lappend options $n
		}
	    }

	    variable panes
	    foreach w $ws {
		set type [uplevel 1 [list $w type]]
		if {$type ne "frame"} {
		    error "Can only add Frames to Accordions.  $w is a $type"
		}

		set text [uplevel 1 [list $w cget? -text]]
		if {$text eq ""} {
		    set text "Tab[llength $panes]"
		}

		uplevel 1 [list $w configure {*}[dict merge [list -state normal -text $text] $options {-div 1}]]
		lappend panes $w
	    }
	}

	superclass ::WubWidgets::widget
	constructor {args} {
	    next {*}[dict merge {} $args]
	    variable panes {}
	    variable set 0
	}
    }

    # make shims for each kind of widget
    variable tks {button label entry text checkbutton scale frame notebook accordion html toplevel upload cookie}

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
