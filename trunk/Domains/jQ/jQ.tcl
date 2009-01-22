package require Html
package require File
package require Debug
Debug off jq 10

package provide jQ 1.0

namespace eval jQ {
    proc postscript {script args} {
	return [list -postscript ${prefix}/scripts/$script $args]
    }

    proc script {r script args} {
	variable prefix
	dict set r -postscript ${prefix}/scripts/$script $args
	return $r
    }

    variable google 0
    proc scripts {r args} {
	variable prefix
	#puts stderr "PRESCRIPT: [Dict get? $r -postscript]"

	variable google
	if {$google} {
	    # use the google AJAX repository
	    dict set r -postscript http://www.google.com/jsapi {}
	    dict set r -postscript !google [<script> {google.load("jquery", "1.2.6");}]
	}
	# load each script
	foreach script $args {
	    if {$google
		&& $script eq "jquery.js"
	    } continue ;# needn't load jquery.js
	    dict set r -postscript ${prefix}/scripts/$script {}
	}
	#puts stderr "SCRIPT: [dict get $r -postscript]"
	return $r
    }

    proc theme {r theme} {
	variable prefix
	dict set r -style ${prefix}/scripts/trunk/themes/$theme/$theme.all.css {}
	return $r
    }

    proc style {r style args} {
	variable prefix
	dict set r -style ${prefix}/css/$style $args
	return $r
    }

    variable defaults {
	editable {
	    indicator {'<img src="/icons/indicator.gif">'}
	    type 'textarea'
	    select false
	    autogrow {{lineHeight:16, minHeight:32}}
	    submit 'OK'
	    onblur 'ignore'
	    cancel 'cancel'
	    cssclass 'autogrow'
	    event 'dblclick'
	    tooltip {'Click to edit'}
	    style 'inherit'
	}

	autogrow {
		maxHeight 1000
		minHeight 100
	}
	galleria {
	    history true
	    clickNext true
	}

	map {
	    center [0,0]
	    mapType 'hybrid'
	}

	galleria1 {
	    history false
	    clickNext false
	    insert undefined
	    onImage {
		function() { $('.nav').css('display','block'); }
	    }
	}
    }

    proc opts {type args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	set opts {}
	variable defaults
	if {[dict exists $defaults $type]} {
	    set args [list {*}[dict get $defaults $type] {*}$args]
	}
	dict for {n v} $args {
	    lappend opts "$n: $v"
	}
	if {$opts eq ""} {
	    return ""
	} else {
	    return "\{[join $opts ,]\}"
	}
    }

    # generate a DOM ready function
    proc ::<ready> {args} {
	set script [lindex $args end]
	set args [lrange $args 0 end-1]
	return [<script> [string map [list %F [string map $args $script] ] {
	    $(function() {
		%F
	    });
	}]]
    }

    # http://garage.pimentech.net/scripts_doc_jquery_jframe/
    proc jframe {r} {
	return [scripts $r jquery.js jquery.form.js jquery.jframe.js]
    }

    # http://jtemplates.tpython.com/
    proc jtemplates {r selector args} {
	return [scripts $r jquery.js jquery-jtemplates.js]
    }

    # http://johannburkard.de/blog/programming/javascript/inc-a-super-tiny-client-side-include-javascript-jquery-plugin.html
    proc inc {r} {
	return [scripts $r jquery.js jquery.inc.js]
    }

    proc tc {url {transform ""} {post ""}} {
	lappend url [string map {" " @ \n @ \t @} $transform]
	lappend url [string map {" " @ \n @ \t @} $post]
	
	return [list class inc:[string trim [join $url "#"] "#"]]
    }

    proc history {r args} {
	set r [scripts $r jquery.js jquery.history-remote.js]
	dict lappend r -postload [<script> {
	    $(function() {
		$.ajaxHistory.initialize();
	    });
	}]
	return $r
    }

    # arrange for the function in args to be run post-load
    proc ready {r args} {
	dict lappend r -postload [::<ready> {*}$args]
	return $r
    }

    # combine a selector/initializer and set of script
    # dependencies to call a jQ package.
    # relies upon preload and postload capabilities of
    # conversion scripts to ensure code is initialized in the correct
    # order, as a postload.
    # relies upon the script and css capabilities to ensure packages
    # are loaded correctly
    # scripts - a list of scripts upon which this depends
    # args - a series of argument/value pairs followed by the script
    proc weave {r scripts args} {
	#puts stderr "WEAVE: $r"
	set script [lindex $args end]
	set args [lrange $args 0 end-1]

	# %prescript is a function to run before the script
	if {[dict exists $args %prescript]
	    && [dict get $args %prescript] ne ""
	} {
	    set js [dict get $args %prescript]\n
	}

	# generate the document ready script with %var substitution
	set script [string map [dict filter $args key %*] $script]
	append js "\$(function()\{\n${script}\n\});"

	# allow different loaders to process script
	switch -- [Dict get? $args loader] {
	    "" {
		# run this script in -postload phase
		set preload -postload
		set script [<script> $js]
	    }
	    google {
		# the script needs google loader
		set preload -google
	    }
	    default {
		# select loader in 'loader' arg
		set preload [dict get $args loader]
		set script [<script> $js]
	    }
	}

	# append the script to the relevant loader request element
	if {$script ne ""} {
	    Debug.jq {WEAVE: $script}
	    dict lappend r $preload $script
	}

	# record the style
	if {[dict exists $args css]} {
	    set r [style $r {*}[dict get $args css]]
	}

	set r [scripts $r {*}$scripts]
	#puts stderr "POST WEAVE: $r"
	return $r
    }
    
    # http://docs.jquery.com/UI/Datepicker
    proc datepicker {r selector args} {
	return [weave $r {
	    jquery.js jquery.timeentry.js
	} %SEL $selector %OPTS [opts datepicker $args] {
	    $('%SEL').timeEntry(%OPTS);
	}]
    }

    # http://keith-wood.name/timeEntryRef.html
    proc timeentry {r selector args} {
	return [weave $r {
	    jquery.js jquery.timeentry.js
	}  css jquery.timeentry.css %SEL $selector %OPTS [opts timeentry $args] {
	    $('%SEL').timeEntry(%OPTS);
	}]
    }

    # http://remysharp.com/2007/01/25/jquery-tutorial-text-box-hints/
    proc hint {r {selector input[title!=""]} args} {
	return [weave $r {
	    jquery.js jquery.hint.js
	}  %SEL $selector %OPTS [opts hint $args] {
	    $('%SEL').hint(%OPTS);
	}]
    }

    # http://www.stainlessvision.com/collapsible-box-jquery
    proc boxtoggle {r selector args} {
	return [weave $r {
	    jquery.js jquery.boxtoggle.js
	}  %SEL $selector %OPTS [opts boxtoggle $args] {
	    boxToggle('%SEL');
	}]
    }

    # http://tablesorter.com/addons/pager/jquery.tablesorter.pager.js
    proc tablesorter {r selector args} {
	return [weave $r {
	    jquery.js jquery.metadata.js jquery.tablesorter.js
	}  %SEL $selector %OPTS [opts tablesorter $args] {
	    $('%SEL').tablesorter(%OPTS);
	}]
    }

    # http://www.fyneworks.com/jquery/multiple-file-upload/
    proc multifile {r} {
	# just supports the simple case
	return [scripts $r jquery.js jquery.MultiFile.js]
    }

    # http://docs.jquery.com/UI/Tabs
    proc tabs {r selector args} {
	return [weave $r {
	    jquery.js jquery.ui.js
	} %SEL $selector %OPTS [opts tabs $args] {
	    $('%SEL').tabs();
	}]
	#loader -preload
    }

    proc addtab {var name content} {
	upvar 1 $var page
	set id "FT[llength [Dict get? $page _]]"
	dict set page $id $content
	dict set page _ $id $name
    }

    proc gentab {r tabid var args} {
	upvar 1 $var page
	set r [tabs $r "#$tabid > ul" {*}$args]

	set index ""
	dict for {id name} [dict get $page _] {
	    append index [<li> [<a> href "#$id" [<span> $name]]] \n
	}
	dict unset page _

	set tabs ""
	dict for {id content} $page {
	    append tabs [<div> id $id \n$content] \n
	}

	dict append r -content [<div> id $tabid class fflora "\n[<ul> \n$index]\n$tabs\n"]

	return $r
    }

    # http://docs.jquery.com/UI/Accordion
    proc accordion {r selector args} {
	return [weave $r {
	    jquery.js jquery.dimensions.js jquery.ui.js
	} %SEL $selector %OPTS [opts accordion $args] {
	    $('%SEL').accordion(%OPTS);
	}]
    }

    # http://docs.jquery.com/UI/Resizeables
    proc resizable {r selector args} {
	return [weave $r {
	    jquery.js jquery.dimensions.js jquery.ui.js
	} %SEL $selector %OPTS [opts resizable $args] {
	    $('%SEL').resizable(%OPTS);
	}]
    }

    # http://docs.jquery.com/UI/Draggables
    proc draggable {r selector args} {
	return [weave $r {
	    jquery.js jquery.dimensions.js jquery.ui.js
	} %SEL $selector %OPTS [opts draggable $args] {
	    $('%SEL').draggable(%OPTS);
	}]
    }

    # http://docs.jquery.com/UI/Droppables
    proc droppable {r selector args} {
	return [weave $r {
	    jquery.js jquery.dimensions.js jquery.ui.js
	} %SEL $selector %OPTS [opts droppable $args] {
	    $('%SEL').droppable(%OPTS);
	}]
    }

    # http://docs.jquery.com/UI/Sortables
    proc sortable {r selector args} {
	return [weave $r {
	    jquery.js jquery.ui.js
	} %SEL $selector %OPTS [opts sortable $args] {
	    $('%SEL').sortable(%OPTS);
	}]
    }

    # http://docs.jquery.com/UI/Selectables
    proc selectable {r selector args} {
	return [weave $r {
	    jquery.js jquery.dimensions.js jquery.ui.js
	} %SEL $selector %OPTS [opts selectable $args] {
	    $('%SEL').selectable(%OPTS);
	}]
    }

    # http://www.aclevercookie.com/facebook-like-auto-growing-textarea/
    proc autogrow {r selector args} {
	return [weave $r {jquery.js jquery.autogrow.js
	} %SEL $selector %OPTS [opts autogrow $args] {
	    $('%SEL').autogrow(%OPTS);
	}]
    }

    # http://jquery.autoscale.js.googlepages.com/
    proc autoscale {r selector args} {
	return [weave $r {jquery.js jquery.autoscale.js
	} %SEL $selector %OPTS [opts autoscale $args] {
	    $('%SEL').autoscale(%OPTS);
	}]
    }

    # http://bassistance.de/jquery-plugins/jquery-plugin-tooltip/
    proc tooltip {r selector args} {
	return [weave $r {
	    jquery.js jquery.dimensions.js
	    jquery.tooltip.js
	} %SEL $selector %OPTS [opts tooltip $args] {
	    $('%SEL').Tooltip(%OPTS);
	}]
    }

    # http://plugins.jquery.com/project/HoverImageText
    proc hoverimage {r selector args} {
	return [weave $r {
	    jquery.js jquery.hoverimagetext.js
	} %SEL $selector %OPTS [opts hoverimage $args] {
	    $('%SEL').HoverImageText(%OPTS);
	}]
    }

    proc galleria {r selector args} {
	return [weave $r {
	    jquery.js jquery.galleria.js
	} %SEL $selector %OPTS [opts galleria $args] {
	    $('ul.%SEL').galleria(%OPTS)
	}]
    }

    # http://benjaminsterling.com/jquery-jqgalview-photo-gallery/
    proc gallery {r selector args} {
	return [weave $r {
	    jquery.js jquery.galview.js
	} %SEL $selector %OPTS [opts gallery $args] {
	    $('%SEL').jqGalView(%OPTS);
	}]
    }

    proc aaccordion {r selector args} {
	return [weave $r {
	    jquery.js jquery.accordion.js
	} %SEL $selector %OPTS [opts aaccordion $args] {
	    $('%SEL').Accordion(%OPTS);
	}]
    }

    proc editable {r selector fn args} {
	if {[dict exists $args %prescript]} {
	    set pre [list %prescript [dict get $args %prescript]]
	    dict unset args %prescript
	} else {
	    set pre ""
	}
	return [weave $r {
	    jquery.js jquery.autogrow.js jquery.jeditable.js
	} %SEL $selector %OPTS [opts editable $args] {*}$pre %FN $fn {
	    $('%SEL').editable(%FN,%OPTS);
	}]
    }

    proc form  {r selector {fn ""}} {
	return [weave $r {
	    jquery.js jquery.form.js
	} %SEL $selector %FN $fn {$('%SEL').ajaxForm(%FN);}]
    }
    
    # http://bassistance.de/jquery-plugins/jquery-plugin-validation/
    proc validate {r selector args} {
	return [weave $r {
	    jquery.js jquery.delegate.js
	    jquery.maskedinput.js jquery.metadata.js
	    jquery.validate.js jquery.validate-ext.js
	} %SEL $selector %OPTS [opts validate $args] {
	    $('%SEL').validate(%OPTS);
	}]
    }

    proc autofill {r selector args} {
	return [weave $r {
	    jquery.js jquery.autofill.js
	} %SEL $selector %OPTS [opts autofill $args] {
	    $('%SEL').autofill(%OPTS);
	}]
    }

    proc confirm {r selector args} {
	return [weave $r {
	    jquery.js jquery.confirm.js
	} %SEL $selector %OPTS [opts confirm $args] {
	    $('%SEL').confirm(%OPTS);
	}]
    }

    proc ingrid {r selector args} {
	return [weave $r {
	    jquery.js jquery.ingrid.js
	} css ingrid.css %SEL $selector %OPTS [opts ingrid $args] {
	    $('%SEL').ingrid(%OPTS);
	}]
    }

    proc map {r selector callback args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}

	if {[dict exists $args key]} {
	    # load the google maps API if we're given a key
	    dict set r -postscript "http://maps.google.com/maps?file=api&v=2&key=[dict get $args key]" {}
	    dict unset args key
	}

	return [weave $r {
	    jquery.js jquery.jmaps.js
	} %SEL $selector %OPTS [opts map $args] %CALL $callback {
	    $('%SEL').jmap('init', %OPTS, %CALL);
	}]
    }

    proc sheet {r selector args} {
	return [weave $r {
	    jquery.js jquery.dimensions.js jquery.clickmenu.js jquery.sheet.calc.js jquery.sheet.js
	} css clickable.css %SEL $selector %OPTS [opts sheet $args] {
	    $('%SEL').sheet(%OPTS);
	}]
    }

    variable root [file dirname [info script]]
    variable prefix /jquery/
    proc do {r} {
	fs do $r
    }

    variable expires 0
    proc init {args} {
	if {$args ne {}} {
	    variable {*}$args
	}
	
	# construct a File wrapper for the jscript dir
	variable prefix
	variable root
	variable expires
	File create ::jQ::fs -prefix $prefix -root $root -expires $expires
	
	return $prefix
    }
    
    namespace export -clear *
    namespace ensemble create -subcommands {}
}
