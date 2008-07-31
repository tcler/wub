package require Html
package require File
package require Debug
Debug off jq 10

package provide jQ 1.0

namespace eval jQ {
    
    proc script {r script args} {
	variable prefix
	dict set r -postscripts ${prefix}/scripts/$script $args
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
	autogrow {
		maxHeight 1000
		minHeight 100
	}
	galleria {
	    history true
	    clickNext true
	    insert \"#main_image\"
	    onImage {
		function(image,caption,thumb) { // let's add some image effects for demonstration purposes
		    
		    // fade in the image & caption
		    image.css('display','none').fadeIn(100);
		    caption.css('display','none').fadeIn(100);
		    
		    // fetch the thumbnail container
		    var _li = thumb.parents('li');
		    
		    // fade out inactive thumbnail
		    _li.siblings().children('img.selected').fadeTo(50,0.3);
		    
		    // fade in active thumbnail
		    thumb.fadeTo('fast',1).addClass('selected');
		    
		    // add a title for the clickable image
		    image.attr('title','Next image >>');
		}
	    }
	    onThumb {
		function(thumb) { // thumbnail effects goes here
				
		    // fetch the thumbnail container
		    var _li = thumb.parents('li');
		    
		    // if thumbnail is active, fade all the way.
		    var _fadeTo = _li.is('.active') ? '1' : '0.3';
		    
		    // fade in the thumbnail when finnished loading
		    thumb.css({display:'none',opacity:_fadeTo}).fadeIn(1500);
		    
		    // hover effects
		    thumb.hover(
				function() { thumb.fadeTo('fast',1); },
				function() { _li.not('.active').children('img').fadeTo('fast',0.3); } // don't fade out if the parent is active
				)
		}
	    }
	}

	maps {
	    mapType hybrid
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
	    $(document).ready(function() {
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

    proc history {r args} {
	set r [scripts $r jquery.js jquery.history-remote.js]
	dict lappend r -postload [<script> "\$(document).ready(function()\{\n$.ajaxHistory.initialize();\n\});"]
	return $r
    }

    # combine a selector/initializer and set of script
    # dependencies to call a jQ package.
    # relies upon preload and postload capabilities of
    # conversion scripts to ensure code is initialized in the correct
    # order, as a postload.
    # relies upon the script and css capabilities to ensure packages
    # are loaded correctly
    proc weave {r scripts args} {
	#puts stderr "WEAVE: $r"
	set script [lindex $args end]
	set args [lrange $args 0 end-1]
	set script [string map [dict filter $args key %*] $script]
	set script "\$(document).ready(function()\{\n${script}\n\});"

	switch -- [Dict get? $args loader] {
	    "" {
		set preload -postload
		set script [<script> $script]
	    }
	    google {
		set preload -google
	    }
	    default {
		set preload [dict get $args loader]
		set script [<script> $script]
	    }
	}
	dict unset args loader

	if {$script ne ""} {
	    Debug.jq {WEAVE: $script}
	    dict lappend r $preload $script
	}

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

	dict append r -content [<div> id $tabid class flora "\n[<ul> \n$index]\n$tabs\n"]

	return $r
    }

    # http://docs.jquery.com/UI/Accordian
    proc accordian {r selector args} {
	return [weave $r {
	    jquery.js jquery.dimensions.js jquery.ui.js
	} %SEL $selector %OPTS [opts accordian $args] {
	    $('%SEL').accordian(%OPTS);
	}]
    }

    # http://docs.jquery.com/UI/Resizeables
    proc resizeable {r selector args} {
	return [weave $r {
	    jquery.js jquery.dimensions.js jquery.ui.js
	} %SEL $selector %OPTS [opts resizeable $args] {
	    $('%SEL').resizeable(%OPTS);
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

    proc aaccordian {r selector args} {
	return [weave $r {
	    jquery.js jquery.accordian.js
	} %SEL $selector %OPTS [opts aaccordian $args] {
	    $('%SEL').Accordian(%OPTS);
	}]
    }

    proc editable {r selector fn} {
	return [weave $r {
	    jquery.js jquery.editable.js
	} %SEL $selector %FN $fn {$('%SEL').editable(%FN);}]
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
	File ::jQ::fs -prefix $prefix -root $root -expires $expires
	
	return $prefix
    }
    
    namespace export -clear *
    namespace ensemble create -subcommands {}
}
