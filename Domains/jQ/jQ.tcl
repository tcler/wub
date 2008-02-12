package require Html
package require File

package provide jQ 1.0

namespace eval jQ {
    
    proc script {r script args} {
	variable prefix
	dict set r -script ${prefix}/scripts/$script $args
	return $r
    }

    proc scripts {r args} {
	variable prefix
	foreach script $args {
	    dict set r -script ${prefix}/scripts/$script {}
	}
	return $r
    }

    proc theme {r theme} {
	variable prefix
	dict set r -style ${prefix}/themes/$style/$style.all
	return $r
    }

    proc style {r style args} {
	variable prefix
	dict set r -style ${prefix}/css/$style $args
	return $r
    }

    variable defaults {}

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
	return [join $opts ,]
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
    proc inc {r selector args} {
	return [scripts $r jquery.js jquery.inc.js]
    }

    proc weave {r scripts args} {
	set script [lindex $args end]
	set args [lrange $args 0 end-1]

	set script [string map [dict filter $args key %*] $script]
	if {$script ne ""} {
	    puts stderr "WEAVE: $script"
	    dict append r -content [<script> "\$(document).ready(function()\{\n$script\n\});"]
	}

	if {[dict exists $args css]} {
	    set r [style $r {*}[dict get $args css]]
	}
	return [scripts $r {*}$scripts]
    }
    
    # http://docs.jquery.com/UI/Datepicker
    proc datepicker {r selector args} {
	return [weave $r {
	    jquery.js jquery.timeentry.js
	} %SEL $selector %OPTS [opts confirm $args] {
	    $('%SEL').timeEntry({%OPTS});
	}]
    }

    # http://keith-wood.name/timeEntryRef.html
    proc timeentry {r selector args} {
	return [weave $r {
	    jquery.js jquery.timeentry.js
	}  css jquery.timeentry.css %SEL $selector %OPTS [opts confirm $args] {
	    $('%SEL').timeEntry({%OPTS});
	}]
    }

    # http://www.fyneworks.com/jquery/multiple-file-upload/
    proc multifile {r selector args} {
	return [weave $r {
	    jquery.js jquery.MultiFile.js
	} %SEL $selector %OPTS [opts confirm $args] {
	    $('%SEL').MultiFile({%OPTS});
	}]
    }

    # http://docs.jquery.com/UI/Tabs
    proc tabs {r selector args} {
	return [weave $r {
	    jquery.js jquery.dimensions.js ui.tabs.js
	} %SEL $selector %OPTS [opts confirm $args] {
	    $('%SEL').tabs({%OPTS});
	}]
    }

    # http://docs.jquery.com/UI/Accordian
    proc accordian {r selector args} {
	return [weave $r {
	    jquery.js jquery.dimensions.js ui.accordian.js
	} %SEL $selector %OPTS [opts confirm $args] {
	    $('%SEL').accordian({%OPTS});
	}]
    }

    # http://docs.jquery.com/UI/Resizeables
    proc resizeable {r selector args} {
	return [weave $r {
	    jquery.js jquery.dimensions.js ui.mouse.js
	    ui.resizeable.js
	} %SEL $selector %OPTS [opts confirm $args] {
	    $('%SEL').resizeable({%OPTS});
	}]
    }

    # http://docs.jquery.com/UI/Draggables
    proc draggable {r selector args} {
	return [weave $r {
	    jquery.js jquery.dimensions.js ui.mouse.js
	    ui.draggable.js ui.draggable.ext.js
	} %SEL $selector %OPTS [opts confirm $args] {
	    $('%SEL').draggable({%OPTS});
	}]
    }

    # http://docs.jquery.com/UI/Droppables
    proc droppable {r selector args} {
	return [weave $r {
	    jquery.js jquery.dimensions.js ui.mouse.js
	    ui.draggable.js ui.draggable.ext.js
	    ui.droppable.js ui.droppable.ext.js
	} %SEL $selector %OPTS [opts confirm $args] {
	    $('%SEL').droppable({%OPTS});
	}]
    }

    # http://docs.jquery.com/UI/Sortables
    proc sortable {r selector args} {
	return [weave $r {
	    jquery.js jquery.dimensions.js ui.mouse.js
	    ui.draggable.js ui.draggable.ext.js
	    ui.droppable.js ui.droppable.js ui.sortable.js
	} %SEL $selector %OPTS [opts confirm $args] {
	    $('%SEL').sortable({%OPTS});
	}]
    }

    # http://docs.jquery.com/UI/Selectables
    proc selectable {r selector args} {
	return [weave $r {
	    jquery.js jquery.dimensions.js ui.mouse.js
	    ui.draggable.js ui.draggable.ext.js
	    ui.droppabe.js ui.droppable.js
	    ui.selectable.js
	} %SEL $selector %OPTS [opts confirm $args] {
	    $('%SEL').selectable({%OPTS});
	}]
    }

    # http://www.aclevercookie.com/facebook-like-auto-growing-textarea/
    proc autogrow {r selector args} {
	return [weave $r {jquery.js jquery.autogrow.js
	} %SEL $selector %OPTS [opts confirm $args] {
	    $('%SEL').autogrow({%OPTS});
	}]
    }

    # http://bassistance.de/jquery-plugins/jquery-plugin-tooltip/
    proc tooltip {r selector args} {
	return [weave $r {
	    jquery.js jquery.dimensions.js
	    jquery.tooltip.js
	} %SEL $selector %OPTS [opts confirm $args] {
	    $('%SEL').Tooltip({%OPTS});
	}]
    }

    # http://plugins.jquery.com/project/HoverImageText
    proc hoverimage {r selector args} {
	return [weave $r {
	    jquery.js jquery.hoverimagetext.js
	} %SEL $selector %OPTS [opts confirm $args] {
	    $('%SEL').HoverImageText({%OPTS});
	}]
    }

    # http://benjaminsterling.com/jquery-jqgalview-photo-gallery/
    proc gallery {r selector args} {
	return [weave $r {
	    jquery.js jquery.galview.js
	} %SEL $selector %OPTS [opts confirm $args] {
	    $('%SEL').jqGalView({%OPTS});
	}]
    }

    proc aaccordian {r selector args} {
	return [weave $r {
	    jquery.js jquery.accordian.js
	} %SEL $selector %OPTS [opts confirm $args] {
	    $('%SEL').Accordian({%OPTS});
	}]
    }

    proc form  {r selector fn} {
	return [weave $r {
	    jquery.js jquery.form.js
	} %SEL $selector %FN fn {$('%SEL').ajaxForm(%FN);}]
    }
    
    # http://bassistance.de/jquery-plugins/jquery-plugin-validation/
    proc validate {r selector args} {
	return [weave $r {
	    jquery.js jquery.delegate.js
	    jquery.maskedinput.js jquery.metadata.js
	    jquery.validate.js jquery.validate-ext.js
	} %SEL $selector %OPTS [opts confirm $args] {
	    $('%SEL').validate({%OPTS});
	}]
    }

    proc confirm {r selector args} {
	return [weave $r {
	    jquery.js jquery.confirm.js
	} %SEL $selector %OPTS [opts confirm $args] {
	    $('%SEL').confirm({%OPTS});
	}]
    }

    proc ingrid {r selector args} {
	return [weave $r {
	    jquery.js jquery.ingrid.js
	} css ingrid.css %SEL $selector %OPTS [opts ingrid $args] {
	    $('%SEL').ingrid({%OPTS});
	}]
    }

    variable root [file dirname [info script]]
    variable prefix /jquery/
    proc do {r} {
	fs do $r
    }

    proc init {args} {
	if {$args ne {}} {
	    variable {*}$args
	}
	
	# construct a File wrapper for the jscript dir
	variable prefix
	variable root
	File ::jQ::fs -prefix $prefix -root $root
	
	return $prefix
    }
    
    namespace export -clear *
    namespace ensemble create -subcommands {}
}
