package require Html
package require File
package require Debug
Debug off jq 10

package provide jQ 1.0
package provide JQ 1.0

set API(Domains/JQ) {
    {
	jQ domain provides a tight integration to the jQuery web framework.  It enables the application to load jQuery plugins and modules (such as the jQuery UI module) in a convenient manner.

	== Supported Plugins ==

	General form of Plugin interfaces is [[jQ ''plugin'' selector args]] where ''selector'' is a jQuery selector expression, and args will be passed to the main entry point of the plugin.

	;jframe: [http://garage.pimentech.net/scripts_doc_jquery_jframe/] jFrame provides an easy way to get an HTML frame-like behaviour on DIV Elements with AJAX.
	;jtemplates: [http://jtemplates.tpython.com/] a template engine for JavaScript.
	;history: [http://stilbuero.de/jquery/history/] plugin for enabling history support and bookmarking
	;<ready>:
	;datepicker: [http://docs.jquery.com/UI/Datepicker] configurable plugin that adds datepicker functionality
	;timeentry: [http://keith-wood.name/timeEntryRef.html] sets an input field up to accept a time value
	;hint: [http://remysharp.com/2007/01/25/jquery-tutorial-text-box-hints/] Show a 'hint' inside the input box when it is not in focus
	;boxtoggle: [http://www.stainlessvision.com/collapsible-box-jquery] takes a container and hides all of it's content apart from the heading
	;tablesorter: [http://tablesorter.com/] Flexible client-side table sorting
	;multifile: [http://www.fyneworks.com/jquery/multiple-file-upload/] non-obstrusive plugin that helps users easily select multiple files for upload quickly and easily
	;containers: [http://plugins.jquery.com/project/mbContainerPlus] full featured and fully skinnable containers.
	;container: format up a container for the mbContainerPlus plugin
	;tabs: [http://docs.jquery.com/UI/Tabs] 
	;addtab:
	;gentab:
	;accordion: [http://docs.jquery.com/UI/Accordion] Accordion widget
	;dict2accordion:
	;resizable: [http://docs.jquery.com/UI/Resizeables] 
	;draggable: [http://docs.jquery.com/UI/Draggables]
	;droppable: [http://docs.jquery.com/UI/Droppables]
	;sortable: [http://docs.jquery.com/UI/Sortables]
	;selectable: [http://docs.jquery.com/UI/Selectables]
	;autogrow:[http://www.aclevercookie.com/facebook-like-auto-growing-textarea/] autogrowing text area
	;autoscale: [http://jquery.autoscale.js.googlepages.com/] Scale an element to browser window size
	;tooltip: [http://bassistance.de/jquery-plugins/jquery-plugin-tooltip/] Display a customized tooltip instead of the default one for every selected element.
	;hoverimage: [http://plugins.jquery.com/project/HoverImageText] create images along with descriptive text that is displayed on mouse over, similar to a tool hip, however the text is overlayed over the image.
	;galleria: [http://monc.se/kitchen] image gallery
	;gallery: [http://benjaminsterling.com/jquery-jqgalview-photo-gallery/] another image gallery
	;editable: [http://www.appelsiini.net/projects/jeditable] in-place editing
	;form: [http://malsup.com/jquery/form/] easily and unobtrusively upgrade HTML forms to use AJAX - numerous options which allows you to have full control over how the data is submitted.
	;validate: [http://bassistance.de/jquery-plugins/jquery-plugin-validation/] form validation
	;autofill: [http://plugins.jquery.com/project/Autofill] auto-fill a form
	;confirm: [http://nadiaspot.com/jquery/confirm] displays a confirmation message in place before doing an action.
	;ingrid: [http://reconstrukt.com/ingrid/] unobtrusively add datagrid behaviors (column resizing, paging, sorting, row and column styling, and more) to tables.
	;map: [http://code.google.com/p/jmaps/] API to create and manage multiple google maps on any page. 

	== General API ==
	jQ package exports functions to load and invoke jQ plugins
    }
    expires {when do these javascript files expire?}
    google {use the google versions of jQuery wherever possible}
}

namespace eval jQ {
    variable root [file dirname [info script]]
    variable mount /jquery/
    variable expires 0
    variable google 0

    proc postscript {script args} {
	variable mount
	return [list -postscript [file join $mount scripts $script] $args]
    }

    proc script {r script args} {
	variable mount
	dict set r -postscript [file join $mount scripts $script] $args
	return $r
    }

    proc postscript {r script} {
	dict set r -postscript ![clock microseconds] [<script> $script]
	return $r
    }

    proc scripts {r args} {
	variable mount
	Debug.jq {PRESCRIPT: [Dict get? $r -postscript]}

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
	    dict set r -postscript [file join $mount scripts $script] {}
	}
	Debug.jq {SCRIPT: [dict get $r -postscript]}
	return $r
    }

    proc theme {r theme} {
	variable mount
	dict set r -style [file join $mount themes $theme ui.all.css] {}
	return $r
    }

    proc style {r style args} {
	variable mount
	dict set r -style [file join $mount css $style] $args
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
	containers {
	}
	container {
	    buttons 'm'
	    skin 'stiky'
	    icon 'browser.png'
	    width '80%'
	}
    }

    proc opts {type args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}

	set opts {}
	variable defaults
	if {$type ne "" && [dict exists $defaults $type]} {
	    set args [list {*}[dict get $defaults $type] {*}$args]
	}
	dict for {n v} $args {
	    lappend opts "$n:$v"
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
	    jquery.js jquery.ui.js
	} %SEL $selector %OPTS [opts datepicker {*}$args] {
	    $('%SEL').datepicker(%OPTS);
	}]
    }

    # http://keith-wood.name/timeEntryRef.html
    proc timeentry {r selector args} {
	return [weave $r {
	    jquery.js jquery.timeentry.js
	}  css jquery.timeentry.css %SEL $selector %OPTS [opts timeentry {*}$args] {
	    $('%SEL').timeEntry(%OPTS);
	}]
    }

    # http://remysharp.com/2007/01/25/jquery-tutorial-text-box-hints/
    proc hint {r {selector input[title!=""]} args} {
	return [weave $r {
	    jquery.js jquery.hint.js
	}  %SEL $selector %OPTS [opts hint {*}$args] {
	    $('%SEL').hint(%OPTS);
	}]
    }

    # http://www.stainlessvision.com/collapsible-box-jquery
    proc boxtoggle {r selector args} {
	return [weave $r {
	    jquery.js jquery.boxtoggle.js
	}  %SEL $selector %OPTS [opts boxtoggle {*}$args] {
	    boxToggle('%SEL');
	}]
    }

    # http://tablesorter.com/addons/pager/jquery.tablesorter.pager.js
    proc tablesorter {r selector args} {
	return [weave $r {
	    jquery.js jquery.metadata.js jquery.tablesorter.js
	}  %SEL $selector %OPTS [opts tablesorter {*}$args] {
	    $('%SEL').tablesorter(%OPTS);
	}]
    }

    # http://www.fyneworks.com/jquery/multiple-file-upload/
    proc multifile {r} {
	# just supports the simple case
	return [scripts $r jquery.js jquery.MultiFile.js]
    }

    # http://plugins.jquery.com/project/mbContainerPlus
    proc containers {r selector args} {
	set r [style $r mbContainer.css]	;# ensure the css is loaded

	variable mount
	dict set args elementsPath '[file join $mount elements]/'
	dict set args containment 'document'

	return [weave $r {
	    jquery.js jquery.ui.js jquery.metadata.js jquery.container.js
	} %SEL $selector %OPTS [opts containers {*}$args] {
	    $('%SEL').buildContainers(%OPTS);
	}]
    }

    # format up a container for the mbContainerPlus plugin
    proc container {args} {
	# grab content (if any)
	if {[llength $args]%2} {
	    set content [lindex $args end]
	    set args [lrange $args 0 end-1]
	} else {
	    set content ""
	}

	# class supplied?
	set class [dict get? $args class]; catch {dict unset args class}
	if {$class ne ""} {
	    set class [list class $class]
	}

	# id supplied?
	set id [dict get? $args id]; catch {dict unset args id}
	if {$id ne ""} {
	    set id [list id $id]
	}

	# set up the funky <div> structure this thing needs
	append C [<div> class ne [<div> class n [dict get? $args title]]] \n; catch {dict unset args title}
	append C [<div> class o [<div> class e [<div> class c [<div> {*}$id class mbcontainercontent {*}$class $content]]]] \n
	append C [<div> class so [<div> class se [<div> class s {}]]] \n
	set C [<div> class no $C]\n

	# assemble the classes
	set class containerPlus

	# draggable?
	set draggable [dict get? $args draggable]; catch {dict unset args draggable}
	if {$draggable ne "" && $draggable} {
	    lappend class draggable
	}

	# resizable?
	set resizable [dict get? $args resizable]; catch {dict unset args resizable}
	if {$resizable ne "" && $resizable} {
	    lappend class resizable
	}

	set attrs ""
	if {0} {
	    # collapsed?
	    set collapsed [dict get? $args collapsed]; catch {dict unset args collapsed}
	    if {$collapsed ne "" && $collapsed} {
		lappend attrs collapsed true
	    }
	    # iconized?
	    set iconized [dict get? $args iconized]; catch {dict unset args iconized}
	    if {$iconized ne "" && $iconized} {
		lappend attrs iconized true
	    }
	}

	# fetch style arg, if any
	set style [dict get? $args style]; catch {dict unset args style}

	# assemble the metadata in a 'class' element
	set opts [string map {' \"} [opts container {*}$args]]
	if {$opts ne ""} {
	    #lappend class $opts
	    set opts [list class $opts]
	}

	return [<div> class $class {*}$opts {*}[expr {[llength $style]?[list style [join $style ;]]:{}}] {*}$attrs $C\n]
    }

    # http://docs.jquery.com/UI/Tabs
    proc tabs {r selector args} {
	return [weave $r {
	    jquery.js jquery.ui.js
	} %SEL $selector %OPTS [opts tabs {*}$args] {
	    $('%SEL').tabs(%OPTS);
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

    proc dict2accordion {dict args} {
	set result {}
	foreach {n v} $dict {
	    lappend result [<div> "[<a> href #$n $n]\n[<div> $v]"]
	}
	return [<div> {*}$args [join $result \n]]
    }

    # http://docs.jquery.com/UI/Accordion
    proc accordion {r selector args} {
	return [weave $r {
	    jquery.js jquery.ui.js
	} %SEL $selector %OPTS [opts accordion {*}$args] {
	    $('%SEL').accordion(%OPTS);
	}]
    }

    # http://docs.jquery.com/UI/Resizeables
    proc resizable {r selector args} {
	return [weave $r {
	    jquery.js jquery.ui.js
	} %SEL $selector %OPTS [opts resizable {*}$args] {
	    $('%SEL').resizable(%OPTS);
	}]
    }

    # http://docs.jquery.com/UI/Draggables
    proc draggable {r selector args} {
	return [weave $r {
	    jquery.js jquery.ui.js
	} %SEL $selector %OPTS [opts draggable {*}$args] {
	    $('%SEL').draggable(%OPTS);
	}]
    }

    # http://docs.jquery.com/UI/Droppables
    proc droppable {r selector args} {
	return [weave $r {
	    jquery.js jquery.ui.js
	} %SEL $selector %OPTS [opts droppable {*}$args] {
	    $('%SEL').droppable(%OPTS);
	}]
    }

    # http://docs.jquery.com/UI/Sortables
    proc sortable {r selector args} {
	return [weave $r {
	    jquery.js jquery.ui.js
	} %SEL $selector %OPTS [opts sortable {*}$args] {
	    $('%SEL').sortable(%OPTS);
	}]
    }

    # http://docs.jquery.com/UI/Selectables
    proc selectable {r selector args} {
	return [weave $r {
	    jquery.js jquery.ui.js
	} %SEL $selector %OPTS [opts selectable {*}$args] {
	    $('%SEL').selectable(%OPTS);
	}]
    }

    # http://www.aclevercookie.com/facebook-like-auto-growing-textarea/
    proc autogrow {r selector args} {
	return [weave $r {jquery.js jquery.autogrow.js
	} %SEL $selector %OPTS [opts autogrow {*}$args] {
	    $('%SEL').autogrow(%OPTS);
	}]
    }

    # http://jquery.autoscale.js.googlepages.com/
    proc autoscale {r selector args} {
	return [weave $r {jquery.js jquery.autoscale.js
	} %SEL $selector %OPTS [opts autoscale {*}$args] {
	    $('%SEL').autoscale(%OPTS);
	}]
    }

    # http://bassistance.de/jquery-plugins/jquery-plugin-tooltip/
    proc tooltip {r selector args} {
	return [weave $r {
	    jquery.js jquery.dimensions.js
	    jquery.tooltip.js
	} %SEL $selector %OPTS [opts tooltip {*}$args] {
	    $('%SEL').Tooltip(%OPTS);
	}]
    }

    # http://plugins.jquery.com/project/HoverImageText
    proc hoverimage {r selector args} {
	return [weave $r {
	    jquery.js jquery.hoverimagetext.js
	} %SEL $selector %OPTS [opts hoverimage {*}$args] {
	    $('%SEL').HoverImageText(%OPTS);
	}]
    }

    proc galleria {r selector args} {
	return [weave $r {
	    jquery.js jquery.galleria.js
	} %SEL $selector %OPTS [opts galleria {*}$args] {
	    $('ul.%SEL').galleria(%OPTS)
	}]
    }

    # http://benjaminsterling.com/jquery-jqgalview-photo-gallery/
    proc gallery {r selector args} {
	return [weave $r {
	    jquery.js jquery.galview.js
	} %SEL $selector %OPTS [opts gallery {*}$args] {
	    $('%SEL').jqGalView(%OPTS);
	}]
    }

    proc aaccordion {r selector args} {
	return [weave $r {
	    jquery.js jquery.accordion.js
	} %SEL $selector %OPTS [opts aaccordion {*}$args] {
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
	} %SEL $selector %OPTS [opts editable {*}$args] {*}$pre %FN $fn {
	    $('%SEL').editable(%FN,%OPTS);
	}]
    }

    # http://malsup.com/jquery/form/
    proc form {r selector args} {
	return [weave $r {
	    jquery.js jquery.form.js
	} %SEL $selector %OPTS [opts validate {*}$args] {
	    $('%SEL').ajaxForm(%OPTS);
	}]
    }
    
    # http://bassistance.de/jquery-plugins/jquery-plugin-validation/
    proc validate {r selector args} {
	return [weave $r {
	    jquery.js jquery.delegate.js
	    jquery.maskedinput.js jquery.metadata.js
	    jquery.validate.js jquery.validate-ext.js
	} %SEL $selector %OPTS [opts validate {*}$args] {
	    $('%SEL').validate(%OPTS);
	}]
    }

    proc autofill {r selector args} {
	return [weave $r {
	    jquery.js jquery.autofill.js
	} %SEL $selector %OPTS [opts autofill {*}$args] {
	    $('%SEL').autofill(%OPTS);
	}]
    }

    proc confirm {r selector args} {
	return [weave $r {
	    jquery.js jquery.confirm.js
	} %SEL $selector %OPTS [opts confirm {*}$args] {
	    $('%SEL').confirm(%OPTS);
	}]
    }

    proc ingrid {r selector args} {
	return [weave $r {
	    jquery.js jquery.ingrid.js
	} css ingrid.css %SEL $selector %OPTS [opts ingrid {*}$args] {
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
	} %SEL $selector %OPTS [opts map {*}$args] %CALL $callback {
	    $('%SEL').jmap('init', %OPTS, %CALL);
	}]
    }

    proc sheet {r selector args} {
	return [weave $r {
	    jquery.js jquery.clickmenu.js jquery.sheet.calc.js jquery.sheet.js
	} css clickable.css %SEL $selector %OPTS [opts sheet {*}$args] {
	    $('%SEL').sheet(%OPTS);
	}]
    }

    proc do {r} {
	fs do $r
    }

    proc new {args} {
	variable {*}$args
	
	# construct a File wrapper for the jscript dir
	variable root; variable mount; variable expires
	set mount /[string trim $mount /]/

	if {[info commands ::jQ::fs] eq ""} {
	    File create ::jQ::fs {*}$args root $root mount $mount expires $expires
	}
	return jQ
    }
    
    namespace export -clear *
    namespace ensemble create -subcommands {}
}

interp alias {} JQ {} jQ	;# convention - Domain names start with U/C char
