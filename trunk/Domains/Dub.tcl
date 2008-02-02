# Dub - Db toy

package provide Dub 1.0

package require Form
package require View
package require Cookies
package require Responder
package require jQ

namespace eval Dub {
    variable db /tmp/dub.db
    variable toplevel
    variable prefix /dub

    # default page
    proc /default {r args} {
	set content [subst {
	    [<h1> "Dub - A Wub Toy"]
	    [<p> "Dub is a Wub application for databases"]
	    [<p> "Dub manages [<a> href http://www.equi4.com/metakit/ Metakit] databases."]
	    [<p> "Dub creates and edits [<a> href views Views] and web [<a> href displays Displays] of those views."]
	}]

	set r [Html style $r css/form.css]

	return [Http NoCache [Http Ok $r $content style/dub]]
    }

    # displays page
    proc /displays {r args} {
	set content [subst {
	    [<h1> "Dub Displays - A Wub Toy"]
	    [<p> "Create a new view or edit an existing display by selecting from the left margin"]
	}]

	set r [Html style $r css/form.css]

	return [Http NoCache [Http Ok $r $content style/dub]]
    }

    # views page
    proc /views {r args} {
	set content [subst {
	    [<h1> "Dub Views - A Wub Toy"]
	    [<p> "A view is a collection of fields which possess certain properties which can be edited.  Dub maintains metadata for each field within each view, allowing the simple construction of editing and reporting programs."]
	    [<p> "Create a new view or edit an existing view by selecting from the left margin"]
	}]

	set r [Html style $r css/form.css]

	return [Http NoCache [Http Ok $r $content style/dub]]
    }



    alias 2native dict get {I integer S string B binary D double F float L long "" string}

    # convert the metakit view to our field/view tables
    proc reflect {view} {
	# reflect view to view table
	if {[catch {
	    vview find view $view
	}]} {
	    vview append view $view lastmod [clock clicks] flushed [clock clicks]
	}
	
	# reflect fields to field table
	foreach f [split [v$view properties]] {
	    lassign [split $f :] field type
	    puts stderr "reflect: $view $field of type '$type'"
	    if {[catch {
		vfield find view $view name $field
	    } index eo]} {
		#puts stderr "reflect adding: $view.$field of type [2native $type]"
		vfield append view $view name $field type [2native $type]
	    } elseif {$type ne [vfield get $index type]} {
		# got a field record, update type
		vfield set $index type $type
	    }
	}
    }
    
    # construct View object for a given view,
    # reflect View's properties to the field table.
    proc mkview {view {layout ""}} {
	set layout [join $layout]
	if {$layout ne ""} {
	    puts stderr "mkview: making $view of '$layout'"
	    mk::view layout db.$view $layout
	    puts stderr "mkview: made $view of '$layout'"
	}

	if {[catch {View init v$view db.$view} r eo]} {
	    puts stderr "View: $r ($eo)"
	} else {
	    puts stderr "cmd: $r - v$view - [info commands v*]"
	}

	variable views
	set views($view) v$view
	puts stderr "mkview: [mk::file views db]"
    }

    # convert the field table field descriptors to mk view's active layout
    proc 2view {view} {
	set layout {}
	vfield with {
	    lappend layout "$field:[type::$type::native $name]"
	} view $view

	variable views
	if {[info exists views($view)]} {
	    mk::view layout db.$view [join $layout]
	} else {
	    mkview $view [join $layout]
	}
    }

    variable css_cache; array set css_cache {}
    proc /css/form {r {name dubform}} {
	variable css_cache
	catch {dict unset r -dynamic}
	if {[info exists css_cache($name)]} {
	    set css $css_cache($name)
	} else {
	    set css [string map [list %F $name] {
		form.%F fieldset {
		    margin-bottom: 1em;
		}
		form.%F legend {
		    padding: 0 2px;
		    font-weight: bold;
		}
		form.%F label {
		    display: inline-block;
		    line-height: 1.8;
		    vertical-align: top;
		}
		form.%F fieldset ol {
		    margin: 0;
		    padding: 0;
		}
		form.%F fieldset li {
		    list-style: none;
		    padding: 5px;
		    margin: 0;
		}
		form.%F fieldset fieldset {
		    border: none;
		    margin: 3px 0 0;
		}
		form.%F fieldset fieldset legend {
		    padding: 0 0 5px;
		    font-weight: normal;
		}
		form.%F fieldset fieldset label {
		    display: block;
		    width: auto;
		}
		form.%F em {
		    font-weight: bold;
		    font-style: normal;
		    color: #f00;
		}
		form.%F label.error {
		    color: red;
		    margin-left: 103px;
		    width: 220px;
		}
		
		form.%F {
		    font-size: 1.0em;
		    color: #333;
		}
		
		form.%F legend {
		    padding-left: 0;
		}
		
		form.%F legend, form.%F label {
		    color: #333;
		}
		
		form.%F fieldset {
		    border: none;
		    border-top: 1px solid #C9DCA6;
		    background-color: #F8FDEF;
		}
		
		form.%F fieldset fieldset {
		    background: none;
		}
		
		form.%F fieldset p, form.%F fieldset fieldset {
		    padding: 5px 10px 7px;
		}
	    
		form.%F label.error, label.error {
		    /* remove the next line when you have trouble in IE6 with labels in list */
		    display: none;
		    color: red;
		    font-style: italic
		}
		div.error { display: none; }
		input {	border: 1px solid black; }
		input:focus { border: 1px dotted black; }
		input.error { border: 1px dotted red; }
		form.%F .gray * { color: gray; }
	    }]
	}

	dict set r -content $css
	dict set r content-type text/css

	return [Http CacheableContent [Http Ok $r] [clock seconds]]
    }

    # edit field properties
    proc /fieldE {r view name type comment {unique 0} {key 0} {hidden 0}} {
	puts stderr "/fieldE: $view.$name of $type key:$key"
	if {![regexp {^[a-zA-Z][a-zA-Z_0-9]*$} $name]} {
	    return [/view $r $view message "Field names must be alphanumeric"]
	}
	if {"::Dub::type::$type" ni [namespace children ::Dub::type]} {
	    return [/view $r $view message "Field type '$type' is not known"]
	}

	if {[catch {
	    vview find view $view
	} index eo]} {
	    vview append view $view lastmod [clock clicks] flushed 0
	} else {
	    vview set $index lastmod [clock clicks]
	}

	if {[catch {
	    vfield find view $view name $name
	} index eo]} {
	    puts stderr "/fieldE new $view.$name"
	    vfield append name $name view $view type $type comment $comment unique $unique key $key hidden $hidden
	} else {
	    puts stderr "/fieldE edit $view.$name: $index"
	    vfield set $index type $type comment $comment unique $unique key $key hidden $hidden
	}

	return [/view $r $view]
    }

    variable page {}
    proc init_page {} {
	variable page
	variable prefix
	dict set page globlinks [subst {
	    Home /
	    Dub ${prefix}
	}]
	dict set page global [<div> [<form> search action index.html {
	    [<text> q size 15 maxlength 250]
	    [<image> submit src search.png alt Search]
	}]]
	dict set page header [<h1> "Wub[<span> class fade Dub]"]
	dict set page sitelinks [subst {
	    Home /
	    Dub ${prefix}
	    Views ${prefix}views
	    Displays ${prefix}displays
	}]
	dict set page breadcrumbs {}
	
	dict set page navbox {}
	dict set page copyright {}
	dict set page footlinks {}
	dict set page footer ""
    }

    proc views-sidebar {contents} {
	set views {}
	set sysv {}
	foreach {rec val} [vview with {
	    list $system $view view?view=$view
	}] {
	    set val [lassign $val system]
	    if {$system} {
		lappend sysv {*}$val
	    } else {
		lappend views {*}$val
	    }
	}
	
	set contents [Sinorca sidebar $contents title "New View" [<form> view class dubform action view {[<text> view size 16 ""]}]]
	set contents [Sinorca sidebar $contents title "User Views" [Html ulinks  $views]]
	set contents [Sinorca sidebar $contents title "Dub Views" [Html ulinks $sysv]]
	return $contents
    }

    proc displays-sidebar {contents} {
	return $contents
    }

    proc dub-sidebar {contents} {
	return $contents
    }

    proc .style/dub.style/sinorca {rsp} {
	variable page
	set contents $page
	dict set contents content [dict get $rsp -content]

	switch -- [file tail [dict get $rsp -path]] {
	    views {
		set contents [views-sidebar $contents]
	    }
	    displays {
		set contents [displays-sidebar $contents]
	    }
	    fieldE -
	    view {
	    }
	    default {
		set contents [dub-sidebar $contents]
	    }
	}
	if {[dict exists $rsp -sidebar]} {
	    set contents [Sinorca sidebar $contents {*}[dict get $rsp -sidebar]]
	}

	dict set rsp -content $contents

	dict lappend rsp -headers [<stylesheet> /sinorca/screen.css]
	return $rsp
    }

    proc /cdisplay {r args} {
	if {![dict exists $args display]} {
	    dict set args display [dict get $args view]
	}
	set view [Dict get? $args view]
	if {$view ne ""} {
	    set drags [<div> class src [subst {
		[<p> "View: [dict get $args view]"]
		[join [dict values [vfield with {
		    <div> class field "$view<br>$name"
		} view $view]] \n]
	    }]]
	}
	puts stderr "CDISP: $drags"
	set dropzone [<div> class dst [<p> "Display: [dict get $args display]"]]
	
	lappend r -headers [list [<style> [subst {
	    .field {
		float: left;
		width: 5em;
		height: 3em;
		background-color: #68BFEF;
		border: 2px solid #0090DF;
		padding: 0.125em;	
		text-align: center;
		margin: 0.5em;
		cursor: pointer;
		z-index: 100;
	    }
	    .el {
		float: left;
		width: 5em;
		height: 3em;
		background-color: #68BFEF;
		border: 2px solid #0090DF;
		padding: 0.125em;	
		text-align: center;
		margin: 0.5em;
		cursor: pointer;
		z-index: 100;
	    }
	    .dst {
		float: left;
		width: 20em;
		/*height: 20em;*/
		background-color: gray; /*#e9b96e*/
		/*border: 3px double #0090DF;*/
		padding: 5px;
		text-align: center;
		overflow: auto;
		opacity: 0.7;
		margin: 2em;
	    }
	    .src {
		float: left;
		width: 20em;
		/*height: 20em;*/
		background-color: gray; /*#e9b96e*/
		/*border: 3px double #0090DF;*/
		padding: 5px;
		text-align: center;
		overflow: auto;
		opacity: 0.7;
		margin: 2em;
	    }
	}]]]

	append content $dropzone
	append content $drags
	append content [<ready> {
	    function swap (what, from, to, whence, where) {
		var x = $(document.createElement('div'));
		x.addClass(to);
		
		x.html($(what).html());
		x.click(function() {
		    swap($(this), to, from, where, whence);
		});
		$(where).append(x);
		$(what).remove();
	    }
	    $('.field').click(function() {
		swap($(this), 'field', 'el', '.src', '.dst');
	    });
	    $('.el').click(function() {
		swap($(this), 'el', 'field', '.dst', '.src');
	    });
	}]

	append xcontent [<ready> {
	    $('.field').draggable(); /* {helper: 'clone'} */

	    $('.dst').droppable({accept:'.field', tolerance: 'fit',
		drop: function(ev, ui) {
		    var x = $(document.createElement('div'));
		    x.draggable();
		    x.addClass('el');
		    x.text($(ui.draggable.element).text());

		    $(ui.draggable.element).remove();
		    $(this).append(x);
		    return 1;
		}});

	    $('.src').droppable({accept:'.el', tolerance: 'fit',
		drop: function(ev, ui) {
		    var x = $(document.createElement('div'));
		    x.draggable();
		    x.addClass('field');
		    x.text($(ui.draggable.element).text());

		    $(ui.draggable.element).remove();
		    $(this).append(x);
		    return 1;
		}});
	}]
	#set r [jQ droppable $r {}]
	set r [jQ script $r jquery.js]
	return [Http NoCache [Http Ok $r $content style/dub]]
    }

    # construct displays
    proc /display {r {display ""} args} {
	if {[catch {
	    set index [vdisplay find display $display]
	    array set D [vdisplay get $index]
	} err eo]} {
	    return [/default $r]
	}
	set els [velement with {
	    <a> href element?element=${} [join [dict values [vfield get $field view name]] .]
	} display $index]

	append content [<ul> <li>[join [dict values $els] "</li><li>"]]
	return [Http NoCache [Http Ok $r $content style/dub]]
    }

    # create a form for field editing
    proc fieldForm {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}

	array set F {key 0 unique 0 hidden 0 type integer comment "" submit Change}
	array set F $args
	if {$F(name) eq ""} {
	    set F(submit) Create
	}

	return [<form> fieldD action ./fieldE class dubform {
	    [<fieldset> fieldDS {
		[<legend> "Properties"]
		[<hidden> view $F(view)]
		[<text> name class required label "Name: " $F(name)]
		[<select> type label "Type: " class required {
		    [Foreach v [namespace children ::Dub::type] {
			[set v [namespace tail $v]; <option> value $v select? $F(type) $v]
		    }]
		}]
	    }]
	    [<fieldset> fieldFlags {
		[<legend> "Flags"]
		[<checkbox> key checked [expr {$F(key)}] label "Key? " value 1]
		[<checkbox> unique checked [expr {$F(unique)}] label "Unique? " value 1]
		[<checkbox> hidden checked [expr {$F(hidden)}] label "Hidden? " value 1]
	    }]

	    [<fieldset> fieldComment {
		[<legend> "Comment"]
		[<textarea> comment cols 40 compact 1 $F(comment)]
	    }]
		[<submit> Go $F(submit)]
	}]
    }

    # field detail form - enable editing of a field's properties
    proc /fieldD {r {view ""} {name ""}} {
	if {[catch {
	    set F [vfield get [vfield find name $name view $view]]
	} err eo]} {
	    puts stderr "/fieldD '$view.$name' err: $err ($eo)"
	    set F [list type "" name $name view $view key 0 unique 0 comment ""]
	}
	
	puts stderr "/fieldD Record: [array get F]"

	if {[catch {
	    set viewD [vview get [vview find view $view]]
	    set content [<p> [dict get $viewD comment]]
	}]} {
	    set content [<p> Enter new view's key field]
	}

	append content [fieldForm $F]

	#onsubmit { return $('\#fieldD').validate().form(); }
	dict lappend r -header [<style> {
	    .error {
		font-size:11px; color:red; 
		padding:1px; margin:3px;
		font-family:"Lucida Grande", Verdana, Arial, sans-serif;
	    }
	}] 

	set r [Html style $r css/form.css]

	return [Http NoCache [Http Ok $r $content x-text/html-fragment]]
    }

    # map of human comprehensible type names to db typenames by db maker
    namespace eval type {
	proc procText {procName} {
	    set result [list proc [namespace tail $procName]]
	    set formals {}
	    foreach var [info args $procName] {
		if {[info default $procName $var def]} {
		    lappend formals [list $var $def]
		} else {
		    # Still need the list-quoting because variable
		    # names may properly contain spaces.
		    lappend formals [list $var]
		}
	    }
	    return [lappend result $formals [info body $procName]]
	}

	proc derived {type} {
	    foreach {proc} [info procs ::Dub::type::${type}::*] {
		puts "DER: [uplevel namespace current] [procText $proc]"
		namespace eval [uplevel namespace current] [procText $proc]
	    }
	}

	proc type {type body} {
	    namespace eval ::Dub::type::$type {
		# default state for NS
		namespace import ::Dub::type::*
		proc tcl2db {view field value args} {return $value}
		proc db2tcl {view field value args} {return $value}
		proc tcl2display {view field value args} {return $value}
		proc display2tcl {view field value args} {return $value}
	    }
	    namespace eval ::Dub::type::$type $body
	}
	namespace export *
	type integer {
	    proc default {name} {return 0}
	    proc native {name} {return ${name}:I}
	    proc valid {view field value args} {
		string is integer -strict $value
	    }
	    #proc jsvalid {view field value args} {}
	    #proc tcl2db {view field value args} {}
	    #proc db2tcl {view field value args} {}
	    #proc tcl2display {view field value args} {}
	    #proc display2tcl {view field value args} {}
	}

	type string {
	    proc default {name} {return ""}
	    proc native {name} {return ${name}:S}
	    proc valid {view field value args} {
		return 1
		# could check for validly encoded characters
	    }
	    #proc jsvalid {view field value args} {}
	    #proc tcl2db {view field value args} {}
	    #proc db2tcl {view field value args} {}
	    proc tcl2display {view field value args} {
		return [armour $name]
	    }
	    #proc display2tcl {view field value args} {}
	}

	type long {
	    derived integer
	    proc native {name} {return ${name}:L}
	    #proc jsvalid {view field value args} {}
	    #proc tcl2db {view field value args} {}
	    #proc db2tcl {view field value args} {}
	    #proc tcl2display {view field value args} {}
	    #proc display2tcl {view field value args} {}
	}

	type float {
	    proc native {name} {
		return ${name}:F
	    }
	    proc valid {view field value args} {
		string is double -strict $value
	    }
	    #proc jsvalid {view field value args} {}
	    #proc tcl2db {view field value args} {}
	    #proc db2tcl {view field value args} {}
	    #proc tcl2display {view field value args} {}
	    #proc display2tcl {view field value args} {}
	}

	type double {
	    proc native {name} {return ${name}:D}
	    proc valid {view field value args} {
		string is double -strict $value
	    }
	    #proc jsvalid {view field value args} {}
	    #proc tcl2db {view field value args} {}
	    #proc db2tcl {view field value args} {}
	    #proc tcl2display {view field value args} {}
	    #proc display2tcl {view field value args} {}
	}

	type binary {
	    proc native {name} {return ${name}:B}
	    proc valid {view field value args} {return 1}
	    #proc jsvalid {view field value args} {}
	    #proc tcl2db {view field value args} {}
	    #proc db2tcl {view field value args} {}
	    proc tcl2display {view field value args} {
		return [armour $value]
	    }
	    proc display2tcl {view field value args} {}
	}

	type boolean {
	    derived integer
	    proc valid {view field value args} {
		# check view for value
		return [catch {expr {$value ? 1: 0}}]
	    }
	    proc jsvalid {view field value args} {
		# call the original's jsvalid
	    }
	    proc tcl2db {view field value args} {
		expr {$value ? 1:0}
		# call the original's tcl2db then test
	    }
	    #proc db2tcl {view field value args} {}
	    #proc tcl2display {view field value args} {}
	    #proc display2tcl {view field value args} {}
	}

	type link {
	    proc type {name} {
		if {[catch {
		    return [vfield get [vfield find view $name name $name] type]
		} linked eo]} {
		    return integer
		}
	    }
	    proc default {name} {
		return [::Dub::type::[type $name] default $name]
	    }
	    proc native {name} {
		# have to look up in view's index field
		return [::Dub::type::[type $name] native $name]
	    }
	    proc valid {view field value args} {
		# check view for value
		if {[catch {
		    if {![::Dub::type::[type $field] valid $value]} {
			return 0
		    }

		    # now check that the linked to record exists
		    if {[catch {v$field find $field $value}]} {
			return 0
		    } else {
			return 1
		    }
		} linked eo]} {
		    if {![string is integer -strict $value]
			|| $value < 0
			|| ($value > [v$field size])
		    } {
			return 0
		    } else {
			return 1
		    }
		}
	    }

	    proc jsvalid {view field value args} {
		# call the original's jsvalid
		return [::Dub::type::[type $field] jsvalid $view $field $value {*}$args]
	    }
	    proc tcl2db {view field value args} {
		# call the original's tcl2db then test
		return [::Dub::type::[type $field] tcl2db $view $field $value {*}$args]
	    }
	    proc db2tcl {view field value args} {
		# call the original's db2tcl then test
		return [::Dub::type::[type $field] db2tcl $view $field $value {*}$args]
	    }
	    proc tcl2display {view field value args} {
		;# have to look up type in view's field
		return [::Dub::type::[type $field] tcl2display $view $field $value {*}$args]
	    }
	    proc display2tcl {view field value args} {
		;# have to look up type in view's field
		return [::Dub::type::[type $field] display2tcl $view $field $value {*}$args]
	    }
	}

	type autoincr {
	    derived integer
	}

	type list {
	    derived string

	    proc valid {view field value args} {
		expr {[string is list -strict $value]}
	    }
	    proc jsvalid {view field value args} {
		# got to be read-only
	    }
	    #proc tcl2db {view field value args} {}
	    #proc db2tcl {view field value args} {}
	    #proc tcl2display {view field value args} {}
	    #proc display2tcl {view field value args} {}
	}

	type dictionary {
	    derived list

	    proc valid {view field value args} {
		expr {[string is list -strict $value]
		      &&
		      ([llength $value]%2 == 0)
		  }
	    }
	    proc jsvalid {view field value args} {
		# got to be read-only
	    }
	    #proc tcl2db {view field value args} {}
	    #proc db2tcl {view field value args} {}
	    #proc tcl2display {view field value args} {}
	    #proc display2tcl {view field value args} {}
	}
    }

    # ensure View is up to date - return the view record
    proc update {view} {
	set index [vview find view $view]
	array set viewd [vview get $index]
	if {$viewd(lastmod) < $viewd(flushed)} {
	    # view view is out of date - update it
	    set keys {}
	    set fields {}
	    vfield with {
		lappend fields $name $type
		if {$key} {
		    lappend keys $name
		}
	    } view $view
	    vview set $index keys $keys fields $fields flushed [clock clicks]
	    return [vview get $index]
	} else {
	    return [array get viewd]
	}
    }

    proc q2keysfields {view args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	array set viewd [update $view]	;# get latest view record
	set fields {}
	set keys {}
	foreach {n v} $args {
	    if {$n in $viewd(keys)} {
		lappend keys $n [type::[dict get $viewd(fields) $n]::tcl2db $view $n $v]
	    } else {
		lappend fields $n [type::[dict get $viewd(fields) $n]::tcl2db $view $n $v]
	    }
	}
	return [list $keys $fields]
    }

    # given some keys fetch a record from view
    proc fetch {view args} {
	lassign [q2keysfields $view {*}$args] keys fields
	set record {}
	set fetch [v$view get [v$view find {*}$keys] {*}$[dict keys $fields]]
	foreach {n v} $fetch {
	    lappend record $n [type::[dict get $viewd(fields) $n]::db2tcl $view $n $v]
	}
	return $record
    }

    proc setfield {view find args} {
	puts stderr "setfield: view '$view' keys '$find' = '$args'"
	v$view set [v$view find {*}$find] {*}$args
    }

    # given some values store a record in view
    proc store {view args} {
	lassign [q2keysfields $view {*}$args] keys fields
	return [v$view set [v$view find {*}$keys] {*}$fields]
    }

    # return a table of field names which inject pages into a jframe
    proc /fields {r {view ""} {target ""}} {
	if {$target ne ""} {
	    set target [list target $target]
	}

	set fieldrefs [dict values [vfield with {
	    <tr> [<td> [<a> href fieldD?view=$view&name=$name {*}$target $name]]
	} view $view]]

	set content [<table> id selector style {margin-top: 1em;} [subst {
	    [<thead> [<tr> [<th> Fields]]]
	    [<tbody> [join $fieldrefs \n]]
	}]]

	return [Http NoCache [Http Ok $r $content x-text/html-fragment]] 
    }

    # display a view's fields, allowing editing
    proc /view {r {view ""} args} {
	set select fields
	set detail fieldD

	puts stderr "/view $view: start"
	if {[catch {vdisplay find display $view}]} {
	    set ddisplay [<a> href cdisplay?view=$view "Create Display"]
	} else {
	    set ddisplay [<a> href display?display=$view "Default Display"]
	}
	dict set r -sidebar [list title "View $view" [subst {
	    $ddisplay
	}]]

	if {[catch {
	    vfield find view $view
	    puts stderr "/view existing $view"
	} err eo]} {
	    puts stderr "/view new '$view': $err ($eo)"
	    set fields [list view $view name $view key 1 type integer submit "New View"]
	    set content [subst {
		[<p> "Create a new View.  Enter view's key field"]
		[fieldForm $fields]
	    }]

	    return [Http NoCache [Http Ok $r $content style/dub]]
	}
	#[<div> id select style $style target detail src ${select}?view=$view&target=detail {}]

	# set up a jframe pair - selector on the left, detail on the right
	set r [jQ jframe $r]
	set style {float:left; margin-left: 5em; border:1;}
	if {[dict exists $args message]} {
	    append content [<p> [dict get $args message]] \n
	}
	append content [subst {
	    [<ready> [string map [list %M {
		<b>loading...</b>
	    }] {
		jQuery.fn.waitingJFrame = function () {
		    /*$(this).html("%M");*/
		}
	    }]]
	    [<div> id select style $style target detail src ${select}?view=$view&target=detail {}]
	    [<div> id detail style $style src ${detail}?view=$view {}]
	}]

	# add style for validation errors
	dict lappend r -header [<style> {
	    .error {
		font-size:11px; color:red; 
		padding:1px; margin:3px;
		font-family:"Lucida Grande", Verdana, Arial, sans-serif;
	    }
	}]

	#return [jQ jframe [jQ ingrid [jQ validate $r ""] ""]]
	return [Http NoCache [Http Ok $r $content style/dub]]
    }

    proc init {args} {
	if {$args ne {}} {
	    variable {*}$args
	}
	init_page
	Convert Namespace ::Dub

	# try to open wiki view
	if {[catch {
	    mk::file views db
	    # we expect threads to be created *after* the db is initialized
	    variable toplevel 0
	} vlist eo]} {
	    #puts stderr "views: $vlist ($eo)"
	    # this is the top level process, need to create a db
	    variable toplevel 1
	    variable db
	    mk::file open db $db -shared
	}

	if {![info exists views(field)]} {
	    Debug on view 10
	    mkview field {
		name:S view:S type:S
		key:I unique:I hidden:I
		comment:S
	    }
	    mkview view {
		view:S
		system:I
		fields:S keys:S
		lastmod:L flushed:L
		comment:S
	    }
	    mkview display {
		display:S
		elements:S
		comment:S
	    }
	    mkview element {
		display:S
		name:S
		field:I
		order:I
	    }

	    foreach {v det comment} {
		field {
		    view link "the view this field belongs to"
		    name string "the name of this field"
		    type string "the type of this field"
		    key boolean "is this field a key field?"
		    unique boolean "is this key field unique?"
		    hidden boolean "is this field usually hidden?"
		    comment string "a description of this field"
		} "Field details"

		view {
		    view string "a database view or table"
		    fields dictionary "a calculated dict of field name -> type"
		    keys list "a list of field names which are keys"
		    system boolean "is this a system view?"
		    lastmod long "time of last modification"
		    flushed long "time last modification was reflected to the db"
		    comment string "a description of this view"
		} "View details"

		display {
		    display string "a collection of display elements"
		    comment string "a description of this display"
		    elements dictionary "list of elements in this display"
		} "Display of fields"

		element {
		    display link "name of display in which this element appears"
		    name string "name of field referenced by element"
		    field link "field index"
		    order integer "order of field within display"
		} "Element of display"
	    } {
		puts stderr "$v: $det"
		reflect $v
		vview set [vview find view $v] comment $comment system 1
		foreach {name type comment} $det {
		    puts stderr "$v.$name $type '$comment'"
		    vfield set [vfield find view $v name $name] type $type comment $comment
		}
	    }
	}

	foreach {view find value} {
	    field {view view name view} {key 1 unique 1}
	    field {view view name fields} {hidden 1}
	    field {view view name keys} {hidden 1}
	    field {view view name lastmod} {hidden 1}
	    field {view view name flushed} {hidden 1}
	    field {view field name name} {key 1}
	    field {view field name view} {key 1}
	    field {view display name display} {key 1 unique 1}
	    field {view element name display} {key 1}
	    field {view element name field} {key 1}
	    field {view element name name} {hidden 1}
	    field {view display name elements} {hidden 1}
	} {
	    setfield $view $find $value
	}

	set vlist [mk::file views db]
	puts stderr "views: $vlist"

	variable views
	foreach v $vlist {
	    if {[info commands ::Dub::v$v] eq {}} {
		mkview $v
		reflect $v
	    }
	}

	#puts stderr "views: [mk::file views db]"
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
