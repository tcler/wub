# Dub - Db toy

package provide Dub 1.0

package require Form
package require View
package require Cookies
package require jQ

package require Debug
Debug off dub 10

set API(Experimental/Dub) {
    {A metakit database toy.}
    db {metakit database to use}
}

namespace eval Dub {
    variable db /tmp/dub.db
    variable toplevel
    variable mount /dub

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
	    [<p> "Displays are collections of fields organised for screen presentation."]
	    [<p> "Create a new display by entering its name, or edit an existing display by selecting it from the list in the left margin"]
	}]

	set r [Html style $r css/form.css]

	return [Http NoCache [Http Ok $r $content style/dub]]
    }

    # views page
    proc /views {r args} {
	set content [subst {
	    [<h1> "Dub Views - A Wub Toy"]
	    [<p> "A view is a collection of typed fields which are stored in a database."]
	    [<p> "Dub maintains metadata for each field, allowing the simple construction of editing and reporting programs."]
	    [<p> "You can create a new view by entering its name, or edit an existing view by selecting it from the left margin"]
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
	    Debug.dub {reflect: $view $field of type '$type'}
	    if {[catch {
		vfield find view $view name $field
	    } index eo]} {
		#Debug.dub {reflect adding: $view.$field of type [2native $type]}
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
	    Debug.dub {mkview: making $view of '$layout'}
	    mk::view layout db.$view $layout
	    Debug.dub {mkview: made $view of '$layout'}
	}

	if {[catch {View init v$view db.$view} r eo]} {
	    Debug.dub {View: $r ($eo)}
	} else {
	    Debug.dub {cmd: $r - v$view - [info commands v*]}
	}

	variable views
	set views($view) v$view
	Debug.dub {mkview: [mk::file views db]}
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
	Debug.dub {/fieldE: $view.$name of $type key:$key}
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
	    Debug.dub {/fieldE new $view.$name}
	    vfield append name $name view $view type $type comment $comment unique $unique key $key hidden $hidden
	} else {
	    Debug.dub {/fieldE edit $view.$name: $index}
	    vfield set $index type $type comment $comment unique $unique key $key hidden $hidden
	}

	mk::file commit db
	return [/view $r $view]
    }

    variable page {}
    proc init_page {} {
	variable page
	variable mount
	dict set page globlinks [subst {
	    Home /
	    Dub ${mount}
	}]
	dict set page global [<div> [<form> search action index.html {
	    [<text> q size 15 maxlength 250]
	    [<image> submit src search.png alt Search]
	}]]
	dict set page header [<h1> "Wub[<span> class fade Dub]"]
	dict set page sitelinks [subst {
	    Home /
	    Dub ${mount}
	    Views ${mount}views
	    Displays ${mount}displays
	    Export ${mount}export
	}]
	dict set page breadcrumbs {}
	
	dict set page navbox {}
	dict set page copyright {}
	dict set page footlinks {}
	dict set page footer ""
    }

    # sidebar containing displays
    proc displays-sidebar {contents args} {
	set args [list url {display?display=$display} {*}$args]
	set url [dict get $args url]
	dict unset args url

	if {[dict exists $args target]} {
	    set target [list target [dict get $args target]]
	    dict unset args target
	} else {
	    set target {}
	}

	set displays [dict values [vdisplay with {
	    <li> [<a> href [subst $url] title $comment {*}$target $display]
	}]]
	if {$displays ne {}} {
	    set contents [Sinorca sidebar $contents title "Displays" {*}$args [<ul> [join $displays \n]]]
	}

	return $contents
    }

    # sidebar containing views - divided into User and Dub views
    proc views-sidebar {contents args} {
	set args [list url {view?view=$view} {*}$args]
	set url [dict get $args url]
	dict unset args url

	if {[dict exists $args target]} {
	    set target [list target [dict get $args target]]
	    dict unset args target
	} else {
	    set target {}
	}

	set views {}
	set sysv {}
	foreach val [dict values [vview with {
	    list $system [<li> [<a> href [subst $url] title $comment {*}$target $view]]
	}]] {
	    lassign $val system val
	    if {$system} {
		lappend sysv {*}$val
	    } else {
		lappend views {*}$val
	    }
	}

	if {$views ne {}} {
	    set contents [Sinorca sidebar $contents title "User Views" {*}$args [<ul> title "User-defined views" [join $views \n]]]
	}
	if {$sysv ne {}} {
	    set contents [Sinorca sidebar $contents title "Dub Views" {*}$args [<ul> title "Dub-defined views for storing metadata" class collapse [join $sysv \n]]]
	}

	return $contents
    }

    proc dub-sidebar {contents} {
	return $contents
    }

    # javascript to hide sidebar ULs
    variable hidejs [<ready> {
	function hider (what) {
	    $(what).next().children("li").hide();
	    $(what).click(function() {
		shower($(this));
	    });
	};

	function shower (what) {
	    $(what).next().children("li").show();
	    $(what).click(function() {
		hider($(this));
	    });
	};

	$('.title').click(function() {
	    hider($(this));
	});
	
	$('.collapse').prev('.title').click(function() {
	    shower($(this));
	});
	$('.collapse').children("li").hide();
    }]

    # convert dub to sinorca styles
    proc .style/dub.style/sinorca {rsp} {
	variable page
	set contents $page

	set content [dict get $rsp -content]

	# add some js to hide sidebar uls
	variable hidejs; append content $hidejs \n

	dict set contents content $content

	if {[dict exists $rsp -sidebar]} {
	    set contents [Sinorca sidebar $contents {*}[dict get $rsp -sidebar]]
	}

	switch -- [file tail [dict get $rsp -path]] {
	    views {
		set contents [Sinorca sidebar $contents title "New View" [<form> view class dubform action ./view {[<text> view title "Create a new view by entering its name here" size 16 ""]}]]
		set contents [views-sidebar $contents]
	    }

	    displays {
		set contents [Sinorca sidebar $contents title "New Display" [<form> view class dubform action ./display {[<text> display title "Create a new display by entering its name here" size 16 ""]}]]
		set contents [displays-sidebar $contents]
	    }

	    display {
		set contents [displays-sidebar $contents src "\#" target dispFrame url {displayframe?display=$display}]
		set contents [views-sidebar $contents src "\#" target detail url {viewframe?view=$view}]
		set contents [Sinorca sidebar $contents title "New Display" [<form> view class dubform action ./display {[<text> display title "Create a new display by entering its name here" size 16 ""]}]]
	    }

	    fieldE -
	    view {
		set contents [Sinorca sidebar $contents title "New View" [<form> view class dubform action ./view {[<text> view title "Create a new view by entering its name here" size 16 ""]}]]
	    }
	    default {
		set contents [dub-sidebar $contents]
	    }
	}

	dict set rsp -content $contents
	set rsp [jQ script $rsp jquery.js]
	dict lappend rsp -headers [<stylesheet> /sinorca/screen.css]
	return $rsp
    }

    variable dispstyle [<style> [subst {
	.field {
	    float: left;
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
	    background-color: #68BFEF;
	    border: 2px solid #0090DF;
	    padding: 0.125em;	
	    text-align: center;
	    margin: 0.5em;
	    cursor: pointer;
	    z-index: 100;
	}
	.display {
	    background-color: yellow;
	}
	.dst {
	    background-color: #c8c8c8;
	    padding: 5px;
	    text-align: center;
	    overflow: auto;
	    opacity: 0.7;
	}
	.src {
	    background-color: #c8c8c8;
	    padding: 5px;
	    text-align: center;
	    overflow: auto;
	    opacity: 0.7;
	}
    }]]

    # javascript to swap fields and elements
    variable swapjs [<ready> {
	function swap (what, from, to, whence, where) {
	    var x = $(document.createElement('div'));
	    x.attr("class", $(what).attr("class"));
	    x.addClass(to); x.removeClass(from);
	    
	    x.attr("title", $(what).attr("title"));
	    x.html($(what).html());

	    x.click(function() {
		swap($(this), to, from, where, whence);
	    });

	    $(where).append(x);
	    $(what).remove();
	};

	$('.field').click(function() {
	    swap($(this), 'field', 'el', '.src', '.dst');
	});

	$('.el').click(function() {
	    swap($(this), 'el', 'field', '.dst', '.src');
	});
    }]

    # javascript to select subviews
    variable seljs [<ready> {
	function selD (name) {
	    if (name == "") return;
	    var x = $(document.createElement('div'));
	    x.addClass("el");
	    x.addClass("display");
	    x.attr("title", "subview");
	    
	    var html = "<input type='text' value='"+ name + "' name='view' style='border: 0pt none; text-align: center; cursor: pointer;' readonly='1' size='8'/>";
	    html += "<input type='hidden' value='' name='field'/>";
	    
	    x.html(html);

	    $('.dst').append(x);
	};

	$('.selD').change(function() {
	    selD(this.options[this.selectedIndex].value);
	});
    }]

    proc viewframe {view} {
	return [<form> view title "click a field to add it to the display" {
	    [<fieldset> viewFS {
		[<legend> "View: $view"]
		[join [dict values [vfield with {
		    <div> class field title $comment "[<text> view size 8 readonly 1 style {border:0;text-align:center; cursor:pointer;} $view]<br>[<text> field size 8 readonly 1 style {border:0;text-align:center;cursor:pointer;} $name]"
		} view $view]] \n]
	    }]
	}]
    }

    proc /viewframe {r view} {
	variable swapjs; append content $swapjs \n
	append content [viewframe $view]

	#set r [jQ jframe $r]
	return [Http NoCache [Http Ok $r $content x-text/html-fragment]]
    }

    proc getElements {dn} {
	if {[catch {
	    vdisplay find display $dn
	} index eo]} {
	    return {}
	} else {
	    # fill body of display with elements' names
	    return [join [dict values [velement with {
		If {$sub == 0} {
		    [set f [vfield get $field]
		     <div> class el title $comment "[<text> view size 8 readonly 1 style {border:0;text-align:center;cursor:pointer;} [dict get $f view]]<br>[<text> field size 8 readonly 1 style {border:0;text-align:center;cursor:pointer;} [dict get $f name]]"
		    ]
		} else {
		    [<div> class el class display title $comment "[<text> view size 8 readonly 1 style {border:0;text-align:center;cursor:pointer;} [vdisplay get $field display]][<hidden> field {}]"]
		}
	    } display $dn -sort order]] \n]
	}
    }

    proc displayframe {name} {
	set values [join [dict values [vdisplay with {
	    If {$display ne $name} {
		[<option> value $display label $display $display]
	    }
	}]] \n]

	if {$values ne ""} {
	    set select [<select> subs label "Add:" id subs class selD title "Select an existing display for inclusion in '$name'" {
		<OPTION selected value=''></OPTION>
		$values
	    }]
	} else {
	    set select ""
	}

	return [<form> display action ./displayE {
	    [<hidden> display $name]
	    [<submit> save jframe no title "Save Changes to '$name'" "Save"]
	    $select
	    [<br>]
	    [<div> class dst title "click an element to remove it from '$name'" [getElements $name]]
	}]
    }

    proc /displayframe {r display} {
	append content [displayframe $display]
	variable swapjs; append content $swapjs \n
	variable seljs; append content $seljs \n

	#set r [jQ jframe $r]

	set r [Http Ok $r $content x-text/html-fragment]
	return [Http NoCache $r]
    }

    # remove elements with fields not found in args
    proc keepElements {di args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	Debug.dub {keepElements [dict keys $args] from $di}
	set delset {}
	velement with {
	    Debug.dub {keepElements del $field ?}
	    if {[dict exists $args $field]} {
		if {$sub} {
		    # it's a sub-display
		    set v ""
		} else {
		    # it's a field element
		    set v [vfield get $field view]
		}
		if {$v ni [dict get $args $field]} {
		    lappend delset ${}
		}
	    } else {
		lappend delset ${}
	    }
	} display $di
	Debug.dub {keepElements except $delset}
	foreach el $delset {
	    velement set $el display ""
	}
    }

    proc /displayE {r display view field} {
	set result [Http Redir $r "./display?display=$display"]

	Debug.dub {/displayE $display: ($view) ($field) [Http Referer $r]}
	# get display record
	if {[catch {
	    vdisplay find display $display
	} di eo]} {
	    set di [vdisplay append display $display]
	} else {
	    # found named display 
	}

	# write all display elements to file in order
	set counter 0
	set fieldset {}
	foreach v $view f $field {
	    Debug.dub {/displayE PROCESS: view: '$v' field: '$f'}
	    if {$f ne ""} {
		# normal field element
		set fi [vfield find name $f view $v]
		set comment [vfield get $fi comment]
		dict lappend fieldset $fi $v
		Debug.dub {displayE EL: di: $di field:$f view:$v -> fi:$fi}
		if {[catch {
		    velement find display $display field $fi sub 0
		} ei eo]} {
		    if {[catch {
			velement find display ""
		    } empty eo]} {
			# add a new element
			set ei [velement append display $display field $fi name $v.$fi order [incr counter] comment $comment sub 0]
		    } else {
			# re-use an empty element
			set ei [velement set $empty display $display field $fi name $v.$fi order [incr counter] comment $comment sub 0]
		    }
		} else {
		    # set the order of display of this element
		    velement set $ei order [incr counter]
		}
	    } else {
		# sub-display element
		set si [vdisplay find display $v]
		set comment [vdisplay get $si comment]
		dict lappend fieldset $si ""
		Debug.dub {displayE SUB: di: $display field:$f view:$v -> si:$si}
		if {[catch {
		    velement find display $display field $si sub 1
		} ei eo]} {
		    if {[catch {
			velement find display ""
		    } empty eo]} {
			# add a new element
			set ei [velement append display $display field $si name "display $v" order [incr counter] comment $comment sub 1]
		    } else {
			# re-use an empty element
			set ei [velement set $empty display $display field $si name "display $v" order [incr counter] comment $comment sub 1]
		    }
		} else {
		    # set the order of display of this element
		    velement set $ei order [incr counter]
		}
	    }
	}
	keepElements $display $fieldset
	mk::file commit db
	return $result
    }

    proc /display {r args} {
	# set up 'display' frame
	set display [dict get? $args display]
	Debug.dub {/display $display ($args)}
	if {$display ne ""} {
	    append content [<div> id dispFrame src displayframe?display=$display {}]
	} else {
	    append content [<div> id dispFrame {}]
	}

	# set up 'view' frame
	set view [dict get? $args view]
	if {$view ne ""} {
	    append content [<div> id detail class src src viewframe?view=$view {}]
	} else {
	    append content [<div> id detail class src {}]
	}

	# load jframe javascript
	set r [jQ jframe $r]
	append content [subst {
	    [<ready> [string map [list %M {
		<b>loading...</b>
	    }] {
		jQuery.fn.waitingJFrame = function () {
		    /*$(this).html("%M");*/
		}
	    }]]
	}]

	variable dispstyle; lappend r -headers [list $dispstyle]

	set r [jQ jframe $r]
	variable swapjs; append content $swapjs \n

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
	    Debug.dub {/fieldD '$view.$name' err: $err ($eo)}
	    set F [list type "" name $name view $view key 0 unique 0 comment ""]
	}
	
	Debug.dub {/fieldD Record: [array get F]}

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

    proc /export {r} {
	set fields {}
	set views [dict values [vview with {
	    lappend fields {*}[dict values [vfield with {
		list Field $view $name type $type key $key unique $unique hidden $hidden comment $comment
	    } view $view]]
	    list View $view system $system lastmod $lastmod comment $comment
	} system 0]]

	set elements {}
	set displays [dict values [vdisplay with {
	    lappend elements {*}[dict values [velement with {
		list Element $display [expr {$sub?[vdisplay get $field display]:[join [vfield get $field view name] .]}]
	    } display $display]]
	    list Display $display comment $comment
	}]]

	return [Http NoCache [Http Ok $r "[join $views \n]\n[join $fields \n]\n[join $displays \n]\n[join $elements \n]\n" text/plain]]
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
		#puts "DER: [uplevel namespace current] [procText $proc]"
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
	Debug.dub {setfield: view '$view' keys '$find' = '$args'}
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
	    <tr> [<td> [<a> href fieldD?view=$view&name=$name title $comment {*}$target $name]]
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

	Debug.dub {/view $view: start}
	set ddisplay {}
	if {0} {
	    if {[catch {vdisplay find display $view}]} {
		lappend ddisplay [<li> [<a> href display?display=$view&view=$view title "Create a default display for $view" "Create Display"]]
	    } else {
		lappend ddisplay [<li> [<a> href display?display=$view&view=$view title "Edit default display of $view" "Default Display"]]
	    }
	}
	dict set r -sidebar [list title "View $view" [<ul> [join $ddisplay \n]]]

	if {[catch {
	    vfield find view $view
	    Debug.dub {/view existing $view}
	} err eo]} {
	    Debug.dub {/view new '$view': $err ($eo)}
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

    variable interp ""
    proc import {text} {
	variable interp
	if {$interp eq ""} {
	    set interp [interp create -safe -- importer]
	    $interp eval {
		variable order 1
		proc View {view args} {
		    return "vview view $view $args"
		}
		proc Field {view field args} {
		    return "vfield append view $view name $field $args"
		}
		proc Display {display args} {
		    return "velement append display $display $args"
		}
		proc Element {display name args} {
		    variable order
		    lassign [split $name .] view field
		    if {$field eq ""} {
			return [string map [list %O [incr order] %D $display %S $field %A $args] {velement append %A display %D sub 1 order %O field [vdisplay find display %S]}]
		    } else {
			return [string map [list %O [incr order] %D $display %V $view %F $field %A $args] {velement append %A display %D sub 0 order %O field [vfield find view %V name %F]}]
		    }
		}
		rename proc {}
		rename eval {}
	    }
	}

	Debug.dub {EVALUATE: $text}
	set text [string map [list \[ "" \] "" \$ "" \; ""] $text]
	set text [split $text \n]
	set text "\[[join $text {]\n[}]\]"
	catch {
	    $interp eval subst [list $text]
	} result eo
	Debug.dub {IMPORT: '$result' ($eo)}
	return $result
    }

    proc /import {r {text ""}} {
	if {$text ne ""} {
	    set result [import $text]
	    return [Http RedirectReferer $r]
	}
	return [Http NoCache [Http Ok $r [<form> importF action import method POST {
	    [<textarea> text cols 80 rows 10 ""]
	    [<br>]
	    [<submit> import]
	}] style/dub]]
    }

    proc _init {} {
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
	    comment:S
	}
	mkview element {
	    display:S
	    sub:I
	    field:I
	    order:I
	    name:S
	    comment:S
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
	    } "Display of fields"
	    
	    element {
		display link "name of display in which this element appears"
		name string "name of field referenced by element (purely informational)"
		sub boolean "Is this element a sub-display?"
		field link "field index"
		order integer "order of field within display"
		comment string "a description of this element"
	    } "Element of display"
	} {
	    Debug.dub {$v: $det}
	    reflect $v
	    vview set [vview find view $v] comment $comment system 1
	    foreach {name type comment} $det {
		Debug.dub {$v.$name $type '$comment'}
		vfield set [vfield find view $v name $name] type $type comment $comment
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
	} {
	    setfield $view $find $value
	}
    }

    proc new {args} {
	if {$args ne {}} {
	    variable {*}$args
	}
	init_page
	Convert Namespace ::Dub

	# try to open wiki view
	if {[catch {
	    mk::file views db
	    # if this succeeds, we're a subthread and the db's open
	    variable toplevel 0
	    Debug.dub {DUB views: $vlist ($eo)}
	} vlist eo]} {
	    # this is the top level process, need to create a db
	    variable toplevel 1
	    variable db
	    mk::file open db $db -shared
	    set vlist [mk::file views db]
	    Debug.dub {DUB views: $vlist ($eo)}
	}

	# construct base table views
	foreach v {field view} {
	    mkview $v
	}

	if {"field" ni $vlist} {
	    # we need to construct all the Dub views
	    _init
	    set vlist [mk::file views db]
	}

	Debug.dub {views: $vlist}

	variable views
	foreach v $vlist {
	    if {[info commands ::Dub::v$v] eq {}} {
		mkview $v
		reflect $v
	    }
	}
	return ::Dub
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
