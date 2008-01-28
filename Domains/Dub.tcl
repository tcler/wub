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
	    [<h1> "Dub - A Metakit Toy"]
	    [
	     set views [vview with {
		 <a> href view?view=$view $view
	     }]
	     if {$views ne {}} {
		 set views [dict values $views]
		 return [<ul> [<li> [join $views "</li>\n<li>"]]]
	     } else {
		 return {}
	     }
	    ]

	    [<form> view class dubform action view {
		[<legend> "New View"]
		[<fieldset> viewS {
		    [<text> label "View: " view ""]
		}]
	    }]
	}]

	set r [Html style $r css/form.css]

	return [Http NoCache [Http Ok $r $content style/dub]]
    }

    # convert the metakit view to our field/view tables
    proc reflect {view} {
	# reflect view to view table
	if {[catch {
	    vview find view $view
	}]} {
	    vview append view $view lastmod [clock clicks] flush [clock clicks]
	}
	
	# reflect fields to field table
	variable types
	variable 2types
	variable dbtype
	foreach f [split [v$view properties]] {
	    lassign [split $f :] field type
	    if {$type eq ""} {
		set type S
	    }
	    if {[catch {
		vfield find view $view field $field
	    } index eo]} {
		vfield append view $view field $field type [dict get $2types $type]
	    } elseif {$type ne
		      [dict get $types [vfield get $index type] $dbtype]
		  } {
		# got a field record, update type
		vfield set $index type [dict get $2types $type]
	    }
	}
    }
    
    # construct View object for a given view,
    # reflect View's properties to the field table.
    proc mkview {view {layout ""}} {
	if {$layout ne ""} {
	    mk::view layout db.$view $layout
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
	variable types
	variable dbtype
	vfield with {
	    lappend layout "$field:[dict get $types $type $dbtype]"
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
    proc /fieldE {r view field type} {
	variable types
	if {![regexp {^[a-zA-Z][a-zA-Z_0-9]*$} $field]
	    || ![dict exists $types $type]
	} {
	    return [/view $r $view]
	}

	if {[catch {
	    vfield find view $view field $field
	} index eo]} {
	    puts stderr "/fieldE new: $view.$field of $type"
	    vfield append field $field view $view type $type
	} else {
	    puts stderr "/fieldE edit: $view.$field of $type"
	    vfield set $index type $type
	}

	if {[catch {
	    vview find view $view
	} index eo]} {
	    vview append view $view lastmod [clock clicks] flush 0
	} else {
	    vview set $index lastmod [clock clicks]
	}

	return [/view $r $view]
    }

    variable page {}
    dict set page globlinks [subst {
	Home /
	Dub ${prefix}/
    }]
    dict set page global [<div> [<form> search action index.html {
	[<text> q size 15 maxlength 250]
	[<image> submit src search.png alt Search]
    }]]
    dict set page header [<h1> "Wub[<fade> Dub]"]
    dict set page sitelinks [subst {
	Home /
	Dub ${prefix}/
    }]
    dict set page breadcrumbs {}

    dict set page navbox {}
    dict set page copyright {}
    dict set page footlinks {}
    dict set page footer ""

    proc .style/dub.style/sinorca {rsp} {
	variable page
	set contents $page
	dict set contents content [dict get $rsp -content]
	set views {}
	foreach {rec val} [vview with {
	    list $view view?view=$view
	}] {
	    lappend views {*}$val
	}

	dict set rsp -content [Sinorca sidebar $contents title Views [Html ulinks  $views]]

	#puts stderr "DUB STYLE: [dict keys [dict get $rsp -content]]"
	dict lappend rsp -headers [<stylesheet> /sinorca/screen.css]
	return $rsp
    }

    variable dbtype mk

    # field detail form - enable editing of a field's properties
    proc /fieldD {r {view ""} {field ""} {comment ""} {type ""}} {
	if {[catch {
	    vfield get [vfield find field $field view $view]
	} record eo]} {
	    #puts stderr "/fieldD '$view.$field' err: $record ($eo)"
	    set record [list type "" field $field view $view]
	}

	#puts stderr "/fieldD Record: $record"
	variable dbtype
	variable types
	dict with record {}
	set content [<form> fieldD action ./fieldE class dubform {
	    [<fieldset> fieldDS {
		[<legend> "Properties"]
		[<hidden> view $view]
		[<p> [subst {
		    [<text> field class required label "Name: " $field]
		    [<select> type label "Type: " class required {
			[Foreach {v n} $types {
			    [<option> value $v select? $type $v]
			}]
		    }]
		}]]
		[<p> [<textarea> comment compact 1 legend "Comment: " $comment]]
		[<submit> Go Change]
	    }]
	}]
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
    variable types {
	integer {mk I}
	string {mk S}
	long {mk L}
	float {mk F}
	double {mk D}
	binary {mk B}
    }

    # return a table of field names which inject pages into a jframe
    proc /fields {r {view ""} {target ""}} {
	if {$target ne ""} {
	    set target [list target $target]
	}

	set fieldrefs [vfield with {
	    <tr> [<td> [<a> href fieldD?view=$view&field=$field {*}$target $field]]
	} view $view]
	set fieldrefs [dict values $fieldrefs]
	#set content [<ul> [<li> [join $fieldrefs "</li>\n<li>"]]]
	set content [<table> id selector style {margin-top: 1em;} [subst {
	    [<thead> [<tr> [<th> Fields]]]
	    [<tbody> [join $fieldrefs \n]]
	}]]
	return [Http NoCache [Http Ok $r $content x-text/html-fragment]] 
    }

    # display a view's fields, allowing editing
    proc /view {r {view ""}} {
	puts stderr "/view $view: start"
	if {[catch {
	    vfield find view $view
	} err eo]} {
	    puts stderr "/view new $view: $err ($eo)"
	    return [/fieldD $r $view $view]
	}
	puts stderr "/view existing $view: $err ($eo)"

	set content [subst {
	    [<ready> [string map [list %M {
		<b>loading...</b>
	    }] {
		jQuery.fn.waitingJFrame = function () {
		    /*$(this).html("%M");*/
		}
	    }]]
	    [<div> id select style {float:left; margin-left: 5em; border:1;} src fields?view=$view&target=detail {}]
	    [<div> id detail style {float:left; margin-left: 5em; border:1;} src fieldD?view=$view {}]
	}]

	dict lappend r -header [<style> {
	    .error {
		font-size:11px; color:red; 
		padding:1px; margin:3px;
		font-family:"Lucida Grande", Verdana, Arial, sans-serif;
	    }
	}]


	set r [Http NoCache [Http Ok $r $content style/dub]]
	#return [jQ jframe [jQ ingrid [jQ validate $r ""] ""]]
	return [jQ jframe $r]
    }


    proc /create {r {view ""} {names {}} {layouts {}}} {
	if {$view eq "" || [lindex $names 0] eq ""} {
	    set ltype [<select> layouts title "type" {
		[<option> value I integer]
		[<option> value S string]
		[<option> value L long]
		[<option> value F float]
		[<option> value D double]
		[<option> value B binary]
	    }]
	    return [Http NoCache [Http Ok $r [subst {
		[<form> create action create {
		    [<fieldset> "Create A View" {
			[<text> view legend "View Name:" $view]
			[<submit> create "Create View"]
			<table>
			[string repeat "<tr><td>[<text> names]</td><td>${ltype}</td></tr>\n" 10]
			</table>
		    }]
		}]
	    }] style/dub]]
	}

	foreach name $names layout $layouts {
	    if {$name ne ""} {
		if {$layout eq ""} {
		    set layout S
		}
		lappend l "$name:$layout"
	    }
	}

	mkview $view $l

	mk::file commit db
	return [/default $r]
    }
    variable views; array set views {}

    proc /del {r view id} {
	mk::row delete db.$view!$id
	mk::file commit db
	return [view $r $view]
    }

    proc /add {r view {name {}} {value {}}} {
	variable views
	if {![info exists views($view)]} {
	    return [/create $r $view]
	}
	set fields {}
	foreach n $name v $value {
	    lappend fields $n $v
	}
	v$view append {*}$fields

	mk::file commit db

	return [view $r $view]
    }

    proc init {args} {
	if {$args ne {}} {
	    variable {*}$args
	}
	Convert Namespace ::Dub

	# invert type dict
	variable types
	variable 2types
	variable dbtype
	dict for {n v} $types {
	    lappend 2types [dict get $v $dbtype] $n
	}
	puts stderr "2types: $2types"

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
	    mkview view {view:S lastmod:L flush:L comment:S}
	    mkview field {field:S view:S type:S comment:S}

	    reflect field
	    reflect view
	    foreach {v det} {
		field {
		    field "the name of this field"
		    view "the view this field belongs to"
		    type "the type of this field"
		    comment "a description of this field"
		}
		view {
		    view "the name of this view"
		    lastmod "time of last modification"
		    flush "time last modification was reflected to the db"
		    comment "a description of this view"
		}
	    } {
		puts stderr "$v: $det [llength $det]"
		foreach {f comment} $det {
		    puts stderr "$v.$f '$comment'"
		    vfield set [vfield find view $v field $f] comment $comment
		}
	    }
	}

	set vlist [mk::file views db]
	puts stderr "views: $vlist"

	variable views
	foreach v $vlist {
	    mkview $v
	    reflect $v
	}

	#puts stderr "views: [mk::file views db]"
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
