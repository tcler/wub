Root {
    type Root
}

Type {
    type Type
}

Basic {
    type Type
}

Basic+Html {
    type Conversion
    content {
	# at the moment, we'll assume Basic is Html
	return [Http Ok $r [dict get $r -content] tuple/html]
    }
}

"Tcl Script" {
    type Type
    mime "Tcl Script"
    content {
	# evaluate tuple of Template as the result of its tcl evaluation
	Debug.tupler {tcl script preprocessing ([dict get $r -content])}
	set content [eval [dict get $r -content]]
	
	# determine the mime type of result
	if {[dict exists $r -mime]} {
	    set mime [string map {_ /} [dict get? $r -mime]]
	} else {
	    set mime [my getmime [dict get? $r -tuple]]
	}
	
	Debug.tupler {tcl script to '$mime' mime type content:($result)}
	return [Http Ok $r $result $mime]
    }
}

Template {
    type Type
    mime "Tcl Script"
    content {
	# evaluate tuple of Template as the result of its tcl evaluation
	Debug.tupler {tcl script preprocessing ([dict get $r -content])}
	set result [subst [dict get $r -content]]

	# determine the mime type of result
	if {[dict exists $r -mime]} {
	    set mime [string map {_ /} [dict get? $r -mime]]
	} else {
	    set mime [my getmime [dict get? $r -tuple]]
	}

	Debug.tupler {tcl script to '$mime' mime type content:($result)}
	return [Http Ok $r $result $mime]
    }
}

*rform+style {
    type css
    content {
	* {zoom: 1.0;}

	input.blur {
	    color:lightgray;
	}
	img.icon {
	    border:0px;
	    width:25px
	}

	div.nav {
	    float:right;
	    background: whitesmoke;
	    padding: 0.3em 0.7em;
	    -moz-border-radius-topleft:5px;
	    -moz-border-radius-topright:5px;
	    -moz-border-radius-bottomleft:5px;
	    -moz-border-radius-bottomright:5px;
	}
	h1.pretty, h2.pretty, h3.pretty, h4.pretty, h5.pretty, h6.pretty {
	    background: darkslategray;
	    color: whitesmoke;
	    padding: 0.2em 0.5em;
	    -moz-border-radius-topleft:7px;
	    -moz-border-radius-topright:7px;
	    -moz-border-radius-bottomleft:7px;
	    -moz-border-radius-bottomright:7px;
	}
	table.pretty {
	    margin: 1em 1em 1em 2em;
	    background: whitesmoke;
	    border-collapse: collapse;
	}
	table.pretty td {
	    border: 1px silver solid;
	    padding: 0.2em;
	}
	table.pretty th {
	    border: 1px silver solid;
	    padding: 0.2em;
	    background: darkslategray;
	    color: white;
	    text-align: left;
	    -moz-border-radius-topleft:7px;
	    -moz-border-radius-topright:7px;
	    -moz-border-radius-bottomleft:7px;
	    -moz-border-radius-bottomright:7px;
	}
	table.pretty tr.family {
	    background: gainsboro;
	}
	table.pretty caption {
	    margin-left: inherit;
	    margin-right: inherit;
	    font-size: 150%;
	}

	fieldset {
	    background: whitesmoke;

	    -moz-border-radius-topleft:7px;
	    -moz-border-radius-topright:7px;
	    -moz-border-radius-bottomleft:7px;
	    -moz-border-radius-bottomright:7px;
	}

	fieldset > legend {
	    background: darkslategray;
	    color: white;
	    -moz-border-radius-topleft:5px;
	    -moz-border-radius-topright:5px;
	    -moz-border-radius-bottomleft:5px;
	    -moz-border-radius-bottomright:5px;
	}

	.button {
	    border: 1px solid #aaa;
	    -webkit-border-radius: 5px;
	    -moz-border-radius: 5px;
	    padding: 2px 5px;
	    margin: 0 3px;
	    cursor: pointer;
	    background: gainsboro;
	}
	.changed {
	    background-color: gainsboro;
	}
    }
}

*rform+edit {
    type "Tcl Script"
    content {
	::set T [my fetch [dict get $r -tuple _left]]
	Debug.tupler {*rform+edit: ($T)}
	dict with T {
	    set content [::textutil::undent [::textutil::untabify $content]]
	    set result [subst {
		[<title> [string totitle "Editing $name"]]
		[<form> Edit_$id class autoform action save/ {
		    [<fieldset> Details_$id title $name {
			[<legend> $name]
			[<text> type label "Type:" [string totitle $type]][<br>]
			[<textarea> content class autogrow style {width:99%} [string trim $content]]
			[<hidden> id $id]
			[<submit> submit]
		    }]
		    [<div> id result {}]
		}]
	    }]
	}
	set result
    }
}

*rform+edit+jQ {
    type text
    content {
	form .autoform target '#result'
	autogrow .autogrow
    }
}

"Not Found" {
    type Template
    content {
	[<title> [string totitle "$kind error"]]
	[<h1> [string totitle "$kind error"]]
	[<p> "'$nfname' not found while looking for '$extra'"]
	[<p> "(Generated from [<a> href "xray/Not Found" "Not Found"] page)"]
    }
}

Glob {
    type Type
    mime "Tcl Script"
    content {
	# search tuples for name matching glob, return a List
	set tuple [dict get $r -tuple]
	Debug.tupler {GLOB conversion: [dict size $r] ($tuple)}
	dict with tuple {
	    if {$mime ne "text"} {
		set search [my tuConvert $tuple tuple/text tuple/$mime]
	    } else {
		set search [dict get $r -content]
	    }
	}
	set search [string trim $search]
	set result [my globByName $search]
	
	Debug.tupler {GLOB search: '$search' -> ($result)}
	return [Http Ok $r $result tuple/list]
    }
}

Named+List {
    type Conversion
    mime "Tcl Script"
    content {
	# search tuples for name matching regexp, return a List
	set tuple [dict get $r -tuple]
	dict with tuple {
	    if {$mime ni {"basic text"}} {
		set search [my tuConvert $tuple tuple/text]
	    } else {
		set search [dict get $r -content]
	    }
	}
	set search [string trim $search]
	set result [my regexpByName $search]
	return [Http Ok $r $result tuple/list]
    }
}

Javascript+Html {
    type Conversion
    mime "Tcl Script"
    content {
	return [Http Ok $r [<script> [dict get $r -content]]] tuple/html]
    }
}

Javascript+Head {
    type Conversion
    mime "Tcl Script"
    content {
	return [Http Ok $r [<script> [dict get $r -content]]] tuple/head]
    }
}

CSS+Html {
    type Conversion
    mime "Tcl Script"
    content {
	set c [dict get $r -content]
	return [Http Ok $r [<pre> "&lt;style&gt;\n$c\n&lt;/style&gt;"] tuple/html]
    }
}

CSS+Head {
    type Conversion
    mime "Tcl Script"
    content {
	set c [dict get $r -content]
	return [Http Ok $r [<style> type text/css $c] tuple/head]
    }
}

ref+html {
    type Conversion
    mime "Tcl Script"
    content {
	set content [dict get $r -content]
	set mime [dict get $r -tuple mime]
	set id [dict get $r -tuple id]

	set c [my tuConvert [dict get $r -content] tuple/text]

	# each ref determines its referenced content's type
	switch -glob -- $mime {
	    css -
	    text/css {
		set content [<stylesheet> {*}$content]
		# should this be in an html body?
	    }

	    javascript -
	    */javascript {
		set content [<script> type text/javascript src {*}$content {}]
	    }

	    transclude/* {
		set content [<div> id T_$id class transclude href {*}$content]
	    }

	    image/* {
		set content [<img> id T_$id src {*}$content]
	    }

	    default {
		set content [<a> id T_$id href {*}$content]
	    }
	}
	return [Http Ok $r $content tuple/html]
    }
}

ref+head {
    type Conversion
    mime "Tcl Script"
    content {
	set content [dict get $r -content]
	set mime [dict get $r -tuple mime]
	set id [dict get $r -tuple id]

	switch -glob -- $mime {
	    css -
	    text/css {
		set content [<stylesheet> {*}$content]
	    }

	    javascript -
	    */javascript {
		set content [<script> type text/javascript src {*}$content {}]
	    }

	    default {
		return -code error -kind type -notfound $id "ref of type $mime has no rendering as Head"
	    }
	}

	return [Http Ok $r $content tuple/head]
    }
}

Dict {
    type Type
    content {
	# do some form checking on the dict
	if {[catch {dict size [dict get? $r -content]} e eo]} {
	    return [Http Ok $r [subst {
		[<h1> "Type error"]
		[<p> "'[armour [dict get $r -tuple name]]' is of Type 'Dict', however its content is not a properly-formed dictionary."]
		[<p> "Dictionaries are tcl lists with an even number of elements."]
		[<h2> Content:]
		[<pre> [armour [dict get? $r -content]]]
	    }] tuple/html]
	} else {
	    return [Http Pass $r]
	}
    }
}

List+Dict {
    type Conversion
    mime "Tcl Script"
    content {
	Debug.tupler {list conversion: [dict size $r] ($r)}
	# make a list into a Dict by making tuple name the key
	set result {}
	foreach v [dict get $r -content] {
	    set v [my fetch $v]
	    dict set result [dict get $v name] [dict set $v id]
	}
	return [Http Ok $r [join $result \n] tuple/dict]
    }
}

List+Html {
    type Conversion
    mime "Tcl Script"
    content {
	Debug.tupler {List to Html conversion: ([dict get $r -content])}
	set result ""
	foreach v [dict get $r -content] {
	    set v [my fetch $v]
	    set c [my tuConvert $v tuple/html]
	    Debug.tupler {List to Html: converted $v to ($c)}
	    append result [<li> id T_[dict get $v id] $c] \n
	}
	if {$result ne ""} {
	    set result [<ul> \n$result]\n
	}
	return [Http Ok $r $result tuple/html]
    }
}

Dict+Head {
    type Conversion
    mime "Tcl Script"
    content {
	Debug.tupler {Dict to Head conversion: [dict size $r] ($r)}
	set result {}
	dict for {n v} [dict get $r -content] {
	    set v [my fetch $v]
	    if {[dict get $v type] eq "ref"} {
		# rename refs so their dict-name is their reference
		set n [lindex [dict get $v content] 0]
	    }
	    if {[info exists $result $n]} continue
	    dict set result $n [my tuConvert $v tuple/head]
	}
	#Debug.tupler {dict conversion: ([dict get $r -content]) -> ($result)}
	dict set r -tuple mime "Tcl Dict"
	return [Http Pass $r $result tuple/head]
    }
}

Dict+Html {
    type Conversion
    mime "Tcl Script"
    content {
	Debug.tupler {Dict to Html conversion: [dict size $r] ($r)}
	# we prefer a tabular form, but could use dl instead
	set result {}
	set content 
	dict for {n v} [dict get $r -content] {
	    if {[dict exists $result $n]} continue
	    set v [my fetch $v]
	    set sub [my tuConvert $v tuple/html]
	    lappend result [<tr> "[<th> [armour $n]] [<td> [armour $sub]]"]
	}
	set result [<table> class sortable border 2 [join $result \n]]
	#Debug.tupler {dict conversion: ([dict get $r -content]) -> ($result)}
	return [Http Pass $r $result tuple/html]
    }
}

"Tcl Variable" {
    type Type
    mime "Tcl Script"
    content {
	# evaluate tuple of "Tcl Script" as the result variable resolution
	set mime [my getmime [dict get? $r -tuple]]

	set result [set [dict get $r -content]]
	Debug.tupler {Tcl Variable '$result' of type '$mime'}
	return [Http Ok $r $result $mime]
    }
}

"Tcl Dict" {
    type Type
    content {
	# do some form checking on the dict
	if {[catch {dict size [dict get? $r -content]} e eo]} {
	    return [Http Ok $r [subst {
		[<h1> "Type error"]
		[<p> "'[armour [dict get $r -tuple name]]' is of Type 'Tcl Dict', however its content is not a properly-formed dictionary."]
		[<p> "Dictionaries are tcl lists with an even number of elements."]
		[<h2> Content:]
		[<pre> [armour [dict get? $r -content]]]
	    }] tuple/html]
	} else {
	    return [Http Pass $r]
	}
    }
}

"Tcl Dict+Head" {
    type Conversion
    mime "Tcl Script"
    content {
	Debug.tupler {Tcl Dict to Head conversion: [dict size $r] ($r)}
	set result {}
	set content [dict get $r -content]
	dict for {n v} $content {
	    lappend result [<tr> "[<th> [armour $n]] [<td> [armour $v]]"]
	}
	set result [<table> class sortable border 2 [join $result \n]]
	#Debug.tupler {dict conversion: ([dict get $r -content]) -> ($result)}
	return [Http Pass $r $result tuple/html]
    }
}

"Tcl Dict+Html" {
    type Conversion
    mime "Tcl Script"
    content {
	Debug.tupler {Tcl Dict to Html conversion: [dict size $r] ($r)}
	# we prefer a tabular form, but could use dl instead
	set result {}
	set content [dict get $r -content]
	dict for {n v} $content {
	    lappend result [<tr> "[<th> [armour $n]] [<td> [armour $v]]"]
	}
	set result [<table> class sortable border 2 [join $result \n]]
	#Debug.tupler {dict conversion: ([dict get $r -content]) -> ($result)}
	return [Http Pass $r $result tuple/html]
    }
}

Text {
    type Type
}

Text+Html {
    type Conversion
    mime "Tcl Script"
    content {
	return [Http Ok $r [<pre> [dict get $r -content]] tuple/html]
    }
}

"example text" {
    type Text
    content "this is text/plain"
}

Uppercase+Text {
    type Conversion
    content {
	return [Http Pass $r [dict get $r -content] tuple/text]
    }
}

Uppercase {
    type Type
    content {
	return [Http Pass $r [string toupper [dict get $r -content]]]
    }
}

"Example Uppercase" {
    type Uppercase
    content "this is uppercase"
}

welcome {
    type Template
    content {
	[<h1> "Welcome to Tuple"]
	[Html ulinks {
	    "Test composition and Tcl Scripting" now
	    "Test Tcl Variable and Tcl Dict rendering" reflect
	    "Test Uppercase and text/plain" {Example+Uppercase}
	    "Test page not found" nothere
	    "XRay of Now page" xray/now
	    "Glob Test" {glob+test}
	}]
    }
}

now {
    type Template
    content {
	[<h1> Now]
	[<p> "[clock format [clock seconds]] is the time"]
	[<p> "This page is generated from a Tcl Script, and assembled from components for [<a> href xray/now+style style] (which makes the header red) and [<a> href xray/now+title title] (which gives the page a title.)"]
	[<p> "The tuple underlying this may be viewed with the [<a> href xray/now "xray facility"]."]

	[<p> "Next step - Creole and Transclusion"]
    }
}

now+title {
    type Text
    content "A Demo Title"
}

now+style {
    type css
    content {
	h1 {color:red;}
    }
}

reflect {
    type "Tcl Variable"
    mime "Tcl Dict"
    content r
}

"Reflect Text" {
    type "Tcl Variable"
    mime Text
    content r
}

"Dict err" {
    type "Tcl Dict"
    content {this is not a properly formed dict}
}

"Glob Test" {
    type Glob
    mime text
    content {
	now+*
    }
}
