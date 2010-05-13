0 {
    name Root
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
	# evaluate tuple of "Tcl Script" as the result of its tcl evaluation
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

javascript+html {
    type Conversion
    mime "Tcl Script"
    content {
	return [Http Ok $r "<script> /* <!\[CDATA\[ */\n[dict get $r -content]\n/* \]\]> */ </script>]" tuple/html]
    }
}

ref+html {
    type Conversion
    mime "Tcl Script"
    content {
	set content [dict get $r -content]
	set mime [dict get $r -tuple mime]
	set id [dict get $r -tuple id]
	switch -glob -- $mime {
	    text/css {
		set content [<stylesheet> {*}$content]
	    }
	    */javascript {
		set content [<script> type text/javascript src {*}$content {}]
	    }
	    image/* {
		set content [<img> id $id src {*}$content]
	    }
	    transclude/* {
		set content [<a> id $id class transclude href {*}$content]
	    }
	    default {
		set content [<a> id $id href {*}$content]
	    }
	}
	return [Http Ok $r $content tuple/html]
    }
}

css+html {
    type Conversion
    mime "Tcl Script"
    content {
	return [Http Ok $r [<style> type text/css [dict get $r -content]] tuple/html]
    }
}

"css ref+html" {
    type Conversion
    mime "Tcl Script"
    content {
	return [Http Ok $r [<stylesheet> {*}[dict get $r -content]] tuple/html]
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

"Tcl Dict+html" {
    type Conversion
    mime "Tcl Script"
    content {
	Debug.tupler {dict conversion: [llength $r] ($r)}
	set result {}
	set content [dict get $r -content]
	if {[llength $content] == 1} {
	    set content [lindex $content 0]
	}
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
    type "Tcl Script"
    content {
	[<h1> "Welcome to Tuple"]
	[Html ulinks {
	    "Test composition and Tcl Scripting" now
	    "Test Tcl Variable and Tcl Dict rendering" reflect
	    "Test Uppercase and text/plain" {Example+Uppercase}
	    "Test page not found" nothere
	}]
    }
}

now {
    type "Tcl Script"
    content {
	[<h1> Now]
	[<p> [clock format [clock seconds]]]
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

dict_err {
    type "Tcl Dict"
    content {this is not a properly formed dict}
}

"Not Found" {
    type "Tcl Script"
    content {
	[<title> [string totitle "$kind error"]]
	[<h1> [string totitle "$kind error"]]
	[<p> "'$name' not found while looking for '$extra'"]
	[<p> "(Generated from User Content)"]
    }
}
