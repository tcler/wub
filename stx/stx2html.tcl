package require stx
package require Form
package provide stx2html 1.1
package require Html

namespace eval stx2html {
    variable features
    array set features {}

    variable toc
    array set toc {}

    variable toccnt {}
    variable tagstart 0	;# number to start tagging TOC sections

    variable title ""	;# title of page

    variable img_properties {}

    proc tagstart {what} {
	variable tagstart $what
    }

    # redefine subst - we only ever want commands in this application
    proc subst {what} {
	Debug.STX {subst: $what}
	return [::subst -nobackslashes -novariables $what]
    }

    proc cdata {args} {
	Debug.STX {cdata: $args}
	return [join $args]
    }

    proc normal {para args} {
	return "[<p> [subst $para]]\n[join $args]"
    }

    proc pre {para args} {
	return "[<pre> $para]\n[join $args]\n"
    }

    proc special {para args} {
	set p [split $para]
	set cmd [string tolower [lindex $p 0]]
	Debug.STX {special: $cmd}
	if {[llength [info commands "_$cmd"]] == 1} {
	    set para [_$cmd [lrange $p 1 end]]
	} else {
	    set para [<p> [::stx::char $para]]
	}
	return "$para\n[join $args]\n"
    }

    proc _title {args} {
	variable title [join $args]
	return ""
    }

    proc _message {args} {
	Debug.STX {STX Message: [join $args]}
	return ""
    }

    proc _comment {args} {
	return "<!-- [join $args] -->\n"
    }

    proc _notoc {args} {
	variable features
	set features(NOTOC) 1
	return ""
    }

    proc _toc {args} {
	variable features
	set features(TOC) 1
	catch {unset features(NOTOC)}
	return "\x81TOC\x82"
    }

    proc header {level para tag args} {
	variable toc
	variable toccnt
	while {[llength $toccnt] <= $level} {
	    lappend toccnt 0
	}
	set toccnt [lrange $toccnt 0 $level]

	lset toccnt $level [expr [lindex $toccnt $level] + 1]
	set toc([join [lrange $toccnt 1 $level] .]) [list $para $tag]

	set p [subst $para]
	variable title
	if {$title eq ""} {
	    set title $p
	}

	return "[<h$level> id $tag $p]\n[join $args]\n"
    }

    proc hr {} {
	return [<hr> style {clear:both;}]
    }

    proc indent {para} {
	return [<p> [subst $para]]
    }

    proc table {args} {
	return [<table> [join $args \n]]\n
    }

    proc row {args} {
	set els {}
	foreach el $args {
	    lappend els [subst $el]
	}
	return [<tr> [<td> [join $els </td><td>]]]
    }

    proc hrow {args} {
	set els {}
	foreach el $args {
	    lappend els [subst $el]
	}
	return [<tr> [<th> [join $els </th><th>]]]
    }

    proc dlist {args} {
	return [<dl> [join $args \n]]\n
    }

    proc dl {term def} {
	Debug.STX {2HTML DL: $term / $def}
	return "[<dt> [subst $term]]\n[<dd> [subst $def]]\n"
    }

    # make list item
    proc li {content args} {
	Debug.STX {2HTML li: '$content' '$args'}
	return "[subst $content]\n[join $args]\n"
    }

    proc llist {form content} {
	set result "<${form}>"
	set open 0
	set cnt 0
	foreach arg $content {
	    if {$cnt && [string match {<[ou]l>*} $arg]} {
		if {!$open} {
		    append result "\n<li>"
		}
		append result "${arg}"
		set open 1
	    } else {
		if {$open} {
		    append result "\n</li>"
		}
		append result "\n<li>${arg}"
		set open 1
	    }
	    incr cnt
	}
	if {$open} {
	    append result "\n</li>"
	}
	return "${result}\n</${form}>\n"
    }

    # translate unordered list
    proc ul {args} {
	return [llist ul $args]
    }

    proc ol {args} {
	return [llist ol $args]
    }

    # This is a NOOP to convert local references to HTML
    # the application should supply its own version to the translate call
    proc local {what} {
	# org code
	set body [join [lassign [split $what] href]]
	if {$body eq ""} {
	    set body $what
	}
	return [<a> href $href $body]

	return $what

	# org code
	set what [split $what]
	return [<a> href [lindex $what 0] [join [lrange $what 1 end]]]
    }

    # make reference content
    proc ref {what} {
	set body [string trim [join [lassign [split $what :] proto] :]]
	set proto [string trim $proto]
	#puts stderr "STX '$what' '$proto' '$body'"
	Debug.STX {2HTML ref '$proto' '$body'}
	switch -glob -- $proto {
	    http {
		set text [string trim [join [lassign [split $body] href]]]
		set href [string trim $href]
		if {$text eq ""} {
		    set text $body
		}
		if {![string match /* $href]} {
		    set what [<a> href $href $text]
		} else {
		    set what [<a> href http:$href $text]
		}
	    }

	    acronym {
		set body [split $body "|"]
		set text [string trim [join [lrange $body 1 end]]]
		set body [string trim [lindex $body 0]]
		set what [<acronym> title $text $body]
	    }

	    \# {
		set body [split $body]
		set text [string trim [join [lrange $body 1 end]]]
		set body [string trim [lindex $body 0]]
		set what [<a> name $body $text]
	    }

	    image {
		set body [split $body]
		set text [string trim [join [lrange $body 1 end]]]
		set body [string trim [lindex $body 0]]
		if {[string match /* $body]} {
		    set body "http:$body"
		}
		if {$text eq ""} {
		    set text $body
		}
		variable img_properties
		set what [<img> src $body class image {*}$img_properties alt $text]
	    }

	    left {
		set body [split $body]
		set text [string trim [join [lrange $body 1 end]]]
		set body [string trim [lindex $body 0]]
		if {[string match /* $body]} {
		    set body "http:$body"
		}
		if {$text eq ""} {
		    set text $body
		}
		variable img_properties
		set what [<img> src $body class imageleft {*}$img_properties style "float:left" alt $text]"
	    }

	    right {
		set body [split $body]
		set text [string trim [join [lrange $body 1 end]]]
		set body [string trim [lindex $body 0]]
		if {[string match /* $body]} {
		    set body "http:$body"
		}
		if {$text eq ""} {
		    set text $body
		}
		variable img_properties
		set what [<img> src $body class imageleft {*}$img_properties style "float:right" alt $text]"
	    }

	    fieldset {
		catch {Form fieldsetS {*}$body {}} what eo
	    }
	    /fieldset {
		set what </fieldset>
	    }

	    form {
		catch {Form formS {*}$body {}} what eo
	    }

	    /form {
		set what </form>
	    }

	    selectset {
		set body [string map [list "\x89" \{ "\x8A" \}] $body]
		set opts [join [lindex $body end] \n]

		if {[catch {<${proto}> {*}[lrange $body 0 end-1] $opts} what eo]} {
		    append what ($eo)
		}
		append what \n
	    }

	    radioset - checkset {
		set body [string map [list "\x89" \{ "\x8A" \}] $body]
		set opts {}
		foreach {opt val} [lindex $body end] {
		    lappend opts [list $opt $val]
		}
		set opts [join $opts \n]

		if {[catch {<${proto}> {*}[lrange $body 0 end-1] $opts} what eo]} {
		    append what ($eo)
		}
		append what \n
	    }

	    password - text - hidden - file - image - textarea -
	    button - reset - submit - radio - checkbox - legend {
		catch {<${proto}> {*}$body} what eo
		append what \n
	    }

	    default {
		variable local
		set what [$local $what]
	    }
	}

	return $what
    }

    # make content underlined
    proc underline {args} {
	return [<span> style {text-decoration:underline;} [join $args]]
    }

    # make content underlined
    proc strike {args} {
	return [<span> style {text-decoration:line-through;} [join $args]]
    }

    # make content subscript
    proc subscript {args} {
	return [<span> style {vertical-align:sub;} [join $args]]
    }

    # make content superscript
    proc superscript {args} {
	return [<span> style {vertical-align:super;} [join $args]]
    }

    # make content big
    proc big {args} {
	return [<span> style {font-size:bigger;} [join $args]]
    }

    # make content strong
    proc strong {args} {
	return [<span> style {font-weight:bold;} [join $args]]
    }

    # make content italic
    proc italic {args} {
	return [<span> style {font-style:italic;} [join $args]]
    }

    # make content italic
    proc smallcaps {args} {
	return [<span> style {font-variant:small caps;} [join $args]]
    }

    proc toc {} {
	variable toc
	set result "<table class='TOC'>\n"
	append result "<thead><tr><td colspan='2'>Table Of Contents</td></tr></thead>"
	append result "<tbody>"
	foreach sect [lsort -dictionary [array names toc]] {
	    append result "<tr>"
	    append result "<td class='TOCnum'><a href='\#[lindex $toc($sect) 1]'>$sect</a></td> "
	    append result "<td>[lindex $toc($sect) 0]</td>\n"
	    append result "</tr>"
	}
	append result "</tbody>"
	return "${result}</table>\n"
    }

    proc scope {num} {
	#puts stderr "SCOPE: $num"
	Debug.STX {scope: $num}
	variable script
	if {$script} {
	    return [interp eval istx subst [list $::stx::scope($num)]]
	} else {
	    return "evaluation disabled"
	}
    }

    # convert structured text to html
    proc translate {text {locallink ::stx2html::local}} {
	variable features
	array unset features
	set features(NOTOC) 1
	variable local $locallink

	variable toc
	array unset toc

	variable title ""
	variable toccnt {}
	variable tagstart	;# number to start tagging TOC sections

	Debug.STX {TRANSLATING}
	set stx [stx::translate $text $tagstart]
	Debug.STX {TRANSLATE: $stx}
	set content [lindex [namespace inscope ::stx2html subst [list $stx]] 0]

	if {0} {
	    set content ""; set content1 ""; set content2 ""
	    if {[catch {stx::translate $text} content eo]} {
		return "<p>STX Error translate: '$text'<br>-> $eo</p>"
	    }
	    if {[catch {namespace inscope ::stx2html subst [list $content]} content1 eo]} {
		return "<p>STX Error subst: $content<br>-> $eo</p>"
	    }
	    if {[catch {lindex $content1 0} content eo]} {
		return "<p>STX Error lindex: '$content1'<br>-> $eo</p>"
	    }
	}

	if {![info exists features(NOTOC)]} {
	    if {[info exists features(TOC)]} {
		set content [string map [list "\x81TOC\x82" [toc]] $content]
	    } elseif {[array size toc] > 3} {
		set content "[toc]\n$content"
	    }
	} else {
	    set content [string map [list "\x81TOC\x82" ""] $content]
	}

	return [string map [list "\x88" "&\#" "\x89" "\{" "\x8A" "\}" "\x8B" "$" "\x87" "\;" "\x84" "&#91\;" "\x85" "&#93\;" "\\" ""] $content]
    }

    variable packages {Form}
    variable script 0
    variable home [file dirname [file normalize [info script]]]
    variable path [list [info library] /usr/share/tcltk/tcl[info tclversion]/ [file dirname $home]/Utilities/ [file dirname $home]/extensions/ /usr/lib/tcllib1.10/textutil/ /usr/lib/tcllib1.10/snit/]
    #puts stderr "PATH:$path"
    proc init {args} {
	if {$args ne {}} {
	    variable {*}$args
	}

	variable script
	variable packages
	variable path
	variable home

	if {$script && ![interp exists istx]} {
	    # create our scope interpreter
	    #puts stderr "Creating Safe istx"
	    variable path
	    ::safe::interpCreate istx -accessPath $path
	    #puts stderr "Initing Safe istx"
	    foreach p $packages {
		#puts stderr "Installing $p"
		interp eval istx [list package require $p]
		#puts stderr "Installed $p"
	    }
	    #puts stderr "Created Safe istx"
	}
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

stx2html init
