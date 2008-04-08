package require stx

package provide stx2html 1.1

namespace eval stx2html {
    variable features
    array set features {}

    variable toc
    array set toc {}

    variable toccnt {}
    variable tagstart 0	;# number to start tagging TOC sections

    variable title ""	;# title of page

    variable img_properties ""
}

proc stx2html::tagstart {what} {
    variable tagstart $what
}

# redefine subst - we only ever want commands in this application
proc stx2html::subst {what} {
    Debug.STX {subst: $what}
    return [::subst -nobackslashes -novariables $what]
}

proc stx2html::cdata {args} {
    return [join $args]
}

proc stx2html::normal {para args} {
    return "<p>[subst $para]</p>\n[join $args]"
}

proc stx2html::pre {para args} {
    return "<pre>$para</pre>\n[join $args]\n"
}

proc stx2html::special {para args} {
    set p [split $para]
    set cmd [string tolower [lindex $p 0]]
    Debug.STX {special: $cmd}
    if {[llength [info commands "_$cmd"]] == 1} {
	set para [_$cmd [lrange $p 1 end]]
    } else {
	set para "<p>[::stx::char $para]</p>"
    }
    return "$para\n[join $args]\n"
}

proc stx2html::_title {args} {
    variable title [join $args]
    return ""
}

proc stx2html::_message {args} {
    Debug.STX {STX Message: [join $args]}
    return ""
}

proc stx2html::_comment {args} {
    return "<!-- [join $args] -->\n"
}

proc stx2html::_notoc {args} {
    variable features
    set features(NOTOC) 1
    return ""
}

proc stx2html::_toc {args} {
    variable features
    set features(TOC) 1
    catch {unset features(NOTOC)}
    return "\x81TOC\x82"
}

proc stx2html::header {level para tag args} {
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

    return "<h$level id='$tag'>$p</h$level>\n[join $args]\n"
}

proc stx2html::hr {} {
    return "<hr style='clear:both;'>"
}

proc stx2html::indent {para} {
    return "<p>[subst $para]</p>"
}

proc stx2html::table {args} {
    return "<table>[join $args \n]</table>\n"
}

proc stx2html::row {args} {
    set els {}
    foreach el $args {
	lappend els [subst $el]
    }
    return "<tr><td>[join $els </td><td>]</td></tr>"
}

proc stx2html::hrow {args} {
    set els {}
    foreach el $args {
	lappend els [subst $el]
    }
    return "<tr><th>[join $els </th><th>]</th></tr>"
}

proc stx2html::dlist {args} {
    return "<dl>[join $args \n]</dl>\n"
}

proc stx2html::dl {term def} {
    Debug.STX {2HTML DL: $term / $def}
    return "<dt>[subst $term]</dt>\n<dd>[subst $def]</dd>\n"
}

# make list item
proc stx2html::li {content args} {
    Debug.STX {2HTML li: '$content' '$args'}
    return "[subst $content]\n[join $args]\n"
}

proc stx2html::llist {form content} {
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
proc stx2html::ul {args} {
    return [llist ul $args]
}

proc stx2html::ol {args} {
    return [llist ol $args]
}

# This is a NOOP to convert local references to HTML
# the application should supply its own version to the translate call
proc stx2html::local {what} {
    # org code
    set what [split $what]
    return "<a href='[lindex $what 0]'>[join [lrange $what 1 end]]</a>"

    return $what

    # org code
    set what [split $what]
    return "<a href='[lindex $what 0]'>[join [lrange $what 1 end]]</a>"
}

# make reference content
proc stx2html::ref {what} {
    set proto [split $what :]
    set body [join [lrange $proto 1 end] :]
    set proto [lindex $proto 0]
    Debug.STX {2HTML ref '$proto' '$body'}
    switch -- $proto {
	http {
	    set body [split $body]
	    set text [string trim [join [lrange $body 1 end]]]
	    set body [string trim [lindex $body 0]]
	    if {$text eq ""} {
		set text "http:$body"
	    }
	    if {![string match /* $body]} {
		set what "<a href='$body'>$text</a>"
	    } else {
		set what "<a href='http:$body'>$text</a>"
	    }
	}

	acronym {
	    set body [split $body "|"]
	    set text [string trim [join [lrange $body 1 end]]]
	    set body [string trim [lindex $body 0]]
	    set what "<acronym title='$text'>$body</acronym>"
	}

	\# {
	    set body [split $body]
	    set text [string trim [join [lrange $body 1 end]]]
	    set body [string trim [lindex $body 0]]
	    set what "<a name='$body'>$text</a>"
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
	    set what "<img src='$body' class='image' $img_properties alt='$text' />"
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
	    set what "<img src='$body' class='imageleft' $img_properties style='float:left' alt='$text' />"
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
	    set what "<img src='$body' class='imageright' $img_properties style='float:right' alt='$text' />"
	}

	default {
	    variable local
	    set what "<a href='[$local $what]'>"
	}
    }

    return $what
}

# make reference content - original/old version
proc stx2html::ref_org {what} {
    if {[llength [set proto [split $what :]]] > 1} {
	set body [join [lrange $proto 1 end] :]
	set proto [lindex $proto 0]
    } elseif {[string match \#* $what]} {
	set proto http
	set body $what
    } else {
	set proto ""
    }
    Debug.STX {ref '$proto' '$body'}
    switch -- $proto {
	http {
	    set body [split $body]
	    set text [string trim [join [lrange $body 1 end]]]
	    set body [string trim [lindex $body 0]]
	    if {$text eq ""} {
		set text "http:$body"
	    }
	    Debug.STX {body http:  ... $body}
	    if {![string match /* $body]} {
		set what "<a href='$body'>$text</a>"
	    } else {
		set what "<a href='http:$body'>$text</a>"
	    }
	}

	image {
	    set body [split $body]
	    set text [string trim [join [lrange $body 1 end]]]
	    set body [string trim [lindex $body 0]]
	    if {$text eq ""} {
		set text "http:$body"
	    }
	    set what "<img src='http:$body' alt='$text'>"
	}

	left {
	    set body [split $body]
	    set text [string trim [join [lrange $body 1 end]]]
	    set body [string trim [lindex $body 0]]
	    if {$text eq ""} {
		set text "http:$body"
	    }
	    set what "<img src='http:$body' style='float:left' alt='$text'>"
	}

	right {
	    set body [split $body]
	    set text [string trim [join [lrange $body 1 end]]]
	    set body [string trim [lindex $body 0]]
	    if {$text eq ""} {
		set text "http:$body"
	    }
	    set what "<img src='http:$body' style='float:right' alt='$text'>"
	}

	default {
	    variable local
	    set what [$local $what]
	}
    }

    return $what
}

# make content underlined
proc stx2html::underline {args} {
    return "<span style='text-decoration: underline;'>[join $args]</span>"
}

# make content underlined
proc stx2html::strike {args} {
    return "<span style='text-decoration: line-through;'>[join $args]</span>"
}

# make content subscript
proc stx2html::subscript {args} {
    return "<span style='vertical-align: sub;'>[join $args]</span>"
}

# make content superscript
proc stx2html::superscript {args} {
    return "<span style='vertical-align: super;'>[join $args]</span>"
}

# make content superscript
proc stx2html::subscript {args} {
    return "<span style='vertical-align: sub;'>[join $args]</span>"
}

# make content big
proc stx2html::big {args} {
    return "<span style='font-size: bigger;'>[join $args]</span>"
}

# make content strong
proc stx2html::strong {args} {
    return "<span style='font-weight: bold;'>[join $args]</span>"
}

# make content italic
proc stx2html::italic {args} {
    return "<span style='font-style: italic;'>[join $args]</span>"
}

# make content italic
proc stx2html::smallcaps {args} {
    return "<span style='font-variant: small-caps;'>[join $args]</span>"
}

proc stx2html::toc {} {
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

# convert structured text to html
proc stx2html::translate {text {locallink ::stx2html::local}} {
    variable features
    array unset features
    set features(NOTOC) 1
    variable local $locallink

    variable toc
    array unset toc

    variable title ""
    variable toccnt {}
    variable tagstart	;# number to start tagging TOC sections

    set content [lindex [namespace inscope ::stx2html subst [list [stx::translate $text $tagstart]]] 0]

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

    return [string map {\x88 &\# \x89 \{ \x8A \} \x8B $ \x87 ; \x84 &#91; \x85 &#93; \\ ""} $content]
}
