# Html.tcl - Useful facilities for manipulating and analysing HTML text.
#
# provides a facility to automatically construct <TAG> procs
#
# provides subst-versions of if, while, foreach and switch commands

package require WubUtils
package require Query
package require know
package provide Html 1.0

set ::API(Utilities/Html) {
    {
	HTML tag soup generator

	Provides tcl procedures for most HTML tags, and some specials.
    }
}

alias tclarmour string map {\[ "&#x5B;" \] "&#x5D;" \{ "&#x7B;" \} "&#x7D;" $ "&#x24;"}

#interp alias {} armour {} string map [list &\# &\# & &amp\; < &lt\; > &gt\; \" &quot\; ' &\#39\;]
interp alias {} armour {} string map {
    &\# &\# ' &\#39;
    \xa0 &nbsp; \xa1 &iexcl; \xa2 &cent; \xa3 &pound; \xa4 &curren;
    \xa5 &yen; \xa6 &brvbar; \xa7 &sect; \xa8 &uml; \xa9 &copy;
    \xaa &ordf; \xab &laquo; \xac &not; \xad &shy; \xae &reg;
    \xaf &macr; \xb0 &deg; \xb1 &plusmn; \xb2 &sup2; \xb3 &sup3;
    \xb4 &acute; \xb5 &micro; \xb6 &para; \xb7 &middot; \xb8 &cedil;
    \xb9 &sup1; \xba &ordm; \xbb &raquo; \xbc &frac14; \xbd &frac12;
    \xbe &frac34; \xbf &iquest; \xc0 &Agrave; \xc1 &Aacute; \xc2 &Acirc;
    \xc3 &Atilde; \xc4 &Auml; \xc5 &Aring; \xc6 &AElig; \xc7 &Ccedil;
    \xc8 &Egrave; \xc9 &Eacute; \xca &Ecirc; \xcb &Euml; \xcc &Igrave;
    \xcd &Iacute; \xce &Icirc; \xcf &Iuml; \xd0 &ETH; \xd1 &Ntilde;
    \xd2 &Ograve; \xd3 &Oacute; \xd4 &Ocirc; \xd5 &Otilde; \xd6 &Ouml;
    \xd7 &times; \xd8 &Oslash; \xd9 &Ugrave; \xda &Uacute; \xdb &Ucirc;
    \xdc &Uuml; \xdd &Yacute; \xde &THORN; \xdf &szlig; \xe0 &agrave;
    \xe1 &aacute; \xe2 &acirc; \xe3 &atilde; \xe4 &auml; \xe5 &aring;
    \xe6 &aelig; \xe7 &ccedil; \xe8 &egrave; \xe9 &eacute; \xea &ecirc;
    \xeb &euml; \xec &igrave; \xed &iacute; \xee &icirc; \xef &iuml;
    \xf0 &eth; \xf1 &ntilde; \xf2 &ograve; \xf3 &oacute; \xf4 &ocirc;
    \xf5 &otilde; \xf6 &ouml; \xf7 &divide; \xf8 &oslash; \xf9 &ugrave;
    \xfa &uacute; \xfb &ucirc; \xfc &uuml; \xfd &yacute; \xfe &thorn;
    \xff &yuml; \u192 &fnof; \u391 &Alpha; \u392 &Beta; \u393 &Gamma;
    \u394 &Delta; \u395 &Epsilon; \u396 &Zeta; \u397 &Eta; \u398 &Theta;
    \u399 &Iota; \u39A &Kappa; \u39B &Lambda; \u39C &Mu; \u39D &Nu;
    \u39E &Xi; \u39F &Omicron; \u3A0 &Pi; \u3A1 &Rho; \u3A3 &Sigma;
    \u3A4 &Tau; \u3A5 &Upsilon; \u3A6 &Phi; \u3A7 &Chi; \u3A8 &Psi;
    \u3A9 &Omega; \u3B1 &alpha; \u3B2 &beta; \u3B3 &gamma; \u3B4 &delta;
    \u3B5 &epsilon; \u3B6 &zeta; \u3B7 &eta; \u3B8 &theta; \u3B9 &iota;
    \u3BA &kappa; \u3BB &lambda; \u3BC &mu; \u3BD &nu; \u3BE &xi;
    \u3BF &omicron; \u3C0 &pi; \u3C1 &rho; \u3C2 &sigmaf; \u3C3 &sigma;
    \u3C4 &tau; \u3C5 &upsilon; \u3C6 &phi; \u3C7 &chi; \u3C8 &psi;
    \u3C9 &omega; \u3D1 &thetasym; \u3D2 &upsih; \u3D6 &piv;
    \u2022 &bull; \u2026 &hellip; \u2032 &prime; \u2033 &Prime;
    \u203E &oline; \u2044 &frasl; \u2118 &weierp; \u2111 &image;
    \u211C &real; \u2122 &trade; \u2135 &alefsym; \u2190 &larr;
    \u2191 &uarr; \u2192 &rarr; \u2193 &darr; \u2194 &harr; \u21B5 &crarr;
    \u21D0 &lArr; \u21D1 &uArr; \u21D2 &rArr; \u21D3 &dArr; \u21D4 &hArr;
    \u2200 &forall; \u2202 &part; \u2203 &exist; \u2205 &empty;
    \u2207 &nabla; \u2208 &isin; \u2209 &notin; \u220B &ni; \u220F &prod;
    \u2211 &sum; \u2212 &minus; \u2217 &lowast; \u221A &radic;
    \u221D &prop; \u221E &infin; \u2220 &ang; \u2227 &and; \u2228 &or;
    \u2229 &cap; \u222A &cup; \u222B &int; \u2234 &there4; \u223C &sim;
    \u2245 &cong; \u2248 &asymp; \u2260 &ne; \u2261 &equiv; \u2264 &le;
    \u2265 &ge; \u2282 &sub; \u2283 &sup; \u2284 &nsub; \u2286 &sube;
    \u2287 &supe; \u2295 &oplus; \u2297 &otimes; \u22A5 &perp;
    \u22C5 &sdot; \u2308 &lceil; \u2309 &rceil; \u230A &lfloor;
    \u230B &rfloor; \u2329 &lang; \u232A &rang; \u25CA &loz;
    \u2660 &spades; \u2663 &clubs; \u2665 &hearts; \u2666 &diams;
    \x22 &quot; \x26 &amp; \x3C &lt; \x3E &gt; \u152 &OElig;
    \u153 &oelig; \u160 &Scaron; \u161 &scaron; \u178 &Yuml;
    \u2C6 &circ; \u2DC &tilde; \u2002 &ensp; \u2003 &emsp; \u2009 &thinsp;
    \u200C &zwnj; \u200D &zwj; \u200E &lrm; \u200F &rlm; \u2013 &ndash;
    \u2014 &mdash; \u2018 &lsquo; \u2019 &rsquo; \u201A &sbquo;
    \u201C &ldquo; \u201D &rdquo; \u201E &bdquo; \u2020 &dagger;
    \u2021 &Dagger; \u2030 &permil; \u2039 &lsaquo; \u203A &rsaquo;
    \u20AC &euro;
}

# xmlarmour - remove characters offensive to xml
interp alias {} xmlarmour {} string map [list &\# &\# & &amp\; < &lt\; > &gt\; \" &quot\; ' &\#39\; \x00 " " \x01 " " \x02 " " \x03 " " \x04 " " \x05 " " \x06 " " \x07 " " \x08 " " \x0B " " \x0C " " \x0E " " \x0F " " \x10 " " \x11 " " \x12 " " \x13 " " \x14 " " \x15 " " \x16 " " \x17 " " \x18 " " \x19 " " \x1A " " \x1B " " \x1C " " \x1D " " \x1E " " \x1F " " \x7F " "]

# control_armour - remove control characters
interp alias {} control_armour {} string map [list \x00 " " \x01 " " \x02 " " \x03 " " \x04 " " \x05 " " \x06 " " \x07 " " \x08 " " \x0B " " \x0C " " \x0E " " \x0F " " \x10 " " \x11 " " \x12 " " \x13 " " \x14 " " \x15 " " \x16 " " \x17 " " \x18 " " \x19 " " \x1A " " \x1B " " \x1C " " \x1D " " \x1E " " \x1F " " \x7F " "]

# demoronizer - remove MS specials.
proc demoronizer {} {
    set result {}
    foreach line [split  {
	128 8364 # euro sign
	130 8218 # single low-9 quotation mark
	131  402 # latin small letter f with hook
	132 8222 # double low-9 quotation mark
	133 8230 # horizontal ellipsis
	134 8224 # dagger
	135 8225 # double dagger
	136  710 # modifier letter circumflex accent
	137 8240 # per mille sign
	138  352 # latin capital letter s with caron
	139 8249 # single left-pointing angle quotation mark
	140  338 # latin capital ligature oe
	142  381 # latin capital letter z with caron
	145 8216 # left single quotation mark
	146 8217 # right single quotation mark
	147 8220 # left double quotation mark
	148 8221 # right double quotation mark
	149 8226 # bullet
	150 8211 # en dash
	151 8212 # em dash
	152  732 # small tilde
	153 8482 # trade mark sign
	154  353 # latin small letter s with caron
	155 8250 # single right-pointing angle quotation mark
	156  339 # latin small ligature oe
	158  382 # latin small letter z with caron
	159  376 # latin capital letter y with diaeresis
    } \n] {
	set line [string trim $line]
	if {$line eq ""} continue
	lassign [split [string trim $line]] from to
	lappend result \\u$from \\u$to
    }
    return [subst -nocommands -novariables $result]
}
interp alias {} demoronizer {} string map [demoronizer]

namespace eval ::Html {
    variable XHTML 0
    variable tagmods 

    # convert a dict to a JSON object
    proc dict2json {d} {
	set result {}
	dict for {k v} $d {
	    lappend result \"[jsarmour $k]\":\"[jsarmour $v]\"
	}
	return \{[join $result ,]\}
    }

    # arrange a set of links as a list
    proc links {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	if {[llength $args] % 2} {
	    set sep [lindex $args 0]
	    set args [lrange $args 1 end]
	} else {
	    set sep ""
	}
	set content {}
	foreach {n url} $args {
	    if {[llength $url] == 1} {
		lappend content [<a> href [lindex $url 0] $n]
	    } else {
		lappend content [<a> {*}$url $n]
	    }
	}
	return [join $content $sep]
    }

    proc ulinks {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	return [<ul> [<li> [links </li>\n<li> {*}$args]]]
    }

    proc olinks {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	return [<ol> [<li> [links </li>\n<li> {*}$args]]]
    }


    proc argsplit {} {
	upvar args margs
	upvar content content

	set content [lindex $margs end]
	if {[llength $margs] > 1} {
	    set margs [lrange $margs 0 end-1]
	} else {
	    set margs {}
	}
    }

    # do arg template substitution in parent
    proc template {name {template ""}} {
	upvar args args
	upvar $name var

	if {[dict exists $args $name]} {
	    if {[dict get $args $name] ne ""} {
		set var [dict get $args $name]
		if {$template ne ""} {
		    set var [uplevel 1 [list subst $template]]
		}
	    }
	    dict unset args $name
	} else {
	    set var ""
	}
    }

    # add arbitrary text to header
    proc header {r content} {
        dict lappend r -headers $content
        return $r
    }

    # add a script to the response <head>
    proc prescript {r url} {
	dict set r -prescript $url {}
	return $r
    }

    # add a script to the response
    proc script {r url {script ""}} {
	if {$script ne ""} {
	    set script [<script> $script]
	}
	dict set r -script $url $script
	return $r
    }

    # add a script to the response
    proc postscript {r script} {
	dict set r -script !$script [<script> $script]
	return $r
    }

    # add style to the response
    proc style {r url args} {
	dict set r -style $url $args
	return $r
    }

    # add style to the response
    proc prestyle {r style} {
	dict set r -style !$style $style
	return $r
    }

    # attr - Construct a properly formed attribute name/value string
    # for inclusion in an HTML element.
    proc attr {T args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}

	# handle per-invocation defaults
	if {[dict exists $args -defaults]} {
	    set defaults [dict get $args -defaults]
	    set args [list {*}$defaults {*}$args]
	}

	# handle per-tag defaults
	set defs [Html default $T]
	if {[dict exists $args class]} {
	    dict args.class [list {*}[dict args.class] {*}[dict defs.class?]]
	}
	set args [list {*}[Html default $T] {*}$args]

	set result ""
	set class {}
	array set seen {}
	foreach {n v} $args {
	    if {$n eq "-defaults"} continue

	    if {$n in {checked disabled selected noshade}} {
		if {$v && ![info exists seen($n)]} {
		    lappend result $n
		}
	    } elseif {$n eq "class"} {
		foreach c [split [string trim $v]] {
		    if {$c ne {}} {
			lappend class [armour $c]
		    }
		}
	    } elseif {![info exists seen($n)]} {
		lappend result "[string trim $n]='[armour [string trim $v]]'"
	    }
	    set seen($n) 1
	}

	if {$class ne {}} {
	    return "$T class='[join $class]' [join $result]"
	} else {
	    return "$T [join $result]"
	}
    }

    variable default {
	script {type text/javascript}
	style {type text/css}
    }

    proc default {tag args} {
	variable default
	if {[dict exists $default $tag]} {
	    return [dict get $default $tag]
	}
    }

    # turn dict or alist into a <ul> list
    proc menulist {menu} {
	return [<ul> [Foreach {text url} $menu {
	    [<li> [<a> href $url $text]]
	}]]
    }

    # turn dict into tables
    proc table {name args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	return [<table> border 1 width 80% [subst {
	    [<tr> [<th> $name]]
	    Foreach n [lsort [dict keys $args]] {
		[<tr> [<td> $n] [<td> [dict get $args $n]]]
	    }
	}]]
    }

    # dict2table - convert dict into sortable HTML table
    proc dict2table {dict header {footer {}}} {
	set row 0
	return [<table> class sortable summary "" [subst {
	    [<thead> [<tr> [Foreach t $header {
		[<th> [string totitle $t]]
	    }]]]
	    [Html If {$footer ne {}} {
		[<tfoot> [<tr> [Foreach t $footer {[<th> [string totitle $t]]}]]]
	    }]
	    [<tbody> [Foreach {k v} $dict {
		[<tr> class [If {[incr row] % 2} even else odd] \
		     [Foreach th $header {
			 [If {[dict exists $v $th]} {
			     [<td> [dict get $v $th]]
			 } else {
			     [<td> {}]
			 }]
		     }]]
	    }]]
	}]]
    }

    # dict2table - convert dict into sortable HTML table
    # provisional new version
    proc dict2table {dict header {footer {}} {tag ""}} {
	set row 0
	return [<table> class sortable summary "" {*}[If {$tag ne ""} { class $tag }] [subst {
	    [<thead> [<tr> [Foreach t $header {
		[<th> class $t [string totitle $t]]
	    }]]]
	    [If {$footer ne {}} {
		[<tfoot> [<tr> [Foreach t $footer {[<th> [string totitle $t]]}]]]
	    } else {
		[<tfoot> [<tr> [Foreach t $header {[<th> [string totitle $t]]}]]]
	    }]
	    [<tbody> [Foreach {k v} $dict {
		[<tr> class [If {[incr row] % 2} even else odd] \
		     [Foreach th $header {
			 [If {[dict exists $v $th]} {
			     [<td> class $th [dict get $v $th]]
			 } else {
			     [<td> {}]
			 }]
		     }]]
	    }]]
	}]]
    }

    # dir2table - convert directory into sortable table
    proc dir2table {dir header {footer {}}} {
	if {$header eq {}} {
	    set header {name size mtime ctime atime}
	}
	return [dict2table [dir2dict [Dict dir $dir $header] $header $footer]]
    }

    variable paMatch {
	quote "^(\[a-zA-Z0-9:_-]+)\[\ \t]*=\[\ \t]*\[\"](\[^\"]*)\[\"]\[\ \t]*(.*)\$"
	squote "^(\[a-zA-Z0-9:_-]+)\[\ \t]*=\[\ \t]*\['](\[^']*)\[']\[\ \t]*(.*)\$"
	uquote "^(\[a-zA-Z0-9:_-]+)\[\ \t]*=\[\ \t]*(\[^\ \t'\"]+)\[\ \t]*(.*)\$ "
    }

    proc parseAttr {astring} {
	variable paMatch
	set attr {}
	set astring [string trim $astring]
	if {$astring eq ""} {
	    return {}
	}

	#puts stderr "parseAttr: '$astring'"

	while {$astring != ""} {
	    set org $astring
	    foreach m {quote squote uquote} {
		if {[regexp [dict get $paMatch $m] $astring all var val suffix]} {
		    #puts stderr "parseAttr $m: $var = '$val'"
		    dict set attr $var $val
		    set astring [string trimleft $suffix]
		}
	    }
	    if {$astring == $org} {
		error "parseAttr: can't parse $astring - not a properly formed attribute string"
	    }
	}
	#puts stderr "parseAttr: [array get attr]"
	return $attr
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

# optional span
proc <span>? {args} {
    set content [lindex $args end]
    if {$content eq {}} {
	return ""
    } else {
	return "<[Html::attr span {*}[lrange $args 0 end-1]]>$content</span>"
    }
}

foreach tag {author description copyright generator keywords} {
    eval [string map [list %T $tag] {
	proc <%T> {content} {
	    return [<meta> name %T content $content]
	}
    }]
}

# return a HTML singleton tag
foreach tag {img br hr} {
    eval [string map [list %T $tag] {
	proc <%T> {args} {
	    if {$::Html::XHTML} {
		set suff /
	    } else {
		set suff ""
	    }
	    return "<[Html::attr %T {*}$args]${suff}>"
	}
    }]
}

foreach tag {link meta} {
    eval [string map [list %T $tag] {
	proc <%T> {args} {
	    if {$::Html::XHTML} {
		return "<[Html::attr %T {*}$args]/>"
	    } else {
		return "<[Html::attr %T {*}$args]>"
	    }
	}
    }]
}

proc <stylesheet> {url {media screen}} {
    return [<link> rel StyleSheet type text/css media $media href $url]
}

proc <load> {url} {
    return "<[Html::attr script src $url]></script>"
}

proc <script> {args} {
    if {[llength $args]%2} {
	set script [lindex $args end]
	set args [lrange $args 0 end-1]

	set script "/* <!\[CDATA\[ */\n$script\n/* \]\]> */\n"
    } else {
	set script ""
    }

    return "<[Html::attr script {*}$args]>$script</script>"
}

foreach tag {style} {
    eval [string map [list %T $tag] {
	proc <%T> {args} {
	    if {([llength $args] % 2) == 1} {
		set content [lindex $args end]
		set args [lrange $args 0 end-1]
		return "<[Html::attr %T {*}$args]>$content</%T>"
	    } elseif {$::Html::XHTML} {
		return "<[Html::attr %T {*}$args]/>"
	    } else {
		return "<[Html::attr %T {*}$args]></%T>"
	    }
	}
    }]
}

foreach tag {html body head} {
    ::proc ::<$tag> {args} [string map [list @T $tag] {
	set content [lindex $args 0]
	if {[llength $args] > 1} {
	    set args [lrange $args 0 end-1]
	} else {
	    set args {}
	}
	set document "<[Html::attr @T $args]>"
	append document [uplevel 1 [list subst $content]] \n
	append document </@T>
    }]
}

proc <message> {args} {
    return [<p> class message [join $args "</p><p class='message'>"]]
}

proc <inc> {what} {
    return [<div> {*}[jQ tc $what] {}]
}

# return a nested set of HTML <divs>
proc divs {ids {content ""}} {
    set divs ""
    foreach id $ids {
	append divs "<div class='$id'>\n"
    }
    append divs [uplevel 1 subst [list $content]]
    append divs "\n"
    append divs [string repeat "\n</div>" [llength $ids]]
    return $divs
}

# HTML <> commands per http://wiki.tcl.tk/2776
know {[string match <*> [lindex $args 0]]} {
    set tag [string trim [lindex $args 0] "<>"]
    set htag [string trimright $tag !+]
    switch -- [string index $tag end] {
	! {
	    set mod {[uplevel 1 $content]}
	}
	+ {
	    set mod {[uplevel 1 [list subst $content]]}
	}
	default {
	    set mod {$content}	;# default just returns content
	}
    }

    ::proc ::<$tag> {args} [string map [list @T@ $htag @M@ $mod] {
        if {[llength $args]%2} {
            set content [lindex $args end]
            set args [lrange $args 0 end-1]
        } else {
            set content {}
        }
	set class {}
	set result "@T@"
	foreach {n v} $args {
	    if {$n eq "class"} {
		lappend class $v	;# aggregate class args
	    } else {
		lappend result "[string trim $n]='[armour [string trim $v]]'"
	    }
	}
	if {$class ne {}} {
	    lappend result "class='[join $class]'"
	}
	if {"@T@" eq "input"} {
	    if {$::Html::XHTML} {
		return "<[join ${result}]>@M@</@T@/>"
	    } else {
		return "<[join ${result}]>@M@</@T@>"
	    }
	} else {
	    return "<[join ${result}]>@M@</@T@>"
	}
    }]

    return [{*}$args]
}

# Some command equivalents which use subst instead of eval

# if using [subst] instead of [eval] to return its body
#
# Note that this version does not support the then keyword in if,
# and requires the else keyword.
proc If {args} {
    #puts stderr "IF cond: [lindex $args 0]"
    while {[llength $args] && ![uplevel 1 expr [list [lindex $args 0]]]} {
	set args [lrange $args 2 end]	;# lose the cond and positive-cond
	#puts stderr "IF cond: [lindex $args 0]"

	if {[lindex $args 0] eq "else"} {
            break
        }

	set args [lrange $args 1 end] ;# assumed to be 'elseif'
    }
    #puts stderr "IF consequence: [lindex $args 0]"
    return [uplevel 1 subst [list [lindex $args 1]]] ;# return with neg-consequence
}

# while using [subst] instead of [eval] to return its body
proc While {cond body} {
    set result {}
    while {[uplevel 1 expr [list $cond]]} {
	lappend result [uplevel 1 subst [list $body]]
    }
    return [join $result]
}

variable feCnt 0

# foreach using [subst] instead of [eval] to return its body
proc Foreach {args} {
    set body [lindex $args end]
    set vars [lrange $args 0 end-1]
    variable feCnt; incr feCnt
    set script [string map [list %A __FE${feCnt}__ %B $body %V $vars] {
	set {%A} {}
	foreach %V {
	    lappend {%A} [subst {%B}]
	}
	return [join [set {%A}]]
    }]
    #puts stderr "FOREACH: $script"
    return [uplevel 1 $script]
}

# switch using [subst] instead of [eval] to return its body
proc Switch {args} {
    set switch {}
    foreach {key body} [lindex $args end] {
	if {$body eq "-"} {
	    lappend switch $key -
	} else {
	    lappend switch $key [list subst $body]
	}
    }
    return [uplevel 1 [list switch {*}[lrange $args 0 end-1] $switch]]
}
