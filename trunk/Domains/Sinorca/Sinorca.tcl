package require fileutil
package require Html
package require RAM
package require Form

package provide Sinorca 1.0

namespace eval Sinorca {
    proc <markup> {args} {
	return [<code> class markup [join $args]]
    }

    proc <tooltip> {tooltip args} {
	return [<span> class tooltip title $tooltip [join $args]]
    }

    proc <navbox> {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	return [<div> class navbox [<ol> <li>[Html links "</li>\n<li>" $args]</li>]]
    }

    proc <floatbox> {args} {
	Html argsplit
	Html template title {[<p> class title $title]}
	Html template hidden {[<p> class hidden $hidden]}

	return [<div> class floatbox {*}$args "[<hr>]
		$hidden
		$title
		[uplevel 1 [list subst $content]]
		[<hr>]"]
    }

    # Global site header
    proc <header> {args} {
	Html argsplit
	return [<div> {*}$args class left [uplevel 1 [list subst $content]]]
    }

    # Global links
    proc <global> {args} {
	Html argsplit
	return [<div> {*}$args class right [subst {
	    [<span> class hidden {Useful Links:}]
	    [uplevel 1 [list subst $content]]
	}]]
    }

    # Site Links
    proc <site> {args} {
	Html argsplit
	return [<div> {*}$args class subheader [subst {
	    [<span> class hidden {Navigation:}]
	    [uplevel 1 [list subst $content]]
	}]]
    }

    proc <sidebox> {args} {
	Html argsplit
	Html template title {[<p> class title $title]}

	return [<div> {*}$args "$title
		[uplevel 1 [list subst $content]]
	"]
    }

    # left sidebar
    proc <sidebar> {content} {
	return [<div> id sidebar [uplevel 1 [list subst $content]]]
    }

    # main content
    proc <content> {args} {
	Html argsplit

	set result ""

	set breadcrumbs ""
	Html template breadcrumbs {[<div> id navhead [subst {
	    [<hr>][<span> class hidden {Path to this page:}]
	    [Html links { &raquo; } {*}$breadcrumbs]
	}]]}

	set navbox ""
	Html template navbox {[<navbox> $navbox]}

	set result "${breadcrumbs}\n${navbox}"
	append result $content \n
	append result [<br> id endmain]

	return [<div> id main {*}$args $result]
    }

    proc <fade> {args} {
	Html argsplit
	return [<span> class fade $content]
    }

    proc <footer> {args} {
	Html argsplit
	set copyright ""
	Html template copyright
	set links ""
	Html template links {[<span> class notprinted [Html links . $links]][<br>]}

	return [<div> id footer {*}$args [subst {
	    [<hr>]
	    $copyright
	    $links
	    [uplevel 1 [list subst $content]]
	}]]
    }

    proc sidebar {contents args} {
	dict set contents "sidebar[dict incr contents _sides]" [<sidebox> {*}$args]
	return $contents
    }

    proc .style/sinorca.x-text/html-fragment {rsp} {
	set contents [dict get $rsp -content]
	#puts stderr "SINORCA: [dict keys $contents]"
	foreach var {globlinks global header sitelinks breadcrumbs sidebar content copyright footlinks footer navbox} {
	    set $var {}
	}

	foreach key [lsort -dictionary [dict keys $contents "side*"]] {
	    append sidebar [dict get $contents $key] \n
	    dict unset contents key
	}

	variable path
	dict lappend rsp -headers [<stylesheet> [file join $path screen.css]]
	dict lappend rsp -headers [<stylesheet> [file join $path print.css] print]
	#puts stderr "SINORCA: [dict keys $contents]"
	dict with contents {}
	return [dict replace $rsp -raw 1 \
		    content-type text/html \
		    -content [subst {
			[<div> id mainlink [<a> href "\#main" "Skip to main content."]]
			
			[<div> id header [subst {
			    [<header> $header]
			    [<global> "[Html links | $globlinks]\n$global"]
			    [<site> [Html links | $sitelinks]]
			}]]
			[<sidebar> $sidebar]
			[<content> navbox $navbox breadcrumbs $breadcrumbs $content]
			[<footer> copyright $copyright links $footlinks $footer]
		    }]]
    }

    variable colours {
	%BODY_FG black
	%BODY_BG white
	%HIGHLIGHT_BG #F0F0F0
	%LIGHTER #F8F8F8
	%SUBHEAD #FDA05E
	%LEFT #FF9800
	%HR #999999
	
	%HEADER_FG #003399
	%HEADER_BG #8CA8E6
	%H1 #999999

	%BORDER transparent

	%FOOTER_BG #6381DC
	%FOOTER_FG white
	
	%TOOLTIP #CCCCCC
	%VISITED #003399
	%LINK #0066CC
    }

    variable home [file dirname [info script]]
    variable screen [::fileutil::cat [file join $home Sinorca-screen.css]]
    variable print [::fileutil::cat [file join $home Sinorca-print.css]]
    variable index ""

    variable path "/sinorca/"

    proc init {args} {
	if {$args ne {}} {
	    variable {*}$args
	}
	variable colours
	variable screen
	variable print
	set screen [string map $colours $screen]

	variable path
	RAM init ram $path

	ram set screen.css $screen content-type text/css
	ram set print.css $print content-type text/css

	variable home 
	foreach el {valid-css.png valid-xhtml10.png wcag1AA.png file-pdf.png totop.png gradient.png} {
	    ram set $el [::fileutil::cat -translation binary -- [file join $home $el]] content-type image/png 
	}

	variable index
	ram set index.html $index content-type style/sinorca -header [<script> {
	    /* Prevent another site from "framing" this web page */
	    if (top != self) {
		top.location.href = location.href;
	    }
	} -header [<style> [string map {
	    %C \#333333
	    %P \#006600
	    %S \#000000
	    %T \#000099
	    %U \#FF0000
	    %V \#FF00FF
	    %X \#999999
	} {
	    /* Styles local to this document */
	    .width50            { width: 50% }
	    table.extraspace td { padding: 1em 5em }
	    div.example         { margin: 1em 0; padding: 0 2.5em; border: 1px solid %X }
	    .genus              { font-style: italic }
	    
	    /* Styles for C code syntax highlighting */
	    .c-comment { color: %C; background: transparent; font-style: italic }
	    .c-preproc { color: %P; background: transparent }
	    .c-syntax  { color: %S; background: transparent; font-weight: bold }
	    .c-type    { color: %T; background: transparent }
	    .c-var     { color: %S; background: transparent }
	    .c-func    { color: %S; background: transparent }
	    .c-funcdef { color: %S; background: transparent; font-weight: bold }
	    .c-string  { color: %U; background: transparent }
	    .c-spchar  { color: %V; background: transparent; font-weight: bold }
	    .c-num     { color: %T; background: transparent }
	    
	    /* Styles for XML code syntax highlighting */
	    .xml-comment { color: %X; background: transparent; font-style: italic }
	}] -header [<style> media screen {
	    div.nb-htmlsampler-width { width: 15em }
	    div.nb-htmlsampler-fixup { margin-right: 17.5em }
	}] -header [<style> media print {
	    div.nb-htmlsampler-fixup  { margin-right: inherit }
	}]]]

	ram set "" index.html content-type x-system/redirect

	namespace export -clear *
	namespace ensemble create -subcommands {}
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

namespace eval Sinorca {
    dict set index content [::fileutil::cat [file join $home Sinorca.htmlf]]
    dict set index globlinks {
	Home ..
	Contacts index.html
	Feedback index.html
	Search index.html
	About index.html
    }

    dict set index global [<div> [<form> search action index.html {
	[<text> q size 15 maxlength 250]
	[<image> submit src search.png alt Search]
    }]]

    dict set index header [<h1> "Sinorca[<fade> ish]"]
    
    dict set index sitelinks {
	Home ..
	Products index.html
	Services index.html
	Support index.html
	About index.html
	Other index.html
    }

    dict set index breadcrumbs {
	Home ..
	Sample index.html
    }
	    
    dict set index sidebar100 [<sidebox> title [<a> href index.html "Sample"] {
	[Html ulinks Overview index.html Template index.html "Sample Page" index.html "Logo Images" index.html]
    }]
    
    dict set index sidebar101 [<sidebox> class lighter title {Sample Sidebar} {
	[<p> "You can have additional divisions in the left sidebar, as shown by this example."]
	[<p> "Please note that the contents of the left sidebar are [<em> not] printed out to paper!"]
    }]
    
    dict set index sidebar102 [<sidebox> title "W3C Validation" {
	[<p> "[<a> href "http://validator.w3.org/check?uri=referer" [<img> src valid-xhtml10.png alt "Validate against the XHTML 1.0 Strict standard" width 88 height 31]][<br>] [<a> href http://jigsaw.w3.org/css-validator/check/referer [<img> src valid-css.png alt "Validate against the CSS 2.1 standard" width 88 height 31]][<br>] [<a> href http://www.w3.org/WAI/WCAG1AA-Conformance [<img> src wcag1AA.png alt "Conforms with Level Double-A of the Web Content Accessibility Guidelines 1.0" width 88 height 32]]<br>Note: this page doesn't actually validate."]
    }]
    
    dict set index navbox {
	"Wub Integration" #h-wub
	"Making a Start" #h-making-start
	"Main Design Elements" #h-main-design
	"A Brief HTML Sampler" #h-html-sampler
	"The Final Word" #h-final-word
    }

    variable home
    dict set index content [fileutil::cat [file join $home Sinorca.htmlf]]
    
    dict set index copyright {Copyright &copy; 2004&ndash;07, John Zaitseff.  All rights reserved.}
    
    dict set index footlinks {
	"Terms of Use" index.html
	"Privacy Policy" index.html
    }

    dict set index footer "This web site is maintained by [<a> href mailto:J.Zaitseff@zap.org.au {John Zaitseff}] Last modified: 22nd March, 2007."
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    lappend auto_path ~/Desktop/Work/Wub/Utilities
    lappend auto_path ~/Desktop/Work/Wub/extensions

    package require Form
    package require Html
    

    #puts stderr [dict keys $content]
    puts [dict get [Sinorca::.style/sinorca.x-text/html-fragment [list -content $::Sinorca::index]] -content]
}
