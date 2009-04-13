# About - a domain for documenting other domains via their API entries
package require Debug
Debug on about 10

package require textutil
package provide About 1.0

set API(Domains/About) {
    {Domain for accessing API documentation of other domains.}
}

set APIDomains {File Repo Mason Direct Honeypot Dub Commenter JQ CGI Icons Tub Login About RAM Tie Coco Sinorca Simplicio Nub}

namespace eval About {
    variable ctype x-text/html-fragment

    proc do {r} {
	foreach x $::APIDomains {
	    package require $x
	}

	proc do {r} {
	    # calculate the suffix of the URL relative to $mount
	    variable mount
	    lassign [Url urlsuffix $r $mount] result r suffix
	    if {!$result} {
		return $r	;# the URL isn't in our domain
	    }
	    
	    global API
	    if {![info exists API($suffix)]} {
		Debug.about {$suffix domain not known}
		if {$suffix eq "/"} {
		    set s [lsort -dictionary [array names API]]
		} else {
		    set s [lsort -dictionary [array names API [file join $suffix *]]]
		}
		set result [<h2> "About Wub: All known $suffix documentation [<a> href .. (Parent)]"]
		set l {}
		foreach n $s {
		    set v [lindex $API($n) 0]
		    set v [string trim $v " \t\n"]
		    set v [::textutil::untabify $v]
		    set v [::textutil::undent $v]
		    set v [string trim $v \n]
		    set v [lindex [split $v \n] 0]

		    set n [file join $mount $n]
		    lappend l [<li> "[<a> href $n [file tail $n]] -- $v"]
		}
		append result \n [<ul> [join $l]]
		set r [Html style $r /css/style.css]
		return [Http NotFound $r $result x-text/html-fragment]
	    }

	    set result ""
	    set n $suffix 
	    set v $API($n)
	    
	    set opts [lassign $v v]
	    if {[string index $v 0] eq "\n"} {
		set v [string trim $v "\n"]
		set v [::textutil::untabify $v]
		set v [::textutil::undent $v]
		
		set optlist {}

		set doc $v
		if {[llength $opts]} {
		    append doc "== Constructor Options ==" \n
		    foreach {var txt} $opts {
			append doc ";" $var : $txt \n
		    }
		}

		if {![catch {
		    stx2html::translate "== About Wub: $n ==\n\[. (Parent)\]\n\n$doc\n(generated by \[../../doc/Utilities/stx.stx STX\])"
		} res eo]} {
		    append result $res \n
		} else {
		    #puts stderr "API ERR: $res ($eo) ($doc)"
		    append result [<h2> $n] \n [<p> [<a> href .. (Parent)]] [<p> $v] \n
		}
	    } else {
		append result [<h2> "About Wub: Domain [file tail $n]"] \n [<p> [<a> href .. (Parent)]] \n [<p> $v] \n
	    }
	    set r [Html style $r /css/style.css]
	    return [Http Ok $r $result x-text/html-fragment]
	}
	return [do $r]
    }

    proc new {args} {
	dict for {n v} $args {
	    variable $n $v
	}
	return ::About
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
