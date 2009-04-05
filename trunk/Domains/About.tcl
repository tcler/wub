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
		set result [<h2> "All known $suffix documentation"]
		set l {}
		foreach n [lsort -dictionary [array names API [file join $suffix *]]] {
		    append l [<li> [<a> href $n [file tail $n]]]
		}
		append result \n [<ul> [join $l]]
		return [Http NotFound $r $result x-text/html-fragment]
	    }
	    
	    set n $suffix 
	    set v $API($n)
	    
	    set opts [lassign $v v]
	    if {[string index $v 0] eq "\n"} {
		set v [string trim $v "\n"]
		set v [::textutil::untabify $v]
		set v [::textutil::undent $v]
		
		set stxified 0
		if {![catch {
		    stx2html::translate "== $n Domain==\n$v"
		} res eo]} {
		    set v $res
		    set stxified 1
		}
		
		if {!$stxified} {
		    append result "[<h2> "Domain $n"]\n[<p> $v]" \n
		} else {
		    append result $res \n
		}
	    } else {
		append result "[<h2> "Domain $n"]\n[<p> $v]" \n
	    }
	    
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
