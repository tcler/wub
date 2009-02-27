# About - a domain for documenting other domains via their API entries
package require Debug
Debug on about 10

package require textutil
package provide About 1.0

set API(About) {
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
	    # compute suffix
	    variable mount
	    if {[dict exists $r -suffix]} {
		# caller has munged path already
		set suffix [dict get $r -suffix]
		Debug.about {-suffix given $suffix}
	    } else {
		# assume we've been parsed by package Url
		# remove the specified prefix from path, giving suffix
		set path [dict get $r -path]
		set suffix [Url pstrip $mount $path]
		Debug.about {-suffix not given - calculated '$suffix' from '$mount' and '$path'}
		if {($suffix ne "/") && [string match "/*" $suffix]} {
		    # path isn't inside our domain suffix - error
		    return [Http NotFound $r]
		}
		dict set r -suffix $suffix
	    }
	    
	    global API
	    if {![info exists API($suffix)]} {
		Debug.about {$suffix domain not known}
		if {$suffix ne "/"} {
		    set result [<message> "$suffix domain not known"]
		} else {
		    set result [<h2> "All known Domains"]
		}
		set l {}
		foreach n [lsort -dictionary [array names API]] {
		    append l [<li> [<a> href $n $n]]
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
