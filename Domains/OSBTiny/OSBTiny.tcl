# OpenStreetBugs redirector: generates a redirect to a specific OSB bug

package require TclOO
namespace import oo::*

package require Debug
Debug define osbtiny 10

package require HTTP
package provide OSBTiny 1.0

set ::API(Domains/OSBTiny) {
    {OpenStreetBugs tinyurl redirector
    }
    apiurl {The OpenStreetBugs API URL}
}

class create ::OSBTiny {
    
    method / {r} {
        variable apiurl
	set suffix [string trim [dict get? $r -suffix] /]
	if {![string is wideinteger -strict $suffix]} {
	    set r [Http title $r "OpenStreetBugs redirector"]
	    return [Http Ok $r "This is OpenStreetBugs redirector. Usage: [string trim [dict get $r -url] /]/bug-id"]
	}
	set V [HTTP run $apiurl [lambda {r id v} {
	    set result [dict get $v -content]
	    regexp {tion><link>([^<]*)</link>} $result -> link
	    set link [string map {&amp; & openstreetbugs.schokokeks.org osmbugs.org} $link]&bugid=$id
	    return [Httpd Resume [Http Redirect $r $link Redirect text/plain]]
	} $r $suffix] get /api/0.1/rssitem?id=$suffix close]

	return [Httpd Suspend $r 100000]
    }

    mixin Direct

    constructor {args} {
	variable mount ""
        variable apiurl http://openstreetbugs.schokokeks.org/
	variable {*}[Site var? OSBTiny]	{*}$args;# allow .ini file to modify defaults
    }
}


