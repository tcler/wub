# Wub module for Woof
package require Debug
Debug on woof 10

package provide Woof 1.0

class create ::Woof {

    # call woof to process URL
    method do {r} {
	# calculate the suffix of the URL relative to $mount
	lassign [Url urlsuffix $r $mount] result r suffix path
	if {!$result} {
	    return $r	;# the URL isn't in our domain
	}
	Debug.woof {Woof do [self] $suffix $path}

	::woof::handle_request $r
	dict set r -passthrough 1	;# response is generated already
	return $r
    }

    variable mount
    constructor {args} {
	Debug.woof {Woof constructed [self] $args}
	foreach {n v} $args {
	    set [string trimleft $n -] $v
	}
	set mount /[string trim $mount /]/
    }
}

if {0} {
    ::woof::init wub_server	;# start up woof

    # construct a nub for Woof
    set wmp /[string trim [::woof::config get url_root] /]/
    Nub domain $wmp Woof
}
