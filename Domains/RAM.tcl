# RAM.tcl - return contents of array as a domain

package require Http
package require TclOO
namespace import oo::*

package require Debug
Debug off RAM 10

package provide RAM 2.0

class create RAM {
    variable ram prefix content_type

    # get - gets keyed content only
    method get {key} {
	Debug.RAM {[self] get $key '$ram($key)'}
	return [lindex $ram($key) 0]
    }

    # exists - does keyed content exist?
    method exists {key} {
	Debug.RAM {exists $key '[info exists ram($key)]'}
	return [info exists ram($key)]
    }

    # _set - assumes first arg is content, rest are to be merged
    method set {key args} {
	if {$args ne {}} {
	    # calculate an accurate content length
	    set now [clock seconds]
	    lappend args -modified $now
	    lappend args last-modified [Http Date $now]
	    lappend args content-length [string length [lindex $args 0]]
	    Debug.RAM {$key set '$args'}
	    set ram($key) $args
	}

	# fetch ram for prefix
	return $ram($key)
    }

    # unset - remove content
    method unset {key} {
	# unset ram element
	Debug.RAM {unset $key '$ram($key)'}
	unset ram($key)
    }

    method keys {prefix} {
	set result {}
	return [array names ram]
    }

    # called as "do $request" returns the value stored in this RAM to be returned.
    method do {rsp} {
	# compute suffix
	if {[dict exists $rsp -suffix]} {
	    # caller has munged path already
	    set suffix [dict get $rsp -suffix]
	    Debug.RAM {-suffix given $suffix}
	} else {
	    # assume we've been parsed by package Url
	    # remove the specified prefix from path, giving suffix
	    set path [dict get $rsp -path]
	    set suffix [Url pstrip $prefix $path]
	    Debug.RAM {-suffix not given - calculated '$suffix' from '$prefix' and '$path'}
	    if {($suffix ne "/") && [string match "/*" $suffix]} {
		# path isn't inside our domain suffix - error
		return [Http NotFound $rsp]
	    }
	}

	variable ram
	Debug.RAM {exists ram $suffix [info exists ram($suffix)]}
	if {![info exists ram($suffix)]} {
	    # path isn't inside our domain suffix - error
	    return [Http NotFound $rsp]
	}

	variable content_type
	set content [lindex $ram($suffix) 0]
	set els {}
	set extra [lrange $ram($suffix) 1 end]

	# check conditional
	if {[dict exists $rsp if-modified-since]
	    && (![dict exists $extra -dynamic] || ![dict get $extra -dynamic])
	} {
	    set since [Http DateInSeconds [dict get $rsp if-modified-since]]
	    if {[dict get $extra -modified] <= $since} {
		Debug.RAM {NotModified: $path - [dict get $extra last-modified] < [dict get $extra if-modified-since]}
		Debug.RAM {if-modified-since: not modified}
		return [Http NotModified $rsp]
	    }
	}
	
	foreach {el val} $extra {
	    if {$el eq "-header"} {
		dict lappend $rsp -headers $val
	    } else {
		dict set els $el $val
	    }
	}
	set rsp [dict merge $rsp [list content-type $content_type {*}$els -content $content]]

	return [Http Ok $rsp]
    }

    # initialize view ensemble for RAM
    constructor {args} {
	set content_type x-text/html-fragment
	set prefix /ram/
	array set ram {}
	foreach {n v} $args {
	    set n [string trim $n -]
	    set $n $v
	}
	set prefix /[string trim $prefix /]/
    }
}
