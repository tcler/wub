# RAM.tcl - return contents of array as a domain

package require Http
package require Debug

package provide RAM 1.0

Debug off RAM 10

namespace eval RAM {
    variable ram
    variable content_type x-text/html-fragment

    # called as "do $request" returns the value stored in this RAM to be returned.
    proc _do {prefix rsp} {

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
	Debug.RAM {exists ram $prefix$suffix [info exists ram($prefix$suffix)]}
	if {![info exists ram($prefix$suffix)]} {
	    # path isn't inside our domain suffix - error
	    return [Http NotFound $rsp]
	}

	variable content_type
	set content [lindex $ram($prefix$suffix) 0]
	set els {} 
	foreach {el val} [lrange $ram($prefix$suffix) 1 end] {
	    if {$el eq "-header"} {
		dict lappend $rsp -headers $val
	    } else {
		dict set els $el $val
	    }
	}
	set rsp [dict merge $rsp [list content-type $content_type {*}$els -content $content]]

	return [Http Ok $rsp]
    }

    # _get - gets keyed content only
    proc _get {prefix key} {
	variable ram
	return [lindex $ram($prefix$key) 0]
    }

    # _set - assumes first arg is content, rest are to be merged
    proc _set {prefix key args} {
	variable ram

	if {$args ne {}} {
	    # calculate an accurate content length
	    lappend args content-length [string length [lindex $args 0]]
	    set ram($prefix$key) $args
	}

	# fetch ram for prefix
	return $ram($prefix$key)
    }

    # initialize view ensemble for RAM
    proc init {cmd prefix args} {
	if {$args ne {}} {
	    variable {*}$args
	}
	set cmd [uplevel 1 namespace current]::$cmd
	namespace ensemble create \
	    -command $cmd -subcommands {} \
	    -map [subst {
		set "_set $prefix"
		get "_get $prefix"
		do "_do $prefix"
	    }]

	return $cmd
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
