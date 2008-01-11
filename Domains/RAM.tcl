# RAM.tcl - return contents of array as a domain

package require Http
package require Debug

package provide RAM 1.0

Debug off RAM 10

namespace eval RAM {
    variable ram

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
	    set suffix [Url pstrip [file split $prefix] $path]
	    Debug.RAM {-suffix not given - calculated '$suffix' from '$prefix' and '$path'}
	    if {[string match "/*" $suffix]} {
		# path isn't inside our domain suffix - error
		return [Http NotFound $rsp]
	    }
	}

	variable ram
	Debug.RAM {exists ram $prefix$suffix [info exists ram($prefix$suffix)]}
	set rsp [dict merge $rsp [list -content {*}$ram($prefix$suffix)]]

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
	set cmd [uplevel 1 namespace current]::$cmd

	namespace ensemble create \
	    -command $cmd -subcommands {} \
	    -map [subst {
		set "_set $prefix"
		get "_get $prefix"
		do "_do $prefix"
	    }]

	foreach {n v} $args {
	    if {[catch {
		_set $prefix $n {*}$v
	    } r eo]} {
		Debug.error {RAM domain $cmd - key '$n' value: '$v', $r ($eo)}
	    }
	}

	return $cmd
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
