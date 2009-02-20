# Tub is a direct domain which allows storage of arbitrary form data

package require TclOO
namespace import oo::*

set API(Tub) {
    {Tub is a domain for storage and retrieval of arbitrary form data}
    view {View within which to store data (no default)}
    key {key within view which uniquely identifies store (default "user")}
    realm {Realm used in password challenges}
    cookie {cookie for storing key - not implemented}
    permissive {boolean - will storing under a new key create record?}
}

package require Debug
Debug on tub 10

package require Direct
package require View
package require Cookies

package provide Tub 1.0

class create Tub {
    variable view realm key properties cookie permissive
    
    # get the data given by key
    method get {r k} {
	if {[catch {$view find $key $k} index eo]} {
	    return [Http NotFound $r]
	}
	set record [$view get $index]

	# handle conditional request
	if {[dict exists $record timestamp]} {
	    set when [dict get $record timestamp]
	    if {[dict exists $r if-modified-since]} {
		set since [Http DateInSeconds [dict get $r if-modified-since]]
		if {$when <= $since} {
		    Debug.file {NotModified: $path - [Http Date [file mtime $path]] < [dict get $r if-modified-since]}
		    Debug.file {if-modified-since: not modified}
		    return [Http NotModified $r]
		}
	    }
	}
	
	if {[dict exists $record args]} {
	    set a [dict get $record args]
	    dict unset record args
	    set record [dict merge $record $args]
	}

	# convert dict to json
	set result {}
	dict for {n v} $d {
	    lappend result "\"$n\": \"$v\""
	}
	set result \{[join $result ,\n]\}

	if {[info exists when]} {
	    return [Http CacheableContent $r $result]
	} else {
	    return [Http Ok $R $result]
	}
    }

    method set {r key args} {
	if {[catch {$view find $key $k} index]} {
	    if {$permissive} {
		# create a new element
		set index [$view append $key $k]
	    } else {
		return [Http NotFound $r]
	    }
	}

	set record {}
	foreach p in $properties {
	    if {$p eq "args"} continue
	    if {[info exists $args $p]} {
		dict set record $p [dict get $args $p]
		dict unset args $p
	    }
	    if {$p eq "timestamp"} {
		# record timestamp
		dict set record $p [clock seconds]
	    }
	}

	dict set record args $args
	$view set $index {*}$record
	return [Http Ok $index]
    }

    method /get {r args} {
	if {![info exists $args $key]} {
	    error "No key to Tub."
	}
	set k [dict get $args $key]
	dict unset args $key
	return [my get $r [dict get $args $k] {*}$args]
    }

    method /set {r args} {
	if {![info exists $args $key]} {
	    error "No key to Tub."
	}
	set k [dict get $args $key]
	dict unset args $key
	return [my set $r [dict get $args $k] {*}$args]
    }

    method /getUser {r} {
	lassign [Http Credentials $r] userid pass
	if {[catch {$view find $key $userid} index]
	    || $userid eq ""
	    || $pass eq ""
	    || [$view get $index password] ne $password
	} {
	    return [challenge $r $realm]
	}
	return [my get $r $userid]
    }

    method /setUser {r args} {
	lassign [Http Credentials $r] userid pass
	if {[catch {$view find $key $userid} index]} {
	    if {$permissive && $userid ne "" && $pass ne ""} {
		set index [$view append $key $userid]
	    } else {
		return [challenge $r $realm]
	    }
	}
	if {$userid eq ""
	    || $pass eq ""
	    || [$view get $index password] ne $pass
	} {
	    return [challenge $r $realm]
	}
	return [my set $r $userid {*}$args]
    }

    method /getCookie {r} {
	return [Http NotImplemented]
    }
    method /setCookie {r args} {
	return [Http NotImplemented]
    }


    method do {r} {
	return [$direct do $r]
    }

    constructor {args} {
	set realm "Tub [self] Realm"
	set permissive 0
	set key user
	set cookie tub
	dict for {n v} $args {
	    set $n $v
	}

	if {[llength $view] > 1} {
	    if {[llength $view]%2} {
		set view [View create {*}$view]
	    } else {
		set view [View new {*}$view]
	    }
	}

	set properties [$view properties]
	set properties [lassign [lindex $properties 0] key]

	# this is the means by which we're invoked.
	set direct [Direct new object [self] ctype application/json]
    }
}
