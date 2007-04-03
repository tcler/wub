package provide Debug 2.0

namespace eval Debug {
    variable detail
    variable level 0

    proc noop {args} {}

    proc debug {tag message {level 1}} {
	variable detail
	if {$detail($tag) >= $level} {
	    upvar 1 self self
	    set code [catch {
		uplevel 1 subst [list $message]
	    } result eo]
	    if {$code} {
		puts -nonewline stderr "@@(DebugError from [info level -1] ($eo)):"
	    }

	    if {[info exists self]} {
		puts stderr "$tag @@$self: $result"
	    } else {
		puts stderr "$tag @@$result"
	    }

	} else {
	    #puts stderr "$tag @@@ $detail($tag) >= $level"
	}
    }

    proc names {} {
	variable detail
	return [lsort [array names detail]]
    }
    proc 2array {} {
	variable detail
	set result {}
	foreach n [lsort [array names detail]] {
	    lappend result $n $detail($n)
	}
	return $result
    }

    proc level {tag {level ""}} {
	variable detail
	if {$level ne ""} {
	    set detail($tag) $level
	}

	if {![info exists detail($tag)]} {
	    set detail($tag) 1
	}
	return $detail($tag)
    }

    proc on {tag {level ""}} {
	level $tag $level
	interp alias {} Debug.$tag {} Debug::debug $tag
    }
    proc off {tag {level ""}} {
	level $tag $level
	interp alias {} Debug.$tag {} Debug::noop
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

Debug off admin
Debug off cache
Debug off convert
Debug off cookies
Debug off db
Debug off dblayout
Debug off dispatch
Debug off file
Debug off home
Debug off host
Debug off http
Debug off mason
Debug off mime
Debug off session
Debug off smtp
Debug off snitdomain
Debug off socket
Debug off sql
Debug off timer
Debug off url
Debug off vfs
Debug off wiki
Debug off wikiload
Debug off wikisearch
