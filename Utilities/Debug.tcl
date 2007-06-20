package provide Debug 2.0

namespace eval Debug {
    variable detail
    variable level 0
    variable fds

    proc noop {args} {}

    proc debug {tag message {level 1}} {
	variable detail
	if {$detail($tag) >= $level} {
	    variable fds
	    set fd $fds($tag)

	    upvar 1 self self
	    set code [catch {
		uplevel 1 subst [list $message]
	    } result eo]
	    if {$code} {
		puts -nonewline $fd "@@[string map {\n \\n} (DebugError from [info level -1] ($eo)):]"
	    }

	    if {[info exists self]} {
		puts $fd "$tag @@$self: [string map {\n \\n} $result]"
	    } else {
		puts $fd "$tag @@[string map {\n \\n} $result]"
	    }

	} else {
	    #puts stderr "$tag @@@ $detail($tag) >= $level"
	}
    }

    # names - return names of debug tags
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

    # level - set level and fd for tag
    proc level {tag {level ""} {fd stderr}} {
	variable detail
	if {$level ne ""} {
	    set detail($tag) $level
	}

	if {![info exists detail($tag)]} {
	    set detail($tag) 1
	}

	variable fds
	set fds($tag) $fd

	return $detail($tag)
    }

    # turn on debugging for tag
    proc on {tag {level ""} {fd stderr}} {
	level $tag $level $fd
	interp alias {} Debug.$tag {} Debug::debug $tag
    }

    # turn off debugging for tag
    proc off {tag {level ""} {fd stderr}} {
	level $tag $level $fd
	interp alias {} Debug.$tag {} Debug::noop
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

Debug on error 100
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
Debug off activity
