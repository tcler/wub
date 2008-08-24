# Coco.tcl - a domain built around coroutines from tcl8.6

# import the relevant commands
namespace eval ::tcl::unsupported {namespace export coroutine yield}
namespace import ::tcl::unsupported::coroutine ::tcl::unsupported::yield

package require Http
package require Debug
Debug on coco 10

package require md5

package provide Coco 1.0

namespace eval Coco {
    proc <message> {message} {
	return [<p> class message [join $message "</p><p class='message'>"]]
    }

    proc form {_r form args} {
	set _vals {}
	foreach {var vv} $args {
	    set _value ""
	    catch {unset _validate}
	    if {[llength $vv] == 1} {
		dict set _vals $var [lindex $vv 0]
	    } else {
		lassign $vv _msg _validate _value
		dict set _vals $var $_value
		if {[info exists _validate]} {
		    set _validates($var) $_validate
		    set _messages($var) $_msg
		}
	    }
	}
	#puts stderr [array get _validates]
	set _message [list {Enter Fields.}]
	while {[llength $_message]} {
	    set _r [jQ hint $_r]	;# add form hinting

	    # issue form
	    set _r [yield [Http Ok [Http NoCache $_r] [dict with _vals {
		subst [string map [list %MESSAGE [<message> $_message]] $form]
	    }] x-text/html-fragment]]

	    # unpack query response
	    set _Q [Query parse $_r]
	    dict set _r -Query $_Q
	    set _Q [Query flatten $_Q]

	    # fetch and validate fields
	    set _message {}
	    foreach _var [dict keys $_vals] {
		dict set _vals $_var [Dict get? $_Q $_var]
	    }
	    dict with _vals {}

	    Debug.coco {form vals: $_vals}
	    foreach _var [dict keys $_vals] {
		if {[info exists _validates($_var)]} {
		    if $_validates($_var) {
			dict set _r -values $_var [dict get $_vals $_var]
		    } else {
			lappend _message $_messages($_var)
		    }
		}
	    }
	}

	return $_r
    }

    variable uniq [pid]	;# seed for unique coroutine names

    # give a uniq looking name
    proc uniq {} {
	variable uniq
	return [::md5::md5 -hex [incr uniq]]
    }

    # yield wrapper with command dispatcher
    proc yield {{retval ""}} {
	set yield [::yield $retval]
	Debug.coco {yield ($retval) -> ($yield)}
	lassign $yield cmd args
	switch -- $cmd {
	    kill {
		return -code return $args
	    }
	    break {
		return -code break $args
	    }
	    call -
	    default {
		return $args
	    }
	}
    }

    # process request
    proc _do {prefix lambda r} {
	# compute suffix
	if {[dict exists $r -suffix]} {
	    # caller has munged path already
	    set suffix [dict get $r -suffix]
	    Debug.coco {-suffix given $suffix}
	} else {
	    # assume we've been parsed by package Url
	    # remove the specified prefix from path, giving suffix
	    set path [dict get $r -path]
	    set suffix [Url pstrip $prefix $path]
	    Debug.coco {-suffix not given - calculated '$suffix' from '$prefix' and '$path'}
	    if {($suffix ne "/") && [string match "/*" $suffix]} {
		# path isn't inside our domain suffix - error
		return [Http NotFound $r]
	    }
	}

	if {$suffix eq "/"} {
	    # this is a new call - create the coroutine
	    set cmd [uniq]
	    set result [coroutine $cmd ::apply [list {*}$lambda ::Coco] $r]
	    if {$result ne ""} {
		Debug.coco {coroutine initialised - ($r) reply}
		return $result	;# allow coroutine lambda to reply
	    } else {
		# otherwise redirect to coroutine lambda
		Debug.coco {coroutine initialised - redirect to $cmd}
		return [Http Redirect $r $cmd]
	    }
	} elseif {[llength [info command ::Coco::$suffix]]} {
	    # this is an existing coroutine - call it and return result
	    Debug.coco {calling coroutine $suffix}
	    if {[catch {
		$suffix [list call $r]
	    } result eo]} {
		Debug.error {coroutine error: $result ($eo)}
		return [Http ServerError $r $result $eo]
	    }
	    Debug.coco {coroutine yielded: ($result)}
	    return $result
	} else {
	    return [Http NotFound $r]
	}
    }

    # initialize ensemble for Coco
    proc init {cmd prefix lambda args} {
	if {$args ne {}} {
	    variable {*}$args
	}
	set cmd [uplevel 1 namespace current]::$cmd
	namespace ensemble create \
	    -command $cmd -subcommands {} \
	    -map [list do [list _do $prefix $lambda]]
	return $cmd
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
