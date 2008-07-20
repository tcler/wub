# Rest - store apply exprs in a random temporary URL
# 
# store a lambda in a temporary store addressable 
# by a generated random Url.
#
# The lambda gets the current request and query args, like Direct
# it returns a completed dict fragment which is merged with
# the request dict and returned as the response.
#
# Prior to the lambda application, the environment dict passed in
# at emit-construction time is merged into the request dict.
#
# The return dict may also set an -env field in the request dict
# which becomes the environment for the next invocation (if any).
#
# After each invocation, a -count field (by default, 1) is decremented
# The lambda is removed when -count falls to 0.

package require Debug
Debug off rest 10
package require RAM
package require Url

package provide Rest 1.0

namespace eval Rest {

    proc do {rsp} {
	# compute suffix
	if {[dict exists $rsp -suffix]} {
	    # caller has munged path already
	    set suffix [dict get $rsp -suffix]
	    Debug.rest {-suffix given $suffix}
	} else {
	    # assume we've been parsed by package Url
	    # remove the specified prefix from path, giving suffix
	    set path [dict get $rsp -path]
	    set suffix [Url pstrip $prefix $path]
	    Debug.rest {-suffix not given - calculated '$suffix' from '$prefix' and '$path'}
	    if {($suffix ne "/") && [string match "/*" $suffix]} {
		# path isn't inside our domain suffix - error
		return [Http NotFound $rsp]
	    }
	}

	Debug.rest {exists $suffix [cstore exists $suffix]}
	if {![cstore exists $suffix]} {
	    # path isn't inside our domain suffix - error
	    return [Http NotFound $rsp]
	}

	# get query dict
	set qd [Query parse $rsp]
	dict set rsp -Query $qd
	Debug.direct {Query: [Query dump $qd]}

	# fetch the rest content
	set env [lassign [cstore set $suffix] apply]
	set org_env $env
	dict set rsp -env $env	;# store the complete environment

	# unpack the apply
	lassign $apply params body ns

	# construct a dummy proc
	proc dummy $params {}

	array set used {}
	set needargs 0
	set argl {}
	Debug.rest {params:$params ([dict keys $qd])}
	foreach arg $params {
	    if {[Query exists $qd $arg]} {
		Debug.rest {param $arg exists} 2
		incr used($arg)
		if {[Query numvalues $qd $arg] > 1} {
		    Debug.rest {multiple $arg: [Query values $qd $arg]} 2
		    lappend argl [Query values $qd $arg]
		} else {
		    Debug.rest {single $arg: [string range [Query value $qd $arg] 0 80]...} 2
		    lappend argl [Query value $qd $arg]
		}
	    } elseif {$arg eq "args"} {
		set needargs 1
	    } else {
		Debug.rest {param '$arg' does not exist} 2
		if {[info default dummy $arg value]} {
		    Debug.rest {default $arg: $value} 2
		    lappend argl $value
		} else {
		    lappend argl {}
		}
	    }
	}

	# collect extra args if needed
	set argll {}
	if {$needargs} {
	    foreach {name value} [Query flatten $qd] {
		if {![info exists used($name)]} {
		    Debug.rest {args $name: [string range $value 0 80]...} 2
		    lappend argll $name $value
		}
	    }
	}

	dict set rsp -dynamic 1

	catch {dict unset rsp -content}
	Debug.rest {applying $apply [string range $argl 0 80]... [dict keys $argll]} 2
	if {[catch {
	    ::apply $apply [dict merge $rsp $env] {*}$argl {*}$argll
	} result eo]} {
	    Debug.rest {error: $result ($eo)}
	    return [Http ServerError $rsp $result $eo]
	} else {
	    Debug.rest {Content: [dict get $result -code] '[string range [dict get $result -content] 0 80]...'} 2
	    if {[dict exists $result -env]} {
		set env [dict get $result -env]
		dict unset result -env
	    }

	    if {[dict exists $env -count]
		&& [dict env incr -count -1] <= 0
	    } {
		# we need to remove the content
		cstore unset $suffix
	    } elseif {$env ne $org_env} {
		# refresh stored environment
		cstore set $suffix $apply {*}$env
	    }

	    # merge the result into the response 
	    return [dict merge $rsp $result]
	}
    }

    variable mount "/_c/"

    # emit - construct a temporary with the apply and environment
    # -count determines for how many calls the temporary is valid.
    proc emit {rsp apply args} {
	# find a (currently-)unique random key
	set key [clock microseconds][expr {int(rand() * 10000)}]
	while {[cstore exists $key]} {
	    set key [clock microseconds][expr {int(rand() * 10000)}]
	}

	# ensure there's a -count environment field
	if {![dict exists $args -count]} {
	    dict set args -count 1
	}

	# store the apply and its args in a temp store
	cstore set $key $apply {*}$args

	# generate a Url to the temp content
	return [Url redir $r [file join $mount $key]]
    }

    # make this Url persist for another use
    proc again {r} {
	set env [dict get $r -env]
	dict incr env -count 1
	dict set r -env $env
	return $r
    }

    proc init {args} {
	if {$args ne {}} {
	    variable {*}$args
	}
	variable mount
	RAM init cstore $mount
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
