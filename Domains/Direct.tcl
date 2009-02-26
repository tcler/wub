# Direct.tcl - direct domain handler, like tclhttpd's
#
# Provides wildcard dispatch, /photo/1234 handled by photo::/default
#
# Examples:
#  1) Create a Direct domain handler (fred) which will interpret URLs 
#  with a prefix /fred/ as proc calls within a namespace ::Fred
#
#   Direct fred namespace ::Fred prefix /fred/
#
#  2) Using [fred] domain handler to interpret a request $r:
#
#  fred do $r

package require TclOO
namespace import oo::*

package require Query
package require Url
package require Debug
Debug off direct 10

package provide Direct 1.0

set API(Direct) {
    {direct domain handler, which dispatches URL requests to commands within a given namespace}
    namespace {namespace in which to invoke commands}
    ctype {default content-type of returned values}
    wildcard {process to be used if a request doesn't match any proc in $namespace (default /default)}
    trim {a prefix which must be found at the beginning of, and will be removed from, the in-domain path component of any URL (default: none) not terribly useful.}
}

class create Direct {
    variable namespace object class ctype mount wildcard trim methods

    method prefix {path args} {
	set path [file split $path]
	
    }

    method do_ns {rsp} {
	Debug.direct {do direct $namespace $mount $ctype}
	
	# search for a matching command prefix
	set cmd ""
	set fn [dict get $rsp -suffix]; if {$fn eq ""} {set fn /}
	set cprefix [file split [armour $fn]]
	set extra {}
	while {$cmd eq "" && [llength $cprefix]} { 
	    set probe [info commands ${namespace}::/[string trim [join $cprefix /] /]]
	    if {[llength $probe] == 1} {
		set cmd [lindex $probe 0]
		break
	    }

	    # there's no exact match, so trim cprefix and try again.
	    Debug.direct {searching for ($cprefix) in '$namespace' among $probe}
	    lappend extra [lindex $cprefix end]		;# remember trailing part of url
	    set cprefix [lrange $cprefix 0 end-1]	;# trim url and try again
	}

	# no match - use wildcard proc
	if {$cmd eq ""} {
	    Debug.direct {no match looking for '$fn' in '$namespace' ([info procs ${namespace}::/*])}
	    set cmd ${namespace}::$wildcard
	    if {[info commands $cmd] eq {}} {
		Debug.direct {default not found looking for $cmd in ([info procs ${namespace}::/*])}
		return [Http NotFound $rsp]
	    }
	} else {
	    dict set rsp -extra [file join [lreverse $extra]]	;# record the extra parts of the domain
	}

	set params [lrange [info args $cmd] 1 end]
	array set used {}
	set needargs 0
	set argl {}
	set qd [dict get $rsp -Query]

	Debug.direct {cmd: '$cmd' params:$params [dict keys $qd]}
	foreach arg $params {
	    if {[Query exists $qd $arg]} {
		Debug.direct {param $arg exists} 2
		incr used($arg)
		if {[Query numvalues $qd $arg] > 1} {
		    Debug.direct {multiple $arg: [Query values $qd $arg]} 2
		    lappend argl [Query values $qd $arg]
		} else {
		    Debug.direct {single $arg: [string range [Query value $qd $arg] 0 80]...} 2
		    lappend argl [Query value $qd $arg]
		}
	    } elseif {$arg eq "args"} {
		set needargs 1
	    } else {
		Debug.direct {param '$arg' does not exist} 2
		if {[info default $cmd $arg value]} {
		    Debug.direct {default $arg: $value} 2
		    lappend argl $value
		} else {
		    lappend argl {}
		}
	    }
	}

	set argll {}
	if {$needargs} {
	    foreach {name value} [Query flatten $qd] {
		if {![info exists used($name)]} {
		    Debug.direct {args $name: [string range $value 0 80]...} 2
		    lappend argll $name $value
		}
	    }
	}

	Debug.direct {calling $cmd [string range $argl 0 80]... [dict keys $argll]} 2
	if {[catch {
	    dict merge $rsp [$cmd $rsp {*}$argl {*}$argll]
	} result eo]} {
	    Debug.direct {error: $result ($eo)}
	    return [Http ServerError $rsp $result $eo]
	} else {
	    Debug.direct {Content: [dict get $result -code] '[string range [dict get $result -content] 0 80]...'} 2
	    return $result
	}
    }

    # locate a matching direct method in an object
    method do_obj {rsp} {
	Debug.direct {do direct $object $mount $ctype}
	
	# search for a matching command prefix
	set fn [dict get $rsp -suffix]
	set cprefix [file split [armour $fn]]
	set extra {}
	set cmd ""
	while {$cmd eq "" && [llength $cprefix]} { 
	    Debug.direct {searching for ($cprefix) in '$object'}
	    set probe [dict keys $methods /[join $cprefix /]]
	    # this strict match can only have 1 or 0 results
	    if {[llength $probe] == 1} {
		set cmd $probe
		break
	    }

	    # there's no exact match, so trim cprefix and try again.
	    lappend extra [lindex $cprefix end]	;# remember the non-matching bits
	    set cprefix [lrange $cprefix 0 end-1]
	}

	# no match - use wildcard method
	if {$cmd eq ""} {
	    Debug.direct {$cmd not found looking for $fn in '$object' ($methods)}
	    set cmd $wildcard
	    if {![dict exists $methods $cmd] eq {}} {
		Debug.direct {default not found looking for $cmd in ($methods)}
		return [Http NotFound $rsp]
	    }
	} else {
	    dict set rsp -extra [file join [lreverse $extra]]	;# record the extra parts of the domain
	}

	# get the formal parameters of the method
	lassign [info class definition [info object class $object] $cmd] def
	if {[lindex $def end] eq "args"} {
	    set needargs 1
	    set params [lrange $def 1 end-1]	;# remove args from params
	} else {
	    set needargs 0
	    set params [lrange $def 1 end]
	}

	set qd [dict get $rsp -Query]
	Debug.direct {cmd: '$cmd' params:$params [dict keys $qd]}

	set argl {}
	array set used {}
	foreach arg $params {
	    lassign $arg arg default
	    if {[Query exists $qd $arg]} {
		Debug.direct {param $arg exists} 2
		incr used($arg)
		if {[Query numvalues $qd $arg] > 1} {
		    Debug.direct {multiple $arg: [Query values $qd $arg]} 2
		    lappend argl [Query values $qd $arg]
		} else {
		    Debug.direct {single $arg: [string range [Query value $qd $arg] 0 80]...} 2
		    lappend argl [Query value $qd $arg]
		}
	    } else {
		Debug.direct {param '$arg' does not exist} 2
		lappend argl $default
	    }
	}

	set argll {}
	if {$needargs} {
	    foreach {name value} [Query flatten $qd] {
		if {![info exists used($name)]} {
		    Debug.direct {args $name: [string range $value 0 80]...} 2
		    lappend argll $name $value
		}
	    }
	}

	Debug.direct {calling method $cmd [string range $argl 0 80]... [dict keys $argll]} 2
	if {[catch {
	    dict merge $rsp [$object $cmd $rsp {*}$argl {*}$argll]
	} result eo]} {
	    Debug.direct {error: $result ($eo)}
	    return [Http ServerError $rsp $result $eo]
	} else {
	    Debug.direct {Content: [dict get $result -code] '[string range [dict get $result -content] 0 80]...'} 2
	    return $result
	}
    }

    # called as "do $request" causes procs defined within 
    # the specified namespace to be invoked, with the request as an argument,
    # expecting a response result.
    method do {r} {
	# get query dict
	set qd [Query parse $r]
	dict set r -Query $qd
	Debug.direct {Query: [Query dump $qd]}

	# remember which mount we're using - this allows several
	# domains to share the same namespace, differentiating by
	# reference to -prefix value.
	dict set r -prefix $mount

	if {[dict exists $r -suffix]} {
	    # caller has munged path already
	    set suffix [dict get $r -suffix]
	    Debug.direct {-suffix given $suffix}
	} else {
	    # assume we've been parsed by package Url
	    # remove the specified mount from path, giving suffix
	    set path [file rootname [dict get $r -path]]
	    set suffix [Url pstrip $mount $path]
	    Debug.direct {-suffix not given - calculated '$suffix' from '$mount' and '$path'}
	    if {($suffix ne "/") && [string match "/*" $suffix]} {
		# path isn't inside our domain suffix - error
		return [Http NotFound $r]
	    }
	}
	
	# record the suffix's extension
	dict set r -extension [file extension $suffix]
	
	# remove suffix's extension and trim /s
	set fn [string trim [file rootname $suffix] /]
	if {[info exists trim] && $trim ne ""} {
	    if {[string match $trim* $fn]} {
		set fn [string range $fn [string length $trim] end]
	    } else {
		return [Http NotFound $r]
	    }
	}
	dict set r -suffix $fn

	# TODO: armour commands
	dict set r content-type $ctype
	#dict set r cache-control no-cache	;# default - not cacheable
	dict set r -dynamic 1
	catch {dict unset r -content}

	if {[info exists object]} {
	    return [my do_obj $r]
	} else {
	    return [my do_ns $r]
	}
    }

    # init cmd {namespace ns} {ctype default-mime-type} {mount mount-of-domain}
    # creates cmd command proxy/interface to procs within $ns called /name,
    # which will be invoked with Query args from the request assigned to actual
    # parameters of matching proc, procs are matched from the -path or -suffix
    # components of the passed request
    constructor {args} {
	set ctype "text/html"
	set mount "/"
	set wildcard /default

	foreach {n v} $args {
	    set [string trimleft $n -] $v
	    Debug.direct {variable: $n $v}
	}

	# one or the other of namespace or object must exist
	if {![info exist namespace] && ![info exists object]} {
	    error "Direct domain must specify namespace or object"
	} elseif {[info exists object]} {
	    if {[info exists namespace]} {
		error "Direct domain: can only specify one of object or namespace"
	    }

	    if {[llength $object] == 1} {
		# object name must be fully ns-qualified
		if {![string match "::*" $object]} {
		    set object ::$object
		}
	    } elseif {[llength $object]%2} {
		Debug.direct {[lindex $object 0] new {*}[lrange $object 1 end] mount $mount}
		set object [[lindex $object 0] new {*}[lrange $object 1 end] mount $mount]
	    } else {
		Debug.direct {[lindex $object 0] new {*}[lrange $object 1 end] mount $mount}
		set object [[lindex $object 0] create {*}[lrange $object 1 end] mount $mount]
	    }

	    foreach m [lreverse [lsort -dictionary [info object methods $object -private -all]]] {
		if {[string match /* $m]} {
		    dict set methods $m {}
		}
	    }
	    objdefine $object export {*}[info object methods $object -all] {*}[dict keys $methods]

	} else {
	    # namespace must be fully qualified
	    if {![string match "::*" $namespace]} {
		set namespace ::$namespace
	    }
	}
	set wildcard /[string trim $wildcard /]
    }
}
