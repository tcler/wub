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
}

class create Direct {
    variable namespace ctype mount wildcard

    # called as "do $request" causes procs defined within 
    # the specified namespace to be invoked, with the request as an argument,
    # expecting a response result.
    method do {rsp} {
	Debug.direct {do direct $namespace $mount $ctype}
	
	# get query dict
	set qd [Query parse $rsp]
	dict set rsp -Query $qd
	Debug.direct {Query: [Query dump $qd]}

	# remember which mount we're using - this allows several
	# domains to share the same namespace, differentiating by
	# reference to -prefix value.
	dict set rsp -prefix $mount

	if {[dict exists $rsp -suffix]} {
	    # caller has munged path already
	    set suffix [dict get $rsp -suffix]
	    Debug.direct {-suffix given $suffix}
	} else {
	    # assume we've been parsed by package Url
	    # remove the specified mount from path, giving suffix
	    set path [file rootname [dict get $rsp -path]]
	    set suffix [Url pstrip $mount $path]
	    Debug.direct {-suffix not given - calculated '$suffix' from '$mount' and '$path'}
	    if {($suffix ne "/") && [string match "/*" $suffix]} {
		# path isn't inside our domain suffix - error
		return [Http NotFound $rsp]
	    }
	}
	
	# record the suffix's extension
	dict set rsp -extension [file extension $suffix]
	
	# remove suffix's extension and trim /s
	set fn [string trim [file rootname $suffix] /]
	dict set rsp -suffix $fn

	set cmd ${namespace}::/[armour $fn]
	if {[info procs $cmd] eq {}} {
	    # no match - use wildcard proc
	    Debug.direct {$cmd not found looking for $fn in '$namespace' ([info procs ${namespace}::/*])}
	    set cmd ${namespace}::/$wildcard
	    if {[info procs $cmd] eq {}} {
		Debug.direct {default not found looking for $cmd in ([info procs ${namespace}::/*])}
		return [Http NotFound $rsp]
	    }
	}

	set params [lrange [info args $cmd] 1 end]
	array set used {}
	set needargs 0
	set argl {}
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

	# TODO: armour commands
	dict set rsp content-type $ctype
	#dict set rsp cache-control no-cache	;# default - not cacheable
	dict set rsp -dynamic 1

	catch {dict unset rsp -content}
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
	}
    }
}
