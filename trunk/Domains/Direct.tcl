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

package require Query
package require Debug

package provide Direct 1.0
Debug off direct 10

namespace eval Direct {

    # strip off path prefix - from ::fileutil
    proc pstrip {prefix path} {
	# [file split] is used to generate a canonical form for both
	# paths, for easy comparison, and also one which is easy to modify
	# using list commands.
	if {[string equal $prefix $path]} {
	    return ""
	}
	
	set npath [file split $path]
	if {[string match ${prefix}* $npath]} {
	    return [file join {*}[lrange $npath [llength $prefix] end] {}]
	} else {
	    return $path
	}
    }

    # called as "do $request" causes procs defined within 
    # the specified namespace to be invoked, with the request as an argument,
    # expecting a response result.
    proc _do {ns ctype prefix response} {
	Debug.direct {do direct $ns $ctype}

	# get query dict
	set qd [Query parse $response]
	dict set response -Query $qd

	if {[dict exists $response -suffix]} {
	    # caller has munged path already
	    set suffix [dict get $response -suffix]
	} else {
	    # assume we've been parsed by package Url
	    # remove the specified prefix from path, giving suffix
	    set suffix [pstrip $prefix [dict get $response -path]]
	    if {[string match "/*" $suffix]} {
		# path isn't inside our domain suffix - error
		return [Http NotFound $response]
	    }
	}

	# record the suffix's extension
	dict set response -extension [file extension $suffix]

	# remove suffix's extension and trim /s
	set fn [string trim [file rootname $suffix] /]
	dict set response -suffix $fn

	set cmd ${ns}::/[armour $fn]
	if {[info procs $cmd] eq {}} {
	    # no match - use wildcard proc
	    set cmd ${ns}::/default
	    if {[info procs $cmd] eq {}} {
		Debug.direct {$cmd not found looking for $fn ([info procs ${ns}::/*])}
		return [Http NotFound $response]
	    }
	}

	set params [lrange [info args $cmd] 1 end]
	array set used {}
	set needargs 0
	set argl {}
	Debug.direct {cmd:$cmd params:$params ($qd)}
	foreach arg $params {
	    if {[Query exists $qd $arg]} {
		Debug.direct {param $arg exists} 2
		incr used($arg)
		if {[Query numvalues $qd $arg] > 1} {
		    Debug.direct {multiple $arg: [Query values $qd $arg]} 2
		    lappend argl [Query values $qd $arg]
		} else {
		    Debug.direct {single $arg: [Query value $qd $arg]} 2
		    lappend argl [Query value $qd $arg]
		}
	    } else {
		Debug.direct {param $arg does not exist} 2
		if {[info default $cmd $arg value]} {
		    Debug.direct {default $arg: $value} 2
		    lappend argl $value
		} elseif {$arg eq "args"} {
		    set needargs 1
		} else {
		    lappend argl {}
		}
	    }
	}

	if {$needargs} {
	    foreach {name value} [Query flatten $qd] {
		if {![info exists used($name)]} {
		    Debug.direct {args $name: $value} 2
		    lappend argl $name $value
		}
	    }
	}

	# TODO: armour commands
	dict set response content-type $ctype
	#dict set response cache-control no-cache	;# default - not cacheable
	dict set response -dynamic 1

	catch {dict unset response -content}
	Debug.direct {calling $cmd $response $argl} 2
	set response [dict merge $response [$cmd $response {*}$argl]]

	#Debug.direct {Content: '[dict get $response -content]'} 2
	return $response
    }

    # init cmd {namespace ns} {ctype default-mime-type} {prefix prefix-of-domain}
    # creates cmd command proxy/interface to procs within $ns called /name,
    # which will be invoked with Query args from the request assigned to actual
    # parameters of matching proc, procs are matched from the -path or -suffix
    # components of the passed request
    proc init {cmd args} {
	set ctype "text/html"
	set prefix "/"
	foreach {n v} $args {
	    set $n $v
	}

	set cmd [uplevel 1 namespace current]::$cmd

	namespace ensemble create \
	    -command $cmd -subcommands {} \
	    -map [subst {
		do "_do $namespace $ctype [list [file split $prefix]]"
	    }]

	return $cmd
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
