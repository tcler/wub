# Direct.tcl - direct domain handler, like tclhttpd's
#
# Provides wildcard dispatch, /photo/1234 handled by photo::/default

package require Query
package require Debug

package provide Direct 1.0
Debug off direct 10

namespace eval Direct {
    proc _do {ns ctype response} {
	Debug.direct {do direct}
	# get query dict
	set qd [Query parse $response]
	dict set response -Query $qd

	set fn [file rootname [dict get $response -suffix]]	;# remove extension
	set cmd ${ns}::/[string trimleft [armour $fn] /]
	if {[info procs $cmd] eq {}} {
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
	#puts stderr "RAAAR: '$cmd' '$response' '$argl'"
	set response [dict merge $response [$cmd $response {*}$argl]]

	#Debug.direct {Content: '[dict get $response -content]'} 2
	return $response
    }

    proc init {cmd args} {
	set ctype "text/html"
	foreach {n v} $args {
	    set $n $v
	}

	set cmd [uplevel 1 namespace current]::$cmd

	namespace ensemble create \
	    -command $cmd -subcommands {}
	    -map [subst {
		do "_do $ctype $namespace"
	    }]

	return $cmd
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
