# Direct.tcl - direct domain handler, like tclhttpd's
#
# TODO: wildcard dispatch, /photo/1234 handled by photo::/*, etc.
#  consider using the namespace unknown better for this.

package require Query
package require Debug
package require snit

package provide Direct 1.0
Debug off direct 10

proc page {name alist page {body ""} {ctype "text/html"}} {
    set whole "upvar request request\n"
    append whole "set ctype $ctype" \n
    append whole "set page [list $page]" \n
    append whole $body \n
    append whole "dict set request content-type \$ctype" \n
    append whole "return \[subst \$page\]" \n
    uplevel 1 [list proc $name $alist $whole]
}

::snit::type Direct {
    option -namespace ""
    option -ctype "text/html"

    method do {response} {
	Debug.direct {do direct}
	# get query dict
	set qd [Query parse $response]
	dict set response -Query $qd

	set ns $options(-namespace)
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
	dict set response content-type $options(-ctype)
	#dict set response cache-control no-cache	;# default - not cacheable
	dict set response -dynamic 1

	catch {dict unset response -content}
	Debug.direct {calling $cmd $response $argl} 2
	#puts stderr "RAAAR: '$cmd' '$response' '$argl'"
	set response [dict merge $response [$cmd $response {*}$argl]]

	#Debug.direct {Content: '[dict get $response -content]'} 2
	return $response
    }

    constructor {args} {
	Debug.direct {Direct constructor: $args}
	$self configurelist $args
	if {$options(-namespace) eq ""} {
	    error "-namespace must be specified"
	}
    }
}

namespace eval DirectTest {
    proc retval {} {
	return "<html><body>[info level -1]</body></html>"
    }
    
    proc /a {args} {
	return [retval]
    }
    proc /a/b1 {x y} {
	return [retval]
    }
    proc /a/b2 {x {y y-default}} {
	return [retval]
    }

    page /page {args} {
	<h1> Test Page Generator </h1>
	<p>Time: [clock scan now]</p>
	<p>Count: $counter</p>
	<p>Args: $args</p>
    } {
	variable counter; incr counter
    }
}
