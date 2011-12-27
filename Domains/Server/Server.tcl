# Server - a domain to provide Server introspection

# add the following to site.config to install this:
# /server {
#	domain Server
#	url /server
# }

package require Debug

package require Report
package require csv

package provide Server 1.0

oo::class create ::Server {

    method /timing {r {on 1}} {
	set r [Http NoCache $r]
	if {![info exists ::Httpd::timers]} {
	    if {$on} {
		set ::Httpd::timers {}
	    } else {
		return [Http Ok $r [<p> "Turn Timestamping [<a> href ./timing?on=1 On]"]]
	    }
	}

	if {!$on} {
	    catch {unset ::Httpd::timers}
	    return [Http Ok $r [<p> "Turn Timestamping [<a> href ./timing?on=1 On]"]]
	}

	# collect headers
	set h {pipeline transaction ipaddr url}
	foreach hh $h {
	    dict set headers $hh 1
	}

	set timers $::Httpd::timers
	set rdict {}
	foreach pipeline [lsort -integer [dict keys $timers]] {
	    foreach transaction [lsort -integer [dict keys [dict get $timers $pipeline]]] {
		set tdict [lassign [dict get $timers $pipeline $transaction] ipaddr url]
		foreach k [dict keys $tdict] {
		    dict set headers $k 1
		}
		dict set rdict $pipeline.$transaction $tdict
		foreach f $h {
		    dict set rdict $pipeline.$transaction $f [set $f]
		}
	    }
	}
	set result [Report html $rdict headers [dict keys $headers] {*}{
	    sortable 1
	    evenodd 1
	}]
	append result [<p> "Turn Timestamping [<a> href ./timing?on=0 Off]"]
	return [Http Ok $r $result]
    }

    method / {r} {
	return [my /timing $r on]
    }

    mixin Direct
    constructor {args} {
	next? {*}$args
    }
}
