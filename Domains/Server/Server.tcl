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
		return [Http Ok $r [<p> "Turn Timestamping [<a> href ./timing?on=0 Off]"]]
	    } else {
		return [Http Ok $r [<p> "Turn Timestamping [<a> href ./timing?on=1 On]"]]
	    }
	}

	if {!$on} {
	    catch {unset ::Httpd::timers}
	    return [Http Ok $r [<p> "Turn Timestamping [<a> href ./timing?on=1 On]"]]
	}

	set ext [file extension [dict get $r -path]]

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

	if {$ext eq ".csv"} {
	    package require csv
	    set result [::csv::join [dict keys $headers]]\n
	    dict for {n v} $rdict {
		set line {}
		foreach key [dict keys $headers] {
		    lappend line [dict get $v $key]
		}
		append result [::csv::join $line] \n
	    }
	    return [Http Ok $r $result text/csv]
	}

	set result [Report html $rdict headers [dict keys $headers] {*}{
	    sortable 1
	    evenodd 1
	}]
	append result [<p> "Turn Timestamping [<a> href ./timing?on=0 Off]"]
	append result [<p> "Deliver Content in [<a> href ./timing.csv "CSV Format"]"]
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
