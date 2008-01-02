# Block - manage a block list of sites

package require fileutil

package provide Block 2.0

namespace eval Block {
    variable blocked; array set blocked {}
    variable logdir ""

    proc block {ipaddr {reason ""}} {
	variable blocked
	set blocked($ipaddr) [list [clock seconds] $reason]
	variable logdir
	::fileutil::appendToFile [file join $logdir blocked] "$ipaddr [list $blocked($ipaddr)]\n"
	Debug.block {BLOCKING: $ipaddr $reason}
    }

    proc blocked? {ipaddr} {
	variable blocked
	return [info exists blocked($ipaddr)]
    }

    proc init {args} {
	variable {*}$args
	variable blocked
	variable logdir
	catch {
	    array set blocked [fileutil::cat [file join $logdir blocked]]
	}
    }

    proc blockdict {} {
	variable blocked
	set result {}
	foreach {n v} [array get blocked] {
	    lappend result $n [list -site $n -when [clock format [lindex $v 0]] -why [lindex $v 1]]
	}
	return $result
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
