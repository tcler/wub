# Block - manage a block list of sites

package require fileutil
package require Http

package provide Block 2.0

set ::API(Server/Block) {
    {
	Blocks misbehaving clients by IP address.  A blocked client will get an HTTP Forbidden response to any request.

	The Wub server will automatically block IP addresses which attempt an illegal HTTP connection method, such as CONNECT or LINK.  It will also block known Spiders identified by User-Agent classification.

	URL prefixes may be explicitly blocked by using the [../Domains/Nub Nub] pseudo-domain '''Block'''.
    }
    logdir {directory in which to log blockages and store blocked db}
}

namespace eval Block {
    variable blocked; array set blocked {}
    variable logdir ""

    proc block {ipaddr {reason ""}} {
	variable blocked
	if {[Http nonRouting? $ipaddr]} {
	    Debug.block {Can't BLOCK: $ipaddr $reason as it's local}
	    return
	}
	set blocked($ipaddr) [list [clock seconds] $reason]
	variable logdir
	::fileutil::appendToFile [file join $logdir blocked] "$ipaddr [list $blocked($ipaddr)]\n"
	Debug.block {BLOCKING: $ipaddr $reason}
    }

    proc blocked? {ipaddr} {
	variable blocked
	if {[Http nonRouting? $ipaddr]} {
	    return 0
	}
	return [info exists blocked($ipaddr)]
    }

    proc do {r} {
	block [dict get $r -ipaddr]
	return [Http Forbidden $r]
    }

    proc create {args} {
	error "Can't create a named Block domain - must be anonymous"
    }

    proc new {args} {
	variable {*}$args
	variable blocked
	variable logdir
	if {![info exists blocked]} {
	    catch {
		array set blocked [fileutil::cat [file join $logdir blocked]]
	    }
	}
	return ::Block
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
