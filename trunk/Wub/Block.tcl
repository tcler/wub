package provide Block 2.0

namespace eval Block {
    variable blocked; array set blocked {}

    proc block {ipaddr {reason ""}} {
	variable blocked
	set blocked($ipaddr) [list [clock seconds] $reason]
	Debug.block {BLOCKING: $ipaddr $reason}
    }

    proc blocked? {ipaddr} {
	variable blocked
	return [info exists blocked($ipaddr)]
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
