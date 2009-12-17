# Dhcpp.tcl - parse dhcpd's leases file, generate reports

package require OO
package require Query
package require Url
package require Debug
package require Report

package provide Dhcpp 1.0

class create ::Dhcpp {
    method stanza {stanza} {
	variable interp
	set stanza [string map {$ \\\$ \[ \\\[ \] \\\]} $stanza]
	if {[catch {$interp eval $stanza} e eo]} {
	    Debug.error {ERR over ($stanza): $e ($eo)}
	}
	set result [$interp eval get_lease]
	#puts stderr stanza:'$result'
	return $result
    }

    method parse {{leasef /var/lib/dhcp3/dhcpd.leases}} {
	variable last
	if {[file mtime $leasef] <= $last} {
	    #puts stderr "no changes to $leasef"
	    return {}	;# no change
	}
	set oldleases $leases
	set new {}
	set changes {}
	foreach line [string trim [split [::fileutil::cat $leasef] \n]] {
	    if {$line eq ""} {
		#puts stderr ">"
		continue
	    }
	    if {[string match #* $line]} {
		#puts stderr ">#"
		continue
	    }
	    if {$line ne "\}"} {
		set line [string map {\} \\\}} $line]
	    }

	    #puts stderr ">$line"
	    append stanza \n $line
	    if {[info complete $stanza]} {
		#puts stderr "parsing '$stanza'"
		set lease [my stanza $stanza]
		if {[dict size $lease]} {
		    #puts stderr "parsing ($stanza)"
		    set ip [dict get $lease ip]
		    if {[dict exists $leases $ip]} {
			if {[dict get $leases $ip] eq $lease} {
			    #puts stderr "no change on '$lease'"
			    continue	;# no change for this lease
			} else {
			    set old [dict get $leases $ip]
			    set nl $lease
			    set add {}
			    set del {}
			    set mod {}
			    dict for {n v} $old {
				if {[dict exists $nl $n]} {
				    if {[dict get $nl $n] ne $v} {
					lappend mod $n $v
				    } else {
				    }
				    dict unset nl $n
				} else {
				    lappend del $n $v
				}
			    }
			    dict for {n v} $new {
				lappend add $n $v
			    }
			}
			lappend changes $ip [list add $add del $del mod $mod]
		    } else {
			lappend new $ip $lease
		    }

		    dict set lease when [clock milliseconds]
		    dict set leases $ip $lease
		}
		set stanza ""
	    }
	    set last [clock seconds]
	}
	return [list new $new changed $changes]
    }

    method process {{file /var/lib/dhcp3/dhcpd.leases}} {
	variable bymac {}
	variable byhost {}
	variable host2mac {}
	variable mac2ip {}
	variable ip2mac {}

	set mods [my parse $file]
	if {![dict size $mods]} {
	    return {}
	}
	lassign [my parse $file] new changed
	dict for {ip lease} $new {
	    set mac [dict get $lease mac]
	    if {[dict exists $bymac $mac]} {
		dict set bymac $mac [clock seconds] $ip	;# record all ips/mac
		dict set ip2mac $ip $mac
		dict set mac2ip $mac $ip
	    }
	    if {[dict exists $lease host]} {
		dict set host2mac [dict get $lease host] $mac
		dict set byhost [dict get $lease host] [dict get $lease ip]
	    }
	}

	dict for {ip chset} $changed {
	    dict with chset {
		if {[dict exists $mod mac]} {
		    # the mac of a lease has changed
		    catch {
			set oldmac [dict get $ip2mac $ip]
			dict unset mac2ip $oldmac
		    }
		    dict set ip2mac $ip [dict get $mod mac]
		    dict set mac2ip [dict get $mod mac] $ip 
		}
	    }
	}
	return $mods
    }

    method /css {r} {
	set css {
	    * {zoom: 1.0;}

	    input.blur {
		color:lightgray;
	    }
	    img.icon {
		border:0px;
		width:25px
	    }

	    div.nav {
		float:right;
		background: whitesmoke;
		padding: 0.3em 0.7em;
		-moz-border-radius-topleft:5px;
		-moz-border-radius-topright:5px;
		-moz-border-radius-bottomleft:5px;
		-moz-border-radius-bottomright:5px;
	    }
	    h1.pretty, h2.pretty, h3.pretty, h4.pretty, h5.pretty, h6.pretty {
		background: darkslategray;
		color: whitesmoke;
		padding: 0.2em 0.5em;
		-moz-border-radius-topleft:7px;
		-moz-border-radius-topright:7px;
		-moz-border-radius-bottomleft:7px;
		-moz-border-radius-bottomright:7px;
	    }
	    table.pretty {
		margin: 1em 1em 1em 2em;
		background: whitesmoke;
		border-collapse: collapse;
	    }
	    table.pretty td {
		border: 1px silver solid;
		padding: 0.2em;
	    }
	    table.pretty th {
		border: 1px silver solid;
		padding: 0.2em;
		background: darkslategray;
		color: white;
		text-align: left;
		-moz-border-radius-topleft:7px;
		-moz-border-radius-topright:7px;
		-moz-border-radius-bottomleft:7px;
		-moz-border-radius-bottomright:7px;
	    }
	    table.pretty tr.family {
		background: gainsboro;
	    }
	    table.pretty caption {
		margin-left: inherit;
		margin-right: inherit;
		font-size: 150%;
	    }

	    fieldset {
		background: whitesmoke;

		-moz-border-radius-topleft:7px;
		-moz-border-radius-topright:7px;
		-moz-border-radius-bottomleft:7px;
		-moz-border-radius-bottomright:7px;
	    }

	    fieldset > legend {
		background: darkslategray;
		color: white;
		-moz-border-radius-topleft:5px;
		-moz-border-radius-topright:5px;
		-moz-border-radius-bottomleft:5px;
		-moz-border-radius-bottomright:5px;
	    }

	    .button {
		border: 1px solid #aaa;
		-webkit-border-radius: 5px;
		-moz-border-radius: 5px;
		padding: 2px 5px;
		margin: 0 3px;
		cursor: pointer;
		background: gainsboro;
	    }
	    .changed {
		background-color: gainsboro;
	    }
	}
	return [Http Ok $r $css text/css]
    }

    method / {r} {
	set mods [my parse]
	if {![dict size $mods]} {
	    #return [Http NotModified $r]
	}
	append content "[<a> href . all] [<a> href ip2mac ip2mac] [<a> href mac2ip mac2ip] [<a> href byhost byhost]"
	append content [Report html $leases headers {ip mac host vendor-string binding_state} {*}$rparams]
	dict set r -style $mount/css {}
	return [Http Ok $r $content]
    }

    method /ip2mac {r} {
	set mods [my process]
	if {![dict size $mods]} {
	    #return [Http NotModified $r]
	}

	variable ip2mac
	append content "[<a> href . all] [<a> href ip2mac ip2mac] [<a> href mac2ip mac2ip] [<a> href byhost byhost]"
	append content [Report html $ip2mac headers {ip mac} {*}$rparams]
	dict set r -style $mount/css {}
	return [Http Ok $r $content]
    }

    method /mac2ip {r} {
	set mods [my process]
	if {![dict size $mods]} {
	    #return [Http NotModified $r]
	}

	variable mac2ip
	append content "[<a> href . all] [<a> href ip2mac ip2mac] [<a> href mac2ip mac2ip] [<a> href byhost byhost]"
	append content [Report html $mac2ip headers {mac ip} {*}$rparams]
	dict set r -style $mount/css {}
	return [Http Ok $r $content]
    }

    method /byhost {r} {
	set mods [my process]
	if {![dict size $mods]} {
	    #return [Http NotModified $r]
	}

	variable byhost
	append content "[<a> href . all] [<a> href ip2mac ip2mac] [<a> href mac2ip mac2ip] [<a> href byhost byhost]"
	append content [Report html $byhost headers {host ip} {*}$rparams]
	dict set r -style $mount/css {}
	return [Http Ok $r $content]
    }

    method every {ms body} {
	eval $body
	set after [after $ms [info level 0]]
    }

    variable mount leasef leases every after rparams
    mixin Direct

    constructor {args} {
	variable last 0
	variable bymac {}
	variable byhost {}
	variable host2mac {}
	variable mac2ip {}
	variable ip2mac {}
	set rparams {
	    class pretty
	    armour 0
	    sortable 0
	    evenodd 0
	    tparam {}
	    thparam {}
	    thrparam {}
	    tfparam {}
	    tfrparam {}
	    rparam {}
	    eclass el
	    eparam {}
	}

	# construct a safe interpreter
	variable interp [interp create -safe [self]_dhcp]
	$interp eval {
	    variable lease {}
	    rename set Set
	
	    proc get_lease {} {
		variable lease
		return $lease
	    }
	    proc set {var = val} {
		variable lease
		dict set lease $var $val
	    }
	    proc hardware {type value} {
		variable lease
		if {$type ne "ethernet"} {
		    dict set lease hardware [list type $type value $value]
		} else {
		    dict set lease mac $value
		}
	    }
	    proc uid {value} {
		dict set lease uid $value
	    }
	    proc client-hostname {value} {
		variable lease
		dict set lease hostname [string trim $value \"]
	    }
	    proc failover {args} {}
	    foreach v {starts ends tstp tsfp cltt atsfp} {
		{*}[string map [list %P% $v] {
		    proc %P% {args} {
			variable lease
			if {[llength $args] == 1} {
			    dict set lease %P% [lindex $args 0]
			} else {
			    lassign $args num date time
			    dict set lease %P% [list num $num date $date time $time]
			}
		    }
		}]
	    }
	    proc binding {x state} {
		variable lease
		dict set lease binding_$x $state
	    }
	    proc next {x y state} {
		variable lease
		dict set lease next_${x}_$y $state
	    }
	    proc lease {ip details} {
		variable lease [list ip $ip]
		eval $details
	    }
	}

	set leases {}
	foreach {n v} $args {
	    set $n $v
	}

	# start poller if needed
	if {[info exists every]} {
	    my every $every {my poll}
	}
    }
}
