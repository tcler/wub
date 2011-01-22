lappend ::auto_path /usr/lib/tcltk/
package require tdom

set example {<?xml version="1.0" encoding="UTF-8"?>

<xrds:XRDS xmlns:xrds="xri://$xrds" xmlns="xri://$xrd*($v*2.0)" xmlns:openid="http://openid.net/xmlns/1.0">

<XRD>
	<Service priority="10">
		<Type>http://specs.openid.net/auth/2.0/signon</Type>
		<URI>http://www.myopenid.com/server</URI>
		<openid:Delegate>http://smoker.myopenid.com/</openid:Delegate>
	</Service>

	<Service priority="50">
		<Type>http://specs.openid.net/auth/2.0/signon</Type>
		<URI>http://www.livejournal.com/openid/server.bml</URI>
		<openid:Delegate>http://www.livejournal.com/users/frank/</openid:Delegate>
	</Service>

	<Service priority="20">
		<Type>http://lid.netmesh.org/sso/2.0</Type>
	</Service>

	<Service>
		<Type>http://lid.netmesh.org/sso/1.0</Type>
	</Service>

</XRD>

</xrds:XRDS>
}

set want "http://specs.openid.net/auth/2.0/signon"
set doc  [dom parse $example]
set root [$doc documentElement]
set node [[$root selectNodes /xrds:XRDS] firstChild]
if {$node ne ""} {
    foreach service [$node child all] {
	if {[$service nodeName] eq "Service"} {
	    set record {}
	    foreach el [$service child all] {
		dict set record [$el nodeName] [[$el firstChild] nodeValue]
	    }
	    if {[dict exists $record Type]
		&& [dict get $record Type] eq $want
	    } {
		dict set record priority [$service getAttribute priority 100000]
		dict unset record Type
		puts stderr $record
	    }
	}
    }
}

$doc delete
