# UA.tcl - user agent detection.
# http://en.wikipedia.org/wiki/User_agent

package provide UA 1.0

# process the ua string into something enabling us to detect MSIE
proc ua {ua} {
    set rest [string trim [join [lassign [split $ua] mozver]]]
    if {![string match Mozilla/* $mozver]} {
	# this is not a standard kind of ua
	return [list id unknown ua $ua]
    }

    set result [dict create ua $ua id unknown]
    if {[catch {
	if {[regexp {([^(])*[(]([^)]*)[)](.*)} -> pre par addition]} {
	    foreach v {pre par addition mozver} {
		set $v [string trim [set $v]]
	    }
	    set addition [split $addition]
	    lassign [split $mozver /] mver
	    dict set result mozilla_version $mver
	    set par [split $par {;}]

	    if {[lindex $par 0] eq "compatible"} {
		dict set result id MSIE
		set fields {version provider platform}
		dict set result extension [lassign $par -> {*}$fields]
		set version [lindex [split $version] 1]
		foreach f $fields {
		    dict set result $f [set $f]
		}
	    } elseif {[string match Gecko/* [lindex $addition 0]]} {
		dict set result id FF
		set fields {platform security subplatform language version}
		dict set extensions [lassign $par {*}$fields]
		set version [lindex [split $version :] end]
		foreach f $fields {
		    dict set result $f [set $f]
		}
		dict set result rest [lassign [split $addition] gecko product]
		foreach p {gecko product}
		dict set result product {*}[split $p /]
	    } elseif {$addition eq ""} {
		dict set result id NS
		dict set result rest [lassign [split $pre] language provider]
		set fields {platform security subplatform}
		dict lappend result rest [lassign $par {*}$fields]
		lappend fields language provider
		foreach f $fields {
		    dict set result $f [set $f]
		}
	    }
	}
    } r eo]} {
	dict set result error "$r ($eo)"
    }
    return $result
}
