# UA.tcl - user agent detection.
# http://en.wikipedia.org/wiki/User_agent

package provide UA 1.0

# process the ua string into something enabling us to detect MSIE
proc ua {ua} {
    set rest [string trim [join [lassign [split $ua] mozver]]]
    set id unknown
    set version {}
    set mozver [lassign [split $mozver /] id version]

    set result [dict create ua $ua id $id version $version]
    if {[catch {
	if {[regexp {([^(])*[(]([^)]*)[)](.*)} $rest -> pre par addition]} {
	    foreach v {pre par addition mozver} {
		set $v [string trim [set $v]]
	    }
	    set addition [split $addition]
	    set mver [lindex [split $mozver /] 1]
	    dict set result mozilla_version $mver
	    set par [split $par {;}]

	    if {[lindex $par 0] eq "compatible"} {
		dict set result id MSIE
		set fields {version provider platform}
		dict set result extension [lassign $par -> {*}$fields]
		set version [lindex [split $version] 1]
		foreach f $fields {
		    dict set result $f [string trim [set $f]]
		}
	    } elseif {[string match Gecko/* [lindex $addition 0]]} {
		dict set result id FF
		set fields {platform security subplatform language version}
		dict set result extensions [lassign $par {*}$fields]
		set version [lindex [split $version :] end]
		foreach f $fields {
		    dict set result $f [string trim [set $f]]
		}
		dict set result rest [lassign [split $addition] gecko product]
		foreach p {gecko product} {
		    dict set result product {*}[split [set $p] /]
		}
	    } elseif {[dict get $result id] eq "Lynx"} {
	    } elseif {$addition eq ""} {
		dict set result id NS
		dict set result rest [lassign [split $pre] language provider]
		set fields {platform security subplatform}
		dict lappend result rest [lassign $par {*}$fields]
		lappend fields language provider
		foreach f $fields {
		    dict set result $f [string trim [set $f]]
		}
	    }
	}
    } r eo]} {
	Debug.error {UA: '$r' ($eo)}
	dict set result error "$r ($eo)"
    }
    return $result
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    foreach x {"Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.8.0.12) Gecko/20070508 Firefox/1.5.0.12"} {
	puts [ua $x]
    }
}
