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
	    set par [split $par {;}]

	    if {[lindex $par 0] eq "compatible"} {
		dict set result id MSIE
		dict set result mozilla_version $version
		set fields {version provider platform}
		dict set result extension [lassign $par -> {*}$fields]
		set version [lindex [split $version] 1]
		foreach f $fields {
		    dict set result $f [string trim [set $f]]
		}
	    } elseif {[string match Gecko/* [lindex $addition 0]]} {
		dict set result id FF
		dict set result mozilla_version $version
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
	    } elseif {$id eq "Lynx"} {
	    } elseif {$id eq "Opera"} {
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
    foreach x {
	"Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.8.0.12) Gecko/20070508 Firefox/1.5.0.12"
	"Lynx/2.8.6rel.4 libwww-FM/2.14 SSL-MM/1.4.1 GNUTLS/1.6.2"
	"Mozilla/2.0 (compatible; Ask Jeeves/Teoma; +http://about.ask.com/en/docs/about/webmasters.shtml)"
	"msnbot/1.0 (+http://search.msn.com/msnbot.htm)"
	"Mozilla/5.0 (compatible; Yahoo! Slurp; http://help.yahoo.com/help/us/ysearch/slurp)"
	"Opera/9.21 (Windows NT 6.0; U; es-es)"
    } {
	puts [ua $x]
    }
}
