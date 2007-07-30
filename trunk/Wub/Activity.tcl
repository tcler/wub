package provide Activity 2.0

# maintain activity record
namespace eval Activity {
    # activity array - packet activity per socket
    # log the last packet parse time per socket.
    variable activity
    array set activity {}
    variable actlog {}
    variable actsize 0

    proc activity {cid what args} {
	variable activity
	lappend activity($cid) $what [list [clock microseconds] {*}$args]
    }

    proc disconnect {cid} {
	variable activity; variable actlog; variable actsize
	lappend activity($cid) disconnect [clock microseconds]
	if {$actsize > 0} {
	    lappend actlog [list $cid {*}$activity($cid)]
	    if {[llength $actlog] > $actsize} {
		set actlog [lrange $actlog 10 end]
	    }
	}
	Debug.activity {$activity($cid)}
	unset activity($cid)
    }

    proc new {cid} {
	variable activity; variable actlog; variable actsize
	if {[info exists activity($cid)]} {
	    lappend activity($cid) ripped [clock microseconds]
	    if {$actsize > 0} {
		lappend actlog [list $cid {*}$activity($cid)]
		if {[llength $actlog] > $actsize} {
		    set actlog [lrange $actlog 10 end]
		}
	    }
	    Debug.activity {$activity($cid)}
	    unset activity($cid)
	}
    }

    proc inactive {{time 60}} {
	set time [expr {$time * 1000000}]
	set result {}
	set now [clock microseconds]

	foreach {s v} [array get ::Activity::activity] {
	    set thread [lindex $v 1 2]
	    set start [lindex $v 1 0]
	    set last [lindex $v end 0]
	    if {($now - $last) > $time} {
		lappend result $thread
	    }
	}
	return $result
    }

    # act2list - make a nested list from an activity list
    proc act2list {act} {
	set headers {date connect age cid thread ipaddr transfer parsed disconnect errors}
	set result [list $headers]
	foreach a [lsort -dictionary -index {2 0} $act] {
	    catch {unset vals}

	    # get connection record
	    set a [lassign $a vals(cid) what connrec]

	    # process connection event (must be first)
	    set connrec [lassign $connrec connect]
	    set s [expr {$connect / 1000000}]
	    set vals(date) [clock format $s -format {%d/%m/%Y}]
	    set vals(connect) [clock format $s -format {%T}]
	    set last [clock microseconds]

	    dict with connrec {
		set vals(ipaddr) $ipaddr
		if {[info exists thread]} {
		    set vals(thread) $thread
		    unset thread
		}
	    }

	    # scan remaining activity events
	    foreach {n v} $a {
		switch $n {
		    parsed {
			#lappend vals(urls) [lindex $v 2]
			#set vals(ipaddr) [lindex $v 1]
		    }
		    transfer {
			if {[llength $v] > 1} {
			    lappend vals(errors) [lrange $v 1 end]
			}
		    }
		}
		lappend vals($n) [expr {(([lindex $v 0] - $connect) / 1000)/1000.0}]s
		set last [lindex $v 0]
	    }
	    set vals(age) [expr {(([clock microseconds] - $last)/1000)/1000.0}]s

	    set row {}
	    foreach n $headers {
		if {[info exists vals($n)]} {
		    if {$n eq "parsed"} {
			lappend row "[llength $vals($n)]: [lindex $vals($n) 0]-[lindex $vals($n) end]"
		    } else {
			lappend row [armour $vals($n)]
		    }
		} else {
		    lappend row {}
		}
	    }
	    lappend result $row
	}
	return $result
    }

    proc current {} {
	variable activity
	set act {}
	foreach {n v} [array get activity] {
	    lappend act [list $n {*}$v]
	}
	return [act2list $act]
    }

    proc log {} {
	variable actlog
	return [act2list $actlog]
    }

    proc clear {} {
	variable actlog; set actlog {}
    }

    proc state {} {
	package require Url
	variable activity; array set A [array get activity]
	if {[info exists ::Httpd::workers]} {
	    array set W [array get ::Httpd::workers]
	} else {
	    array set W {}
	}
	
	set result {}
	foreach {cid con} [array get ::Httpd::connection] {
	    dict with con {
		# get backend thread if possible
		if {[catch {Backend s2be $socket} be]} {
		    set be ""
		}

		if {[info exists A($cid)]} {
		    set time [expr {[lindex $A($cid) 1 0] / 1000000}]
		    set start [list [clock format $time -format {%d/%m/%Y}] [clock format $time -format {%T}]]
		    
		    set time [expr {[lindex $A($cid) end 0] / 1000000}]
		    set end [list [clock format $time -format {%d/%m/%Y}] [clock format $time -format {%T}]]
		    
		    catch {unset urls}; array set urls {}
		    set log {}
		    foreach {n v} $A($cid) {
			if {$n eq "parsed"} {
			    set url [Url parse [lindex $v 2]]
			    incr urls([dict get $url -path]) 1
			    lappend log [dict get $url -path]
			}
		    }
		} else {
		    set start ""; set end ""; set log ""
		}

		lappend result [list $cid $socket $thread $be $ipaddr $start $end $log]
	    }
	}
	return $result
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
