package provide Activity 2.0

# maintain activity record
namespace eval Activity {
    # activity array - packet activity per socket
    # log the last packet parse time per socket.
    variable activity {}
    variable actlog {}
    variable actsize 0

    proc activity {what cid args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	variable activity
	catch {
	    catch {dict unset args -entity}
	    dict lappend activity $cid $what [list -when [clock microseconds] {*}$args]
	} r eo
	#puts stderr "ACT: $cid $what ($args)"
    }

    proc disconnect {cid args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	catch {
	    variable activity; variable actlog; variable actsize
	    dict lappend activity $cid disconnect [list -when [clock microseconds] {*}$args]
	    dict unset activity $cid
	    Debug.activity {[dict get $activity $cid]}
	    #dict unset activity $cid
	} r eo
	#puts stderr "DIS: $cid ($args) ($activity)"
    }

    proc new {cid args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	catch {
	    variable activity; variable actlog; variable actsize
	    if {[dict exists $activity $cid]} {
		dict unset activity $cid
	    }
	} r eo
	#puts stderr "NEW: $cid ($args) ($activity)"
    }

    # act2list - make a nested list from an activity list
    proc act2list {act} {
	set headers {cid date time duration thread ipaddr action value error}
	set result [list $headers]

	dict for {cid log} $act {
	    catch {unset vals}
	    # scan remaining activity events
	    set vals(cid) $cid

	    #puts stderr "LOG $cid $log"
	    if {[catch {
		# one of these
		dict get [lindex [dict get $log connected] 0] -when
	    } start]} {
		set start [dict get [lindex $log 1] -when]
	    }
	    set last $start
	    set s [expr {$start / 1000000}]
	    set vals(date) [clock format $s -format {%d/%m/%Y}]
	    set vals(time) [clock format $s -format {%T}]

	    if {[catch {
		# one of these
		dict get [lindex [dict get $log connected] 0] -ipaddr
	    } ipaddr]} {
		set ipaddr ""
	    }
	    set vals(ipaddr) $ipaddr

	    if {[catch {
		# one of these
		dict get [lindex [dict get $log connected] 0] -iworker
	    } thread]} {
		if {[catch {dict get [lindex $log 1] -iworker} thread]} {
		    set thread ""
		}
	    }
	    set vals(thread) $thread

	    foreach {n cr} $log {
		catch {unset vals(action)}
		catch {unset vals(value)}
		catch {unset vals(error)}
		#puts stderr "LIST: $cid $n $cr"
		set when [dict get $cr -when]
		set s [expr {$when / 1000000}]
		if {[dict exists $cr -error]} {
		    lappend vals(error) [dict get $cr -error]
		}
		set vals(action) $n
		set vals(duration) [expr {([dict get $cr -when] - $last) / 1000000.0}]
		catch {set vals(thread) [dict get $cr -iworker]}
		if {[dict exists $cr -ipaddr] && [dict get $cr -ipaddr] ne $ipaddr} {
		    set ipaddr [dict get $cr -ipaddr]
		    set vals(ipaddr) $ipaddr
		} else {
		    catch {unset vals(ipaddr)}
		}
		if {[dict exists $cr -iworker] && [dict get $cr -iworker] ne $thread} {
		    set thread [dict get $cr -iworker]
		    set vals(thread) $thread
		} else {
		    catch {unset vals(thread)}
		}
		catch {set vals(value) [dict get $cr -url]}
		set last $when

		set row {}
		foreach n $headers {
		    if {[info exists vals($n)]} {
			lappend row [armour $vals($n)]
		    } else {
			lappend row {}
		    }
		}
		lappend result $row
		catch {unset vals(date)}
		catch {unset vals(time)}
		catch {unset vals(cid)}
	    }

	}
	return $result
    }

    proc current {} {
	variable activity
	return [act2list $activity]
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
