# Chan - reflected channels in TclOO
package require TclOO
namespace import oo::*

package require Debug
Debug off chan 10
Debug on connections 10

package provide Chan 1.0

# Chan.tcl - reflected channels
class create IChan {
    # Event management.
    method blocking {mychan mode} {
	if {[catch {
	    ::chan configure $chan -blocking $mode
	} r eo]} {
	    Debug.chan {$mychan blocking $chan $mode -> error $r ($eo)}
	} else {
	    Debug.chan {$mychan blocking $chan $mode -> $r}
	    return $r
	}
    }

    method watch {mychan eventspec} {
	Debug.chan {$mychan watch $chan $eventspec}
	if {"read" in $eventspec} {
	    ::chan event $chan readable [list [self] readable $mychan]
	} else {
	    ::chan event $chan readable ""
	}

	if {"write" in $eventspec} {
	    ::chan event $chan writable [list [self] writable $mychan]
	} else {
	    ::chan event $chan writable ""
	}
    }

    # Internals. Methods. Event generation.
    method readable {mychan} {
	Debug.chan {$mychan readable $chan}
	::chan postevent $mychan read
	return
    }

    method writable {mychan} {
	Debug.chan {$mychan writable $chan}
	::chan postevent $mychan write
	return
    }

    # Basic I/O
    method read {mychan n} {
	if {[catch {::chan read $chan $n} result eo]} {
	    Debug.chan {$mychan read $chan $n -> error $result ($eo)}
	} else {
	    Debug.chan {$mychan read $chan $n -> [string length $result] bytes: [string map {\n \\n} "[string length $result] bytes '[string range $result 0 20]...[string range $result end-20 end]"]'}
	    Debug.chan {$mychan read $chan eof     = [chan eof     $chan]}
	    Debug.chan {$mychan read $chan blocked = [chan blocked $chan]}
	    Debug.chan {$chan configured: ([chan configure $chan])}

	    # ![chan configure $chan -blocking] - optimization -> save the
	    # -blocking information in a flag, as it passes through method
	    # 'blocking'.
	    if {![string length $result] &&
		![chan configure $chan -blocking] &&
		![chan eof $chan]} {
		return -code error EAGAIN
	    }
	}
	return $result
    }

    method write {mychan data} {
	Debug.chan {$mychan write $chan [string length $data]}
	::chan puts -nonewline $chan $data
	return [string length $data]
    }

    # Setting up, shutting down.
    method initialize {mychan mode} {
	Debug.chan {$mychan initialize $chan $mode}
	Debug.chan {$chan configured: ([chan configure $chan])}
	return [list initialize finalize blocking watch read write cget cgetall configure]
    }

    method configure {mychan option value} {
	return [next $mychan $option $value]
    }

    method finalize {mychan} {
	Debug.chan {$mychan finalize $chan}
	catch {next $mychan}
	catch {::chan close $chan}
	catch {my destroy}
    }

    method cget {mychan option} {
	switch -- $option {
	    -self {
		return [self]
	    }
	    -fd {
		return $chan
	    }
	}
	if {[catch {next $mychan $option} result]} {
	    return [::chan configure $chan $option]
	} else {
	    return $result
	}
    }

    method cgetall {mychan} {
	if {[catch {
	    next $mychan
	} result]} {
	    set result {}
	}
	lappend result -self [self] -fd $chan
	lappend result {*}[::chan configure $chan]
	Debug.chan {[self] cgetall $mychan -> $result}
	return $result
    }

    variable chan
    constructor {args} {
	# Initialize the buffer, current read location, and limit
	set chan ""

	# process object args
	set objargs [dict filter $args key {[a-zA-Z]*}]
	foreach {n v} $objargs {
	    if {$n ni [info class variables [info object class [self]]]} {
		error "$n is not a valid parameter. ([info class variables [info object class [self]]] are)"
	    }
	    set $n $v
	}

	next {*}$args

	if {![llength $objargs]} {
	    my destroy	;# this wasn't really a connected socket, just set classvars
	    return
	}

	# validate args
	if {$chan eq [self]} {
	    error "recursive chan!  No good."
	} elseif {$chan eq ""} {
	    error "Needs a chan argument"
	}
	#rename [self] [namespace qualifiers [self]]::$chan	;# rename the object to the $chan.
    }

    destructor {
	Debug.chan {[self] destroyed}
	if {[catch {::chan close $chan} e eo]} {
	    Debug.chan {failed to close $chan [self] because '$e' ($eo)}
	}
	next
    }
}

class create CaptureChan {
    variable capture file fd

    method read {mychan n} {
	set result [next $mychan $n]
	if {$capture && $fd ne ""} {
	    puts -nonewline $fd $result; flush $fd
	}
	return $result
    }

    method capture {{on 1}} {
	set capture $on
    }

    # set capture on/off
    constructor {args} {
	# process class parameters
	set fd ""
	set capture 0

	set classargs [dict filter $args key {-*}]
	foreach {n v} $classargs {
	    switch -- [string trim $n -] {
		capture {
		    set capture $v	;# set capture on/off
		}
		file {
		    # construct the capture file
		    set file $v
		    set fd [open $v a]
		    ::chan configure $fd -buffering none -translation binary
		}
	    }
	}
	next {*}$args
    }

    destructor {
	if {$fd ne ""} {
	    chan close $fd
	}
	next
    }
}

class create Socket {
    method socket {} {return $chan}
    method endpoints {} {return $endpoints}

    method configure {mychan option value} {
	switch -glob -- $option {
	    max* {
		set ip [dict get $endpoints peer ip]
		my static maxconnections	;# allow setting of max connections
		dict set $maxconnections $ip $value
	    }
	}
    }

    method cgetall {mychan} {
	set result {}
	foreach n {-connections -maxconnections} {
	    lappend result $n [my cget $mychan $n]
	}
	return $result
    }

    method cget {mychan option} {
	switch -glob -- $option {
	    -maxconnections {
		set ip [dict get $endpoints peer ip]
		my static maxconnections	;# allow setting of max connections
		if {[dict exists $maxconnections $ip]
		    return [dict get $maxconnections $ip]
		} else {
		    return [dict get $maxconnections ""]
		}
	    }
	    -connections {
		set ip [dict get $endpoints peer ip]
		my static connections
		return [dict keys $connections $ip]
	    }
	}
	error "No such option $option"
    }

    # provide static variables
    method static {args} {
        if {![llength $args]} return
        set callclass [lindex [self caller] 0]
        define $callclass self export varname
        foreach vname $args {
            lappend pairs [$callclass varname $vname] $vname
        }
        uplevel 1 upvar {*}$pairs
    }

    method maxconnections {args} {
	my static maxconnections	;# allow setting of max connections
	lassign $args ip value
	if {$value eq "" && [string is integer -strict $value]} {
	    dict set maxconnections "" $value
	} else {
	    dict set maxconnections $ip $value
	}
    }

    #mixin CaptureChan IChan	;# run the capture refchan
    mixin IChan		;# mixin the identity channel
    variable chan endpoints

    constructor {args} {
	Debug.chan {Socket construction ($args)}

	# process class parameters
	set chan [dict get? $args chan]
	set classargs [dict filter $args key {-*}]
	foreach {n v} $classargs {
	    switch -glob -- [string trim $n -] {
		max* {
		    my static maxconnections	;# allow setting of max connections
		    if {$chan ne ""} {
			dict set maxconnections $ip $v
		    } else {
			dict set maxconnections "" $v
		    }
		    dict unset args $n
		}
		default {
		    lappend cargs $n $v
		}
	    }
	}

	if {$chan eq ""} {
	    return
	}

	::chan configure $chan -blocking 0 -buffering none -encoding binary -eofchar {{} {}} -translation {binary binary}

	# get the endpoints for this connected socket
	foreach {n cn} {socket -sockname peer -peername} {
	    set ep [::chan configure $chan $cn]
	    lassign [split $ep] ip name port
	    foreach pn {ip name port} {
		dict set endpoints $n $pn [set $pn]
	    }
	}

	Debug.chan {Socket configured $chan to [::chan configure $chan]}

	# keep tally of connections from a given peer
	my static connections
	dict set connections $ip $port [self]
 
	# determine maxconnections for this ip
	my static maxconnections
	if {![info exists maxconnections]} {
	    dict set maxconnections "" 20	;# an arbitrary maximum
	}
	if {[dict get? $maxconnections $ip] ne ""} {
	    set mc [dict get $maxconnections $ip]
	} else {
	    # default maxconnections
	    set mc [dict get $maxconnections ""]
	}

	# check overconnections
	set x [dict get $connections $ip]
	if {[dict size $x] > $mc} {
	    Debug.connections {$ip has connections [dict size $x] > $mc from ([dict get $x]) / [llength [chan names]] open fds}
	    #error "Too Many Connections from $name $ip"
	} else {
	    Debug.connections {$ip connected from port $port ([dict size $x] connections) / [llength [chan names]] open fds}
	}
    }

    method finalize {mychan} {
	catch {my destroy}
    }

    destructor {
	# remove connection record for connected ip
	my static connections
	if {![info exists endpoints]} return

	set ep [dict get $endpoints peer]
	dict with ep {
	    dict unset connections $ip $port
	    set remain [dict get $connections $ip]
	    Debug.connections {$ip port $port disconnected ([dict size $remain]) remaining}
	    
	    if {![dict size $remain]} {
		dict unset connections $ip
		Debug.connections {total: [dict size $connections]}
	    }
	}
    }
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    set fd [open [info script] r]
    set fd0 [IChan new chan $fd]
    set fdr [::chan create {read write} $fd0]
    set lc 0
    while {[gets $fdr line] != -1 && ![eof $fdr]} {
	puts "[incr lc]: $line"
    }
}
