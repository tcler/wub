# Chan - reflected channels in TclOO
package require TclOO
namespace import oo::*

package require Debug
Debug off chan 10

package provide Chan 1.0

# Chans.tcl - reflected channels
class create Chan {
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

    # Basic I/O
    method read {mychan n} {
	if {[catch {::chan read $chan $n} result eo]} {
	    Debug.chan {$mychan read $chan $n -> error $result ($eo)}
	} else {
	    Debug.chan {$mychan read $chan $n -> [string map {\n \\n} "[string length $result] bytes '[string range $result 0 20]...[string range $result end-20 end]"]'}
	}
	return $result
    }

    method write {mychan data} {
	Debug.chan {$mychan write $chan [string length $data]}
	::chan puts -nonewline $chan $data
	return [string length $data]
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

    method configure {mychan args} {
	if {[catch {::chan configure $chan} r eo]} {
	    Debug.chan {$mychan configure $chan $args -> error $r ($eo)}
	} else {
	    Debug.chan {$mychan configure $chan $args -> r}
	}
	return $r
    }

    # Setting up, shutting down.
    method initialize {mychan mode} {
	Debug.chan {$mychan initialize $chan $mode ([::chan configure $chan])}
	::chan configure $chan -blocking 0 -buffering none -encoding binary -eofchar {{} {}} -translation {binary binary}
	return [list initialize finalize configure blocking watch read write]
    }

    method finalize {mychan} {
	Debug.chan {$mychan finalize $chan}

	# remove connection record for connected ip
	my static connections
	set ep [dict get $endpoints peer]
	dict with ep {
	    dict unset connections $ip $port
	}

	catch {::chan close $chan}
	catch {my destroy}
    }

    destructor {
	Debug.chan {[self] destroyed}
	catch {::chan close $chan}
    }

    method socket {} {return $chan}
    method endpoints {} {return $endpoints}

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

    variable chan endpoints
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
    
	# process class parameters
	set classargs [dict filter $args key {-*}]
	foreach {n v} $classargs {
	    switch -- [string trim $n -] {
		maxconnections {
		    my static maxconnections	;# allow setting of max connections
		    if {$class ne ""} {
			dict set maxconnections $ip $v
		    }
		}
	    }
	}

	if {![llength $objargs]} {
	    my destroy	;# this wasn't really a connected socket, just set classvars
	    return
	}

	# validate args
	if {$chan eq [self]} {
	    error "recursive Chan!  No good."
	} elseif {$chan eq ""} {
	    error "Needs a chan argument"
	}

	# get the endpoints for this connected socket
	foreach {n cn} {sock -sockname peer -peername} {
	    set ep [::chan configure $chan $cn]
	    lassign [split $ep] ip name port
	    foreach pn {ip name port} {
		dict set endpoints $n $pn [set $pn]
	    }
	}

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
	    Debug.chan {$ip has connections [dict size $x] > $mc from ([dict get $x])}
	    error "Too Many Connections from $name $ip"
	}
    }
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    set fd [open [info script] r]
    set fd0 [Chan new chan $fd]
    set fdr [::chan create {read write} $fd0]
    set lc 0
    while {[gets $fdr line] != -1 && ![eof $fdr]} {
	puts "[incr lc]: $line"
    }
}
