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
	    chan configure $chan -blocking $mode
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
	    chan event $chan readable [list [self] readable $mychan]
	} else {
	    chan event $chan readable ""
	}

	if {"write" in $eventspec} {
	    chan event $chan writable [list [self] writable $mychan]
	} else {
	    chan event $chan writable ""
	}
    }

    # Basic I/O
    method read {mychan n} {
	if {[catch {chan read $chan $n} result eo]} {
	    Debug.chan {$mychan read $chan $n -> error $result ($eo)}
	} else {
	    Debug.chan {$mychan read $chan $n -> [string map {\n \\n} "[string length $result] bytes '[string range $result 0 20]...[string range $result end-20 end]"]'}
	}
	return $result
    }

    method write {mychan data} {
	Debug.chan {$mychan write $chan [string length $data]}
	chan puts -nonewline $chan $data
	return [string length $data]
    }

    # Internals. Methods. Event generation.
    method readable {mychan} {
	Debug.chan {$mychan readable $chan}
	chan postevent $mychan read
	return
    }

    method writable {mychan} {
	Debug.chan {$mychan writable $chan}
	chan postevent $mychan write
	return
    }

    method configure {mychan args} {
	if {[catch {chan configure $chan} r eo]} {
	    Debug.chan {$mychan configure $chan $args -> error $r ($eo)}
	} else {
	    Debug.chan {$mychan configure $chan $args -> r}
	}
	return $r
    }

    # Setting up, shutting down.
    method initialize {mychan mode} {
	Debug.chan {$mychan initialize $chan $mode ([chan configure $chan])}
	chan configure $chan -blocking 0 -buffering none -encoding binary -eofchar {{} {}} -translation {binary binary}
	return [list initialize finalize configure blocking watch read write]
    }

    method finalize {mychan} {
	Debug.chan {$mychan finalize $chan}
	catch {chan close $chan}
	catch {my destroy}
    }

    destructor {
	Debug.chan {$mychan destroyed}
	catch {chan close $chan}
    }

    variable chan
    constructor {args} {
	# Initialize the buffer, current read location, and limit
	set chan ""
	foreach {n v} $args {
	    if {$n ni [info class variables Chan]} {error "$n is not a valid parameter. ([info class variables Chan])"}
	    set $n $v
	}
	if {$chan eq [self]} {
	    error "recursive Chan!  No good."
	} elseif {$chan eq ""} {
	    error "Needs a chan argument"
	}
    }
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    set fd [open [info script] r]
    set fd0 [Chan new chan $fd]
    set fdr [chan create {read write} $fd0]
    set lc 0
    while {[gets $fdr line] != -1 && ![eof $fdr]} {
	puts "[incr lc]: $line"
    }
}
