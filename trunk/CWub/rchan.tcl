# rchan - simple channel reflector for CWub
#
# This implements a buffered channel in tcl, so CWub users
# can treat an HTTP connection as a file/stream.

package rchan 1.0

namespace eval rchan {

    variable chan	;# set of known channels
    array set chan {}

    proc initialize {chanid} {
	variable chan
	set chan($chanid) ""
    }

    proc finalize {chanid} {
	variable chan
	unset chan($chanid)
    }

    variable watching
    array set watching {read 0 write 0}

    proc watch {chanid events} {
	variable watching
	if {$event eq {}} {
	    foreach event $events {
		set watching($event) 0
	    }
	} else {
	    foreach event $events {
		set watching($event) 1
	    }
	}
    }

    proc read {chanid count} {
	variable chan
	if {[string length $chan($chanid)] < $count} {
	    set result $chan($chanid); set chan($chanid) ""
	} else {
	    set result [string range $chan($chanid) 0 $count-1]
	    set chan($chanid) [string range $chan($chanid) $count end]
	}

	# implement max buffering
	variable watching
	variable max
	if {$watching(write) && ([string length $chan($chanid)] < $max)} {
	    chan postevent $chanid write
	}

	return $result
    }

    variable max 1048576	;# maximum size of the reflected channel

    proc write {chanid data} {
	variable chan
	variable max
	variable watching

	set left [expr {$max - [string length $chan($chanid)]}]	;# bytes left in buffer
	set dsize [string length $data]
	if {$left >= $dsize} {
	    append chan($chanid) $data
	    if {$watching(write) && ([string length $chan($chanid)] < $max)} {
		# inform the app that it may still write
		chan postevent $chanid write
	    }
	} else {
	    set dsize $left
	    append chan($chanid) [string range $data $left]
	}

	# inform the app that there's something to read
	if {$watching(read) && ($chan($chanid) ne "")} {
	    chan postevent $chanid read
	}

	return $dsize	;# number of bytes actually written
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
