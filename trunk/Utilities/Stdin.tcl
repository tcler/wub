package provide Stdin 1.0

namespace eval Stdin {
    variable command
    variable prompt "% "

    proc exit {} {
	upvar stdin stdin
	close $stdin
    }

    proc puts {args} {
	return $args
    }

    proc cmd {stdin stdout} {
	if {[eof $stdin]} {
	    fileevent $stdin readable {}
	    return
	}

	variable command
	variable prompt
	append command($stdin,command) [gets $stdin]
	if {[info complete $command($stdin,command)]} {
	    if {[catch {namespace eval ::Stdin $command($stdin,command)} result eo]} {
		::puts -nonewline $stdout "$result ($eo)\n$prompt"
	    } else {
		::puts -nonewline $stdout "$result\n$prompt"
	    }
	    flush $stdout
	    set command($stdin,command) ""
	} else {
	    append command($stdin,command) \n
	}
    }

    proc accept {sock addr port} {
	if {$addr ne "127.0.0.1"} {
	    close $sock
	} else {
	    fconfigure $sock -buffering line
	    ::puts $sock "Command Shell"
	    fileevent $sock readable [list ::Stdin::cmd $sock $sock]
	}
    }

    proc start {{stdin stdin} {stdout stdout}} {
	if {$stdin ne "stdin"} {
	    socket -server ::Stdin::accept -myaddr localhost $stdin
	} else {
	    fileevent $stdin readable [list ::Stdin::cmd $stdin $stdout]
	}
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
