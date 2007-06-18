package provide Stdin 1.0

namespace eval Stdin {
    variable command
    variable prompt "% "

    proc exit {} {
	upvar stdin stdin
	close $stdin
    }

    proc backends {cmd} {
	set result {}
	foreach tid [array names ::backend::worker] {
	    catch {::thread::send $tid $cmd} r eo
	    lappend result [list "$tid: '$r' ($eo)"]
	}
	return [join $result \n]
    }

    proc workers {cmd} {
	set result {}
	foreach tid [array names ::Httpd::worker] {
	    catch {::thread::send $tid $cmd} r eo
	    lappend result [list "$tid: '$r' ($eo)"]
	}
	return [join $result \n]
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

    proc dump {thread} {
	lassign [thread::send $thread {
	    list [set request] [set sock] [chan eof $::sock] [chan event $sock readable] [chan event $sock writable] [set pending] [set gets] [array size replies] [set response]
	}] request sock eof readable writable pending gets replies response
	return "request: $request sock:$sock eof:$eof readable:$readable writable:$writable pending:$pending gets:$gets replies:$replies response:$response"
    }

    proc ::Stdin::closeit {thread} {
	thread::send $thread {disconnect "operator forced"}
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
