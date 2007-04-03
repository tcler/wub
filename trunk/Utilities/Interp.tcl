package provide Interp 1.0

namespace eval Interp {
    proc exists {req} {
	return expr {
		     [dict exists $req -interp]
		     || ([[dict get $req -http] tied interp] ne "")
		     || ([Session exists $req interp] &&
			 ([Session get $req interp] ne "error"))
		 }
    }

    proc name {req} {
	if {[dict exists $req -interp]} {
	    # already got an interp associated with req
	    return [dict get $req -interp]
	} elseif {[set tied_interp [[dict get $req -http] tied interp]] ne ""} {
	    # we already have an interpreter tied to the Httpd session
	    # recover tied interpreter from Httpd
	    set interp [lindex $tied_interp 1]
	    return $interp
	} elseif {[Session exists $req interp]
		  && ([Session get $req interp] ne "error")} {
	    # get an interpreter from the Session, if any
	    return [Session get $req interp]
	} else {
	    return ""
	}
    }

    # aliases to be passed to session interpreter
    variable aliases {
	Http Query Entity Cookies
	Session Login
	Html Url Form do
    }

    proc nstree {interp {prefix ""}} {
	set result {}
	foreach ns [$interp eval namespace children $prefix] {
	    lappend result $ns
	    lappend result {*}[nstree $interp $ns]
	}
	return $result
    }

    proc serialize {interp} {
	$interp eval {unset -nocomplain errorCode; unset -nocomplain errorInfo}
	set result {}
	foreach v [$interp eval info globals] {
	    if {[string match tcl* $v]} continue
	    if {[$interp eval array exists $v]} {
		lappend result "array set $v [list [$interp eval array get $v]]"
	    } else {
		lappend result "set $v [list [$interp eval set $v]]"
	    }
	}

	foreach ns [nstree $interp] {
	    foreach v [$interp eval info vars ${ns}::*] {
		if {[array exists $v]} {
		    lappend result "array set $v [list [$interp eval array get $v]]"
		} else {
		    lappend result "set $v [list [$interp eval set $v]]"
		}
	    }
	}
	return [join $result "; "]
    }

    proc create {req args} {
	if {[dict exists $req -interp]} {
	    # already got an interp associated with req
	    set interp [dict get $req -interp]
	} elseif {[set tied_interp [[dict get $req -http] tied interp]] ne ""} {
	    # we already have an interpreter tied to the Httpd session
	    # recover tied interpreter from Httpd
	    set interp [lindex $tied_interp 1]
	} elseif {[Session exists $req interp]
		  && ([Session get $req interp] ne "error")} {
	    # get an interpreter from the Session, if any
	    set interp [Session get $req interp]
	} else {
	    # create an interpreter tied to the Httpd or Session

	    if {[dict exists $req -safe] && [dict get $req -safe]} {
		# we want a safe interpreter
		set interp [interp create -safe]
	    } else {
		set interp [interp create]
	    }
	    
	    if {[Session alive $req]} {
		# prefer to associate with Session
		Session set $req interp $interp	;# tie resource to session
	    } else {
		# fallback to tie to http obj
		[dict get $req -http] tie interp delete $interp ;# tie resource to http
	    }
	}

	# set up some useful procs inside the interp
	if {[interp eval $interp {
	    if {[info procs respond] eq {}} {
		proc respond {cmd args} {
		    #set code [catch {
		    do respond $cmd $::response {*}$args
		    #} e eo]
		    #return -code $code -options $eo
		}
		# convert a bastardised emacs httms timestamp to something useful
		proc hhmts {time} {
		    set ::hhmts [string trim $time "<!->\n"]
		    return $time
		}
		return 1
	    } else {
		return 0
	    }
	}]} {
	    # this interpreter wasn't set up fully

	    # these aliases will be constant for a tied interp
	    foreach {a r} {Httpd -http Host -hostobj} {
		if {[dict exists $req $r]} {
		    interp alias $interp $a {} [dict get $req $r] 
		}
	    }

	    # install generally useful aliases
	    variable aliases
	    foreach a $aliases {
		interp alias $interp $a {} ::$a
	    }
	    #interp alias $interp Cache {} [::[dict get $req -hostobj] Cache]
	}
	
	# set up some useful interp aliases
	foreach cmd $args {
	    interp alias $interp $cmd {} ::$cmd
	}

	return $interp
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
