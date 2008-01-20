# Session - handle Session vars stored in a mk db, indexed by a cookie slot.
#
# This is to be run in the main thread, and to amend the -session var in
# a request, to be passed to Workers.
#
# Use of a mk db allows sessions to be available to all threads in a
# multithreaded environment.
#
# Examples:
# 1) Fetch a session
#	set req [Session fetch $req] ;# fetch session specified by cookie
#	set session [dict get $req -session]
#	if {[dict exists $session _key]} {
#		# this is an existing session
#		puts "[dict get $session _key] is the session key"
#	} else {
#		# this is a brand new session
#	}
#
# 2) Modify a session variable
#
#	dict set req -session somevar $value	;# set a session variable
#	dict with req -session {
#		... modify existing variables - only works on existing sessions
#		# risky unless you can control the contents of the dict,
#		# as any element in dict *will* overwrite other vars.
#	}
#
#	set rsp [Session store $req]	;# session doesn't exist until stored
#	return $rsp			;# the rsp will have required cookies
#
# 3) Remove a session from the database
#
#	set rsp [Session remove $req]
#	return $rsp

package provide Session 3.0

namespace eval Session {
    variable salt [clock microseconds]	;# source of random session key
    variable size	;# total count of sessions
    variable empty	;# count of empty slots

    # traverse the session db clearing old sessions
    #
    # this should be done occasionally, whenever the db gets too big, for example
    proc gc {} {
	set dope {}
	foreach el [session properties] {
	    set type [split $el :] name
	    lappend dope $name
	    switch $type {
		I - D - F {
		    lappend dope 0
		}
		default {
		    lappend dope {}
		}
	    }
	}

	variable size [session size]
	variable empty 0
	foreach slot [session lselect _key ""] {
	    session set $slot {*}$dope _slot $slot
	    incr empty
	}
    }

    # fetch a session slot in a request
    proc fetch {req} {
	if {![dict exists $req -session]} {
	    # got to locate session slot
	    set req [Cookies 4Server $req]

	    variable cookie
	    if {[catch {
		dict get [Cookies fetch [dict get $req -cookies] -name $cookie] -value
	    } slot eo]} {
		Debug.error {fetch session: $slot ($eo)}
		return $req
	    }

	    lassign [split $slot] slot key	;# fetch the slot
	    if {$key ne ""} {
		# non null key means active slot
		set session [session get $slot]
		if {[dict get session _key] eq $key} {
		    dict set req -session $session
		    dict set req --session $session
		}
	    }
	} else {
	    dict set req --session {}
	}

	return $req
    }

    # remove the session associated with this request
    proc remove {req} {
	set req [Cookies 4Server $req cookie]
	if {[dict exists $req -session _slot]} {
	    set slot [dict get $req -session _slot]
	    dict unset req -session
	    session set $slot _key ""

	    variable cookie
	    dict set req -cookies [Cookies remove [dict get $req -cookies] -name $cookie]
	}
	return $req
    }

    proc with {rv body} {
	if {[catch {
	    uplevel "dict with [list $rv] -session [list $body]"
	} r eo]} {
	    Debug.error {Session with: $r ($eo)}
	} else {
	    return $r
	}
    }

    # store a session in the db if it's changed
    proc store {req} {
	if {[Dict get? $req -session] eq [Dict get? $req --session]} {
	    return $req	;# no change to session vars - just skip it
	}

	set req [Cookies 4Server $req cookie]
	set session [dict get $req -session]
	if {[dict exists $session _slot]} {
	    # it's already been created
	    set slot [dict get $session _slot]
	    set key [dict get $session _key]
	    session set [dict get $session _slot] {*}$session _mtime [clock seconds]
	} else {
	    # need to create a new slot
	    variable salt;
	    set key [md5::md5 [incr salt]]	;# new slot's key
	    set now [clock seconds]
	    if {[catch {
		session find key ""	;# get a deleted session slot
	    } slot]} {
		# no empty slots - create a new slot
		variable cookie
		set slot [session append {*}$session _key $key _ctime $now _mtime $now]
		session set $slot _slot $slot
		variable size
		incr size
	    } else {
		# use a deleted session slot
		session set $slot {*}$session _key $key _ctime $now _mtime $now _slot $slot
		variable empty
		incr empty -1
	    }
	}

	# add the accessor cookie to the request
	variable cookie
	dict set req -cookies [Cookies add [dict get $req -cookies] -name $cookie -value [list $slot $key]]

	return $req
    }

    variable cookie "session"	;# session cookie name
    variable db session.mk	;# session database

    # initialize the session accessor functions
    proc init {args} {
	if {$args ne {}} {
	    variable {*}$args
	}

	# try to open wiki view
	if {[catch {
	    mk::file views session
	    # we expect threads to be created *after* the db is initialized
	    variable toplevel 0
	} views]} {
	    # this is the top level process, need to create a db
	    variable toplevel 1
	    variable db
	    mk::file open db $db -shared
	}
	View init session db.session

	gc	;# start up by garbage collecting Session db.
    }

    namespace export -clear *
    namespace ensemble create -subcommands {} -map {}
}
