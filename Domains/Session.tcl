# Session - handle Session vars stored in a mk db, indexed by a cookie slot.
#
# This is to be run in the main thread, and to amend the -session var in
# a request, to be passed along with request to Workers.
#
# Use of a mk db allows sessions to be available to all threads in a
# multithreaded environment.
#
# ARCHITECTURE:
# Cookies shouldn't be sent with a path of /, because it screws up caching
# Session maintains a key in cookie which is sent within a constrained path.
# (a) how can a cookie within a constrained path be used
# everywhere on a site when it's only sent for a subpath of the site - use a
# web-bug technique - load some file from session domain on every page, don't cache
# that subdomain.  Use Wub to associate the cookie with the whole session.
# (b) how can a cookie within a sub-path impact on the whole page as seen by a 
# client?  Javascript.  Send javascript from the web bug.
#
# IMPLEMENTATION:
# Sessions are records in a metakit database which are loaded into a subdict
# of request, using the session key value passed in via a cookie.
# If the session subdict changes during processing of the request, changes will
# be rewritten to the database.
#
# Examples:
#
# 1) Fetch a session
#
#	set req [Session fetch $req] ;# fetch session specified by cookie
#	set session [dict get $req -session]	;# this is the session dict
#	if {[dict exists $session _key]} {
#		# this is an existing session
#		puts "[dict get $session _key] is the session key"
#	} else {
#		# this is a brand new session (as no key has been assigned)
#	}
#
# 2) Modify/use session state
#
# FIRST:
#	dict set req -session somevar $value	;# set a session variable
#
# THEN:
#	dict with req -session {
#		... modify existing variables - only works on existing sessions
#		# risky unless you can control the contents of the dict,
#		# as any element in dict *will* overwrite other vars.
#	}
# OR:
#	Session with req {
#		... modify existing variables
#	}
#
# FINALLY:
#	set rsp [Session store $req]	;# session doesn't exist until stored
#	return $rsp			;# the rsp will have required cookies
#
# 3) Remove a session from the database
#
#	set rsp [Session remove $req]
#	return $rsp

package provide Session 3.1

namespace eval Session {
    variable db session.mk	;# session database

    variable salt [clock microseconds]	;# source of random session key
    variable size	;# total count of sessions
    variable empty	;# count of empty slots

    variable cookie "session"	;# session cookie name
    variable cpath "/"		;# session cookie path - this default is a bad idea.

    # traverse the session db clearing old session data
    #
    # we don't remove the rows, because we depend on the slot not changing,
    # however we remove the data from each of the deleted records
    # this should be done occasionally, whenever the db gets too big
    # however, there's a race if new sessions can be made in parallel with gc, so don't.
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
    proc fetch {req args} {
	if {[dict exists $req -session]} {
	    # -session exists, ensure that
	    # it's written back to the db by setting --session to empty
	    dict set req --session {}
	    return $req
	}

	# no session record in request dict
	set req [Cookies 4Server $req]	;# first get cookies

	variable cookie
	if {[catch {
	    dict get [Cookies fetch [dict get $req -cookies] {*}$args -name $cookie] -value
	} slot eo]} {
	    # there's no session cookie, we're done
	    #Debug.error {fetch session: $slot ($eo)}
	    return $req
	}

	# got a session cookie
	lassign [split $slot] slot key	;# fetch the slot
	if {$key ne ""} {
	    # non null key means session has an active slot
	    set session [session get $slot]	;# read session from db
	    if {[dict get session _key] eq $key} {
		#
		dict set req -session $session	;# store the session in the request
		dict set req --session $session	;# copy session to detect changes
	    }
	}

	return $req
    }

    # remove the session associated with this request
    proc remove {req} {
	set req [Cookies 4Server $req cookie]	;# fetch the cookies
	if {![dict exists $req -session _slot]} {
	    return $req	;# no session in request, we're done
	}

	# there's a session in the request - remove it
	set slot [dict get $req -session _slot]	;# get the session slot
	catch {dict unset req -session}		;# remove -session from request
	catch {dict unset req --session} 	;# remove comparison --session too
	session set $slot _key ""	;# flag session as deleted in the db

	# remove the cookie as well as the session
	variable cookie
	dict set req -cookies [Cookies clear [dict get $req -cookies] -name $cookie]

	return $req
    }

    # provide session vars as local vars in caller
    proc with {rv body} {
	if {[catch {
	    uplevel "dict with $rv -session [list $body]"
	} r eo]} {
	    Debug.error {Session with: $r ($eo)}
	} else {
	    return $r
	}
    }

    # store the session in the db if it's changed
    proc store {req args} {
	if {[Dict get? $req -session] eq [Dict get? $req --session]} {
	    return $req	;# no change to session vars - just skip it
	}

	# write back changed -session state
	set req [Cookies 4Server $req cookie]	;# get cookie (redundant?)
	set session [dict get $req -session]	;# get the session

	if {[dict exists $session _slot]} {
	    # session is already stored in db - update it.
	    set slot [dict get $session _slot]	;# remember session slot
	    set key [dict get $session _key]	;# remember session key
	    session set $_slot {*}$session _mtime [clock seconds]
	} else {
	    # need to create a new slot for the session
	    variable salt;
	    set key [md5::md5 [incr salt]]	;# new slot's random key
	    set now [clock seconds]		;# get current time
	    if {[catch {
		session find key ""	;# get a deleted session slot
	    } slot]} {
		# no empty slots - create a new slot
		set slot [session append {*}$session _key $key _ctime $now _mtime $now]
		session set $slot _slot $slot	;# fixup session's slot
		variable size; incr size
	    } else {
		# use a deleted session slot - write session content
		session set $slot {*}$session _key $key _ctime $now _mtime $now _slot $slot
		variable empty; incr empty -1
	    }
	}

	# add the accessor cookie to the request
	variable cookie; variable cpath
	dict set req -cookies [Cookies add [dict get $req -cookies] -path $cpath {*}$args -name $cookie -value [list $slot $key]]

	return $req
    }

    # delete current session
    proc /_sdel {r} {
	return [Http NoCache [Http Ok [remove $r] "Session removed" text/plain]]
    }

    # show current session's values
    proc /_sshow {r} {
	set content [<table class session border 1 width 80% [subst {
	    [<tr> [<th> "Session"]]
	    For {n v} [Dict get? $r -session] {
		[<tr> "[<td> $n] [<td> [armour $v]]"]
	    }
	}]]

	return [Http NoCache [Http Ok $r $content x-text/html-fragment]]
    }

    # this is the default wildcard proc for Sessions
    # it does nothing, but it causes the session to be
    # presented to the server
    proc / {r} {
	return [Http NoCache [Http Ok $r "" text/plain]]
    }

    variable subdomains {}
    proc do {rq} {
	variable cpath	;# use the cookie path for our path
	variable subdomains	;# set of domains to traverse
	# TODO: this could be much more efficient using arrays to
	# match direct domains, rather than iterating through Direct
	foreach sd [list {*}$subdomains ::Session] {
	    set rsp [Direct::_do $sd x-text/html-fragment $cpath "" $rq]
	    if {[dict get $rsp -code] <= 400} {
		return $rsp
	    }
	}
	return [Http NotFound $rq]
    }

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
	} views eo]} {
	    # this is the top level process, need to create a db
	    variable toplevel 1
	    variable db
	    catch {mk::file open db $db -shared}
	}
	View init session db.session

	gc	;# start up by garbage collecting Session db.
    }

    namespace export -clear *
    namespace ensemble create -subcommands {} -map {}
}
