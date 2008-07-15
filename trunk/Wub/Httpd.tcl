# Httpd - near HTTP/1.1 protocol supervisor.
#
# This is the front-end top-level supervisor for
# a collection of HttpdWorker entities which may be running in sub threads.

# fast read starts a series of tasks to cope with each listener.
if {[info exists argv0] && ($argv0 eq [info script])} {
    # test Httpd
    puts stderr "Httpd test"
    lappend auto_path [pwd] ../Utilities/ ../extensions/ ../Utilities/zlib/
    package require Http
}

package require Listener
package require Query
package require struct::queue
package require fileutil
package require Debug
#package require Access
package require Activity
package require Block

package require Cache 2.0
package require Honeypot
package require Html

package provide Httpd 3.0

proc bgerror {args} {
    #puts stderr "bgERROR: $args"
    Debug.error {bgerror: $args}
}
interp bgerror {} bgerror

# define a default [pest] predicate, allow it to be overriden by pest.tcl
proc pest {req} {return 0}
catch {source [file join [file dirname [info script]] pest.tcl]}

namespace eval Httpd {
    variable rqCallOut {}
    variable dispatch "Backend"
    variable server_port ;# server's port (if different from Listener's)
    variable server_id "Wub/[package provide Httpd]"

    proc dump {} {
	variable connection
	set result "Connections: [array get connection]\n"
	append result [dump2]
	return $result
    }

    # Got - worker thread has parsed one request
    # now the dispatcher process (this one) decides how to process
    # the request.
    # By default, it invokes the -dispatch script on the request.
    proc Got {request} {
	Debug.http {Got: $request} 1
	set cid [dict get $request -cid]

	# check the incoming ip for blockage
	if {[Block blocked? [Dict get? $request -ipaddr]]} {
	    dict set request connection close
	    Activity activity blocked $cid $request
	    send [Http Forbidden $request]
	    return
	} elseif {[Honeypot guard request]} {
	    # check the incoming ip for bot detection
	    # this is a bot - reply directly to it
	    Activity activity honeypot $cid $request
	    send $request
	    return
	}

	Activity activity parsed $cid $request

	variable rqCallOut
	if {[llength $rqCallOut] != 0} {
	    # provide for a request callout - called with each parsed request
	    catch {{*}$rqCallOut $request} request
	    # this callout might transform request.
	    # if it unsets -dispatch, the request is the reply
	    # it can also set up -dispatch to whatever it wants
	}

	# check Cache for match
	if {[dict size [set cached [Cache check $request]]] > 0} {
	    # reply from cache
	    dict set cached -transaction [dict get $request -transaction]
	    dict set cached -generation [dict get $request -generation]
	    dict set cached -cid [dict get $request -cid]
	    Activity activity cached $cid $request

	    # send the reply
	    send $cached 0
	} elseif {[dict exists $request -dispatch]} {
	    # dispatch for content
	    # dict set request -Query [Query parse $request] ;# parse the query?
	    # Cookie processing for Session
	    # Session handling

	    {*}[subst [dict get $request -dispatch]] Incoming $request
	} else {
	    # just send the reply as we have it
	    Activity activity replied $cid $request
	    send $request
	}
    }

    # exhausted - method called by Listener to report server exhaustion
    variable retry_wait 20
    proc exhausted {sock {eo {}} {retry 0}} {
	if {$retry == 0} {
	    # use the default retry wait advisory period
	    variable retry_wait
	    set retry $retry_wait
	}

	Debug.socket {Exhausted $sock $eo $retry}

	variable server_id
	puts $sock "HTTP/1.1 503 Exhaustion\r"
	puts $sock "Date: [Http Now]\r"
	puts $sock "Server: $server_id\r"
	puts $sock "Connection: Close\r"
	puts $sock "Retry-After: $retry\r"
	puts $sock "Content-Length: 0\r"
	puts $sock \r
	flush $sock
	close $sock
    }

    # forbidden - method called by Listener to blocked clients
    proc forbidden {sock} {
	Debug.socket {Forbidden $sock}

	variable server_id
	puts $sock "HTTP/1.1 403 Forbidden\r"
	puts $sock "Date: [Http Now]\r"
	puts $sock "Server: $server_id\r"
	puts $sock "Connection: Close\r"
	puts $sock "Content-Length: 0\r"
	puts $sock \r
	flush $sock
	close $sock
    }

    variable cid 0		;# unique connection ID
    variable connection		;# connection state
    array set connection {}

    # connection control
    # when a disconnection and reconnection occurs
    # we can tell because the socket to IP mapping changes
    variable sock2cid		;# socket to connection id
    array set sock2cid {}

    # exhaustion control
    variable max_conn 10	;# max connections per IP
    variable connbyIP		;# count of connections
    array set connbyIP {}
    variable too_many		;# how many times has this IP address been told?
    array set too_many {}
    variable no_really 30	;# after this many max_conns - it's blocked

    # connect - process a connection request
    proc Connect {sock ipaddr rport args} {
	Debug.socket {get thread for reading}

	# the socket must stay in non-block binary binary-encoding mode
	chan configure $sock -blocking 0 -translation {binary binary} -encoding binary

	variable ignore
	if {$ignore} {
	    # we're shutting down, so ignore new requests
	    exhausted $sock
	    return
	}

	# ensure that client is not spamming us.
	# (sadly we can't do this if we're reverse-proxied)
	variable connbyIP
	variable max_conn

	switch -- [::ip::type $ipaddr] {
	    "normal" {
		# check list of blocked ip addresses
		if {[Block blocked? $ipaddr]} {
		    forbidden $sock	;# drop this connection with absolute contempt
		    return
		}

		# normal external connection
		if {[incr connbyIP($ipaddr)] > $max_conn} {
		    # Too many connections for $ipaddr - no more than $max_conn
		    variable too_many
		    variable no_really
		    if {[incr too_many($ipaddr)] > $no_really} {
			# this client has been told repeatedly - block it.
			Block block $ipaddr "Repeatedly too many connections"
		    } else {
			Debug.log {Too many connections for $ipaddr}
		    }
		    exhausted $sock
		    incr connbyIP($ipaddr) -1
		    return
		}
	    }

	    "private" {
		# TODO - this may not be desired behavior.  ReThink
		# just because an ip connection is local doesn't mean it's
		# unlimited, does it?
		# OTOH, it may just be from a local cache, and the original
		# ip address may come from a higher level protocol.
	    }
	}

	# record significant values
	dict set args -sock $sock
	dict set args -ipaddr $ipaddr
	dict set args -rport $rport
	variable cid; set id [incr cid]
	dict set args -cid $id

	# detect and resolve socket conflict
	variable sock2cid
	if {[info exists sock2cid($sock)]} {
	    # this can only happen if the remote's closed the socket
	    # and a new connection has arrived on the same socket
	    # before the worker has informed us.
	    # Call disconnect on old socket owner.
	    Debug.socket {socket $sock reused - $sock2cid($sock)}
	    set oid $sock2cid($sock)
	    disconnect $oid
	    catch {unset connection($oid)}
	}

	# remember details of connection
	variable connection
	set connection($id) [list socket $sock ipaddr $ipaddr]
	set sock2cid($sock) $id

	# get port on which connection arrived
	# this may differ from Listener's port if reverse proxying
	# or transparent ip-level forwarding is performed
	variable server_port
	if {[info exists server_port]} {
	    # use defined server port
	    dict set args -port $server_port
	} else {
	    # use listener's port
	}

	# record some per-server request values
	variable server_id; dict set args -server_id $server_id
	variable dispatch; dict set args -dispatch $dispatch
	dict set args -version 1.1	;# HTTP/1.1

	# process partial request
	associate $args

	# log new connection
	Activity new $cid
	Activity activity connected $cid {*}$args
    }

    # a worker thread has completely processed input, or has hit a socket error
    proc Disconnect {id error {eo ""}} {
	Debug.socket {Disconnect: $id '$error' - ($eo)}

	variable connection
	if {[info exists connection($id)]} {
	    # log the disconnection
	    Activity disconnect $id

	    disassociate $id	;# remove worker association

	    dict with connection($id) {
		# clean up on disconnect
		variable connbyIP; incr connbyIP($ipaddr) -1
		variable sock2cid; unset sock2cid($socket)
	    }

	    unset connection($id)

	    # inform backend of disconnection
	    variable dispatch
	    catch {{*}$dispatch Disconnect $sock}
	}

	# perform quiescent callback if we're now idle
	variable ignore
	if {$ignore && [array size connection] == 0} {
	    # we're quiescent - perform after idle command
	    variable quiescent
	    Debug.log {Idle task: $quiescent}
	    if {$quiescent ne ""} {
		uplevel #0 $quiescent
		set quiescent ""
	    }
	}
    }

    variable ignore 0		;# we're ignoring new connections
    variable quiescent ""	;# we'll do this when quiet

    # Idle - stop accepting new connections, go idle
    # when idle, evaluate the script in variable quiescent
    proc Idle {{then ""}} {
	variable quiescent $then
	variable ignore 1	;# no more connections

	variable connection
	if {[array size connection] == 0} {
	    uplevel #0 $quiescent
	    set quiescent ""
	}
    }

    # Live - start accepting new connections again
    proc Live {} {
	variable ignore 0
    }

    # destroy - destroy the Httpd protocol stacks
    proc destroy {} {
	variable ignore 1	;# stop new incoming

	# release all known sockets
	variable connection
	foreach {id conn} [array get connection] {
	    # release each socket
	    catch {chan close [dict get $conn socket]}
	}

	cleanup	;# clean up worker associations
    }

    # configure - set Httpd protocol defaults
    proc configure {args} {
	variable {*}$args
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

Debug on block 10

if {[info exists argv0] && ($argv0 eq [info script])} {
    package require Stdin
    package require Listener
    package require Debug

    Debug off socket 10
    Debug off http 2
    Debug off cache 10
    Debug off dispatch 10

    set listener [Listener %AUTO% -port 8080 -sockets Httpd -httpd {-dispatch "puts"}]
    set forever 0
    vwait forever
}
