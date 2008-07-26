# Listener
#
# Listener is a network server which listens for connection attempts and
# dispatches the connected socket to a handler
#
# This implementation dispatches to a pool of objects which
# interact at the protocol level, to provide a network service.
#
# NOTE: there's no need for the $socket [get] [connect] [Exhausted] interface - just a single [connect] which handles the rest.

if {[info exists argv0] && ($argv0 eq [info script])} {
    # test Listener
    lappend auto_path [pwd]
}

package require WubUtils
package require Debug
package provide Listener 2.0

namespace eval Listener {
    variable listeners; array set listeners {}	;# listener by port

    # accept --
    #
    #	This is the socket accept callback invoked by Tcl when
    #	clients connect to the server.
    #
    # Arguments:
    #   opts	a dict containing listener options
    #	sock	The new socket connection
    #	ipaddr	The client's IP address
    #	port	The client's port
    #
    # Results:
    #	none
    #
    # Side Effects:
    #	Set up a handler, HttpdRead, to read the request from the client.
    #	The per-connection state is kept in Httpd$sock, (e.g., Httpdsock6),
    #	and upvar is used to create a local "data" alias for this global array.

    proc accept {opts sock ipaddr rport} {
	Debug.socket {accepted: $sock $ipaddr $rport}

	if {[catch {
	    # select an Http object to handle incoming
	    {*}[dict get $opts -httpd] Connect $sock $ipaddr $rport {*}$opts
	} result eo]} {
	    Debug.error {accept: $eo}
	}
    }

    variable id 0	;# listener id

    proc listen {args} {
	variable id
	if {[catch {
	    set args [dict merge [subst {
		-host [info hostname]
		-port 8015
		-httpd Httpd
		-id [incr id]
	    }] $args]

	    if {![dict exists $args -tls]} {
		set cmd [list socket -server [list ::Listener::accept $args]]
	    } else {
		package require tls
		puts stderr "TLS:$args"
		dict set args -tls [dict merge {
		    -ssl3 1 -ssl2 0 -tls1 1
		} [dict get $args -tls]]
		dict set args -certfile server-public.pem
		dict set args -keyfile  server-private.pem
		set cmd [list tls::socket -server [list ::Listener::accept $args] -command ::Listener::progress {*}[dict get $args -tls]]
	    }
	    
	    if {[dict exists $args -myaddr] &&
		[dict get $args -myaddr] != 0
	    } {
		lappend cmd -myaddr [dict get $args -myaddr]
	    }
	    
	    lappend cmd [dict get $args -port]

	    Debug.socket {server: $cmd}
	    puts stderr "LL: $cmd"
	    if {[catch $cmd listen eo]} {
		error "[dict get $args -host]:[dict get $args -port] $listen\ncmd=$cmd"
	    }
	    variable listeners; set listeners([dict get $args -port]) $id

	} error eo]} {
	    Debug.error {constructor err: $eo}
	}
    }

    proc destroy {} {
	variable listeners
	foreach listen [array names listeners] {
	    catch {close $listen}
	}
    }

    proc progress {args} {
	puts stderr "TLS: $args"
	return 1
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    # test Listener with Httpd
    package require Httpd
    package require Query

    proc Dispatch {req} {
	#puts stderr "Dispatcher: $req"

	set http [dict get $req -http]
	{*}$http Restart $req	;# assume it's not a POST

	# clean out some values
	dict unset req cache-control

	set c {
	    <html>
	    <head>
	    <title>Test Page</title>
	    </head>
	    <body>
	    <h1>Test Content</h1>
	}

	append c "<table border='1' width='80%'>" \n
	append c <tr> <th> metadata </th> </tr> \n
	dict for {n v} $req {
	    if {[string match -* $n]} {
		append c <tr> <td> $n </td> <td> $v </td> </tr> \n
	    }
	}
	append c </table> \n

	append c "<table border='1' width='80%'>" \n
	append c <tr> <th> "HTTP field" </th> </tr> \n
	dict for {n v} $req {
	    if {![string match -* $n]} {
		append c <tr> <td> $n </td> <td> $v </td> </tr> \n
	    }
	}
	append c </table> \n

	append c "<table border='1' width='80%'>" \n
	append c <tr> <th> "Query field" </th> </tr> \n
	dict for {n v} [Query flatten [Query parse $req]] {
	    append c <tr> <td> $n </td> <td> $v </td> </tr> \n
	}
	append c </table> \n

	append c {
	    </body>
	    </html>
	}

	$http Respond 200 [dict replace $req -content $c \
			       warning "199 Moop 'For fun'" \
			       content-type text/html \
			  ]
    }

    # start Listener
    set listener [Listener %AUTO% -port 8080 -dispatcher Dispatch]
    #puts stderr "Listener $listener"

    package require Stdin

    set forever 0
    vwait forever
}
