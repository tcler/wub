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

package require OO
package require Chan
package require WubUtils
package require Debug
Debug define listener 10
package provide Listener 2.0

set ::API(Server/Listener) {
    {
	Listener is a network server which listens for connection attempts and dispatches the connected socket to a handler.

	== Handler Interface ==

	The protocol handler is a script (specified by -httpd argument, below) which is invoked as follows:

	''handler'' Connect $sock $ipaddr $rport args

	Where '''args''' consists of all the options passed to [Listen listener] when the Listener was created.

	== Creating a Listener ==

	A Listener is created by [[Listen listen]] which takes the following args:

	;-host: host name used in HTTP replies (default: [[info hostname]])
	;-myaddr: ip address to listen on (default: any)
	;-port: port upon which to listen (default ''8015'')
	;-httpd: a script prefix which invokes a handler of an incoming connection (default ''Httpd'')
	;-id: a unique identifier of this listener (default: ''system assigned'')
	;-tls: a list of args passed to tls::socket

	All of the arguments passed to [[Listen listen]] will be passed as args to the protocol handler ''Connect'' command.
	
	Some options bound for the protocol handler are important.  Notably ''-dispatch'' which is specifies the worker script for each request processed by the [Httpd] protocol stack.

	Generating SSL key is very easy, just use these two commands:
	    openssl genrsa -out server-private.pem 1024
	    openssl req -new -x509 -key server-private.pem -out server-public.pem -days 365
    }
}

class create ::Listener {
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

    method accept {opts sock ipaddr rport} {
	Debug.listener {accepted: $sock $ipaddr $rport}

	if {[catch {
	    if {[dict exists $opts -tls]} {
		tls::handshake $sock
	    }
	    # select an Http object to handle incoming
	    {*}[dict get $opts -httpd] Connect $sock $ipaddr $rport {*}$opts
	} result eo]} {
	    Debug.error {accept: $eo}
	}
    }

    method progress {args} {
	puts stderr "TLS: $args"
	return 1
    }

    variable listener

    constructor {args} {
	if {[catch {
	    set args [dict merge [subst {
		-host [info hostname]
		-port 8080
		-httpd Httpd
	    }] $args]
	    dict set args -id [self]

	    if {![dict exists $args -tls]} {
		set cmd [list socket -server [list [self] accept $args]]
	    } else {
		set defaults {
		    -certfile server-public.pem
		    -keyfile server-private.pem
		    -ssl2 1
		    -ssl3 1
		    -tls1 0
		    -require 0
		    -request 0
		}
		set args [dict merge $defaults $args]
		set cmd [list tls::socket -server [list [self] accept $args] -command [list [self] progress] {*}[dict in $args [dict keys $defaults]]]
	    }
	    
	    if {[dict exists $args -myaddr] &&
		[dict get $args -myaddr] != 0
	    } {
		lappend cmd -myaddr [dict get $args -myaddr]
	    }
	    
	    lappend cmd [dict get $args -port]

	    Debug.listener {server: $cmd}
	    if {[catch $cmd listener eo]} {
		Debug.error {Listener Failed: '$cmd' $listener ($eo)}
	    }
	    Debug.log {Listener $listener on [fconfigure $listener]}
	} error eo]} {
	    Debug.error {constructor err: $eo}
	}
    }

    destructor {
	catch {close $listen}
    }
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
