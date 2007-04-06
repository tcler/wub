# Listener
#
# Listener is a network server which listens for connection attempts and
# dispatches the connected socket to one of a pool of handler objects of
# -type type
#
# This implementation dispatches to a pool of objects which
# interact at the protocol level, to provide a network service.

if {[info exists argv0] && ($argv0 eq [info script])} {
    # test Listener
    lappend auto_path [pwd]
}

package provide Listener 1.0

package require WubUtils
package require logger
package require snit
package require Pool

snit::type Listener {
    variable listen	;# socket upon which to accept connections

    option -server "Wub 2.0"	;# server name
    option -host [info hostname]	;# host name
    option -port 8015	;# port for listener to listen on
    option -myaddr 0	;# ip address to listen on
    option -type Httpd  ;# URI scheme - also controls socket handler type
    option -dispatcher "Host Dispatch"
    option -pool Pool	;# Pool handler
    option -sockets ""	;# optional Pool for handler allocation
    option -tls ""	;# tls args
    option -httpd ""	;# httpd args

    # pool of sockets for this listener
    # note: this provides the release method
    # to return sockets to the pool
    component sockets -inherit true
    delegate option -maxsize to sockets

    # base logger
    option -logger ::
    variable log

    option -retryafter 5	;# retry period for socket exhaustion

    method log {cmd args} {
        ${log}::$cmd {*}$args
    }

    method Count {args} {}
    method CountStart {args} {}
    method CountHist {args} {}
    method CountName {args} {}

    # accept --
    #
    #	This is the socket accept callback invoked by Tcl when
    #	clients connect to the server.
    #
    # Arguments:
    #	self	A list of {protocol name port} that identifies the server
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

    method accept {sock ipaddr rport} {
	Debug.socket {$self accepted: $sock $ipaddr $rport}

	if {[catch {
	    $self Count accepts sockets

	    # start tls on port
	    if {$options(-tls) ne ""} {
		::tls::import $sock -command tls::progress {*}$options(-tls)
	    }

	    # select an Http object to handle incoming
	    if {[catch {$sockets get sock $ipaddr $rport} http eo]} {
		puts stderr "accept failed $http ($eo)"
		# we have exceeded per-listener pool size
		catch {
		    $option(-type) Exhausted $sock $eo $options(-retryafter)
		}
		flush $sock
		close $sock
	    } else {
		Debug.socket {accept "$http connect -listener $self \
				  $options(-httpd) \
				  -ipaddr $ipaddr \
				  -rport $rport \
				  -sock $sock"}

		# tell the Httpd object it's now connected
		{*}$http connect -listener $self \
		    {*}$options(-httpd) \
		    -ipaddr $ipaddr \
		    -rport $rport \
		    -sock $sock
		Debug.socket {accept complete: $http}
	    }
	} result eo]} {
	    ${log}::error "$self accept: $eo"
	}
    }

    method debuglog {on} {
	if {$on} {
	    ${log}::enable debug
	} else {
	    ${log}::disable debug
	}
	
    }

    constructor {args} {
	if {[catch {
	    $self configurelist $args

	    set log [logger::init $options(-host)$options(-port)]
	    ${log}::disable debug
	    #${log}::enable debug

	    if {$options(-sockets) eq ""} {
		install sockets using $options(-pool) ${self}_pool -maxsize 257 \
		    -constructor [list $options(-type) %AUTO% \
				      -logger $log \
				      -dispatcher $options(-dispatcher)]
	    } else {
		# creator supplied a readymade Pool
		# useful for Enthread usage
		set sockets $options(-sockets)
	    }

	    if {$options(-tls) eq ""} {
		set cmd [list socket -server [list $self accept]]
	    } else {
		::tls::init \
		    -ssl2 1 \
		    -ssl3 1 \
		    -tls1 0 \
		    -require 0 \
		    -request 0

		set cmd [list tls::socket -server [list $self accept] -command tls::progress {*}$options(-tls)]
	    }

	    if {$options(-myaddr) != 0} {
		lappend cmd -myaddr $options(myaddr)
	    }

	    lappend cmd $options(-port)

	    Debug.socket {server: $cmd}
	    if {[catch $cmd listen eo]} {
		error "$options(-host):$options(-port) $listen\ncmd=$cmd"
	    }
	} error eo]} {
	    puts stderr "$self constructor err: $eo"
	    ${log}::error "$self constructor err: $eo"
	}
    }

    destructor {
	${log}::notice "Destroying $self"
	catch {close $listen}
	if {[catch {$sockets destroy} result eo]} {
	    ${log}::notice "Error destroying socket pool: $result ($eo)"
	}
	catch {${log}::delete}
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

namespace eval tls {
    proc progress {args} {
	puts stderr "TLS: $args"
	return 1
    }

    proc password {args} {
	puts stderr "Asking password $args"
	return "sample"
    }
}
