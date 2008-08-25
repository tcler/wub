# HttpC - HTTP 1.1 client

# import the relevant commands
namespace eval ::tcl::unsupported {namespace export coroutine yield}
namespace import ::tcl::unsupported::coroutine ::tcl::unsupported::yield

lappend auto_path [pwd]	;# path to the Site.tcl file

set home [file normalize [file dirname [info script]]]
set topdir [file dirname $home]

foreach lib {extensions Utilities} {
    lappend ::auto_path [file join $topdir $lib]
}

package require Url

package require Debug
Debug on HttpC 10

package provide HttpC 1.0

proc bgerror {args} {
    #puts stderr "bgERROR: $args"
    Debug.error {bgerror: $args}
}
interp bgerror {} bgerror

namespace eval HttpC {
    variable uniq [pid]	;# seed for unique coroutine names

    # give a uniq looking name
    proc uniq {} {
	variable uniq
	return [incr uniq]
    }

    # yield wrapper with command dispatcher
    proc yield {{retval ""}} {
	upvar \#1 timer timer timeout timeout cmd cmd

	while {1} {
	    set timer [after $timeout $cmd TIMEOUT]	;# set new timer
	    set yield [::yield $retval]	;# wait for some input
	    catch {after cancel $timer}	;# cancel old timer

	    Debug.HttpC {yield $cmd ($retval) -> ($yield)}
	    lassign $yield op args
	    set op [string toupper $op]
	    switch -- $op {
		TIMEOUT {
		    # we've timed out - oops
		    puts stderr "TIMEOUT $cmd"
		    error "Timeout $cmd"
		}

		KILL {
		    return -code return $args
		}

		BREAK {
		    return -code break $args
		}

		READ {
		    # this can only happen in the reader coro
		    return $args
		}

		default {
		    # this can only happen in the writer coro
		    # where $op should be one of the HTTP operations
		    # sent by an external process
		    send $op {*}$args
		}
	    }
	}
    }

    # return an HTTP date
    proc Date {{seconds ""}} {
	if {$seconds eq ""} {
	    set seconds [clock seconds]
	}

	return [clock format $seconds -format {%a, %d %b %Y %T GMT} -gmt true]
    }

    # send - send an op HTTP request to the server
    proc send {method url args} {
	upvar \#1 socket socket sent sent cmd cmd host host port port template template
	Debug.HttpC {send $cmd $method ($args)}

	set T [dict merge $template $args [Url parse $url]]
	set T [dict merge $T [list -method $method date [Date] -port $port -host $host host $host]]

	# format entity
	if {[dict exists $T -entity]} {
	    # encode entity body
	    set body [dict get $T -entity]
	    dict set T content-length [string length $body]
	}

	# format up header
	set header "$method [Url http $T] HTTP/1.1\r\n"
	dict for {n v} [dict filter $T key {[a-zA-Z]*}] {
	    if {[string length $v] > 100} {
		set sv {}
		while {[string length $v] > 100} {
		    lappend sv [string range $v 0 99]
		    set v [string range $v 100 end]
		}
		set v [join $sv "\r\n "]
	    }
	    append header "$n: $v\r\n"
	}

	if {[info exists body]} {
	    append header \r\n $body
	} else {
	    append header \r\n
	}
	Debug.HttpC {send ($header)}
	chan puts -nonewline $socket $header
    }

    # writer - coro to send HTTP requests to a server
    variable writer {
	Debug.HttpC {writer: $args}
	if {[llength $args]%2} {
	    set ops [list get [lindex $args end]]
	    set args [lrange $args 0 end-1]
	}

	# unpack all the passed-in args
	foreach {_var _val} $args {
	    if {[string tolower $_var] in {get put post delete}} {
		# collect protocol operations
		lappend ops [string toupper $_var] $_val
	    } else {
		set $_var $_val
	    }
	}
	set cmd [lindex [info level 0] 0]	;# who are we?
	set timer [after $timeout $cmd TIMEOUT]

	# construct a request template
	dict set template User-Agent "HttpC/[package present HttpC]"
	#lappend template accept-encoding gzip

	# send any pending ops
	if {[info exists ops]} {
	    Debug.HttpC {initial ops: $ops}
	    foreach {_op _val} $ops {
		set entity [lassign $_val url]
		send $_op $url {*}$entity
	    }
	}

	while {1} {
	    set result [yield]
	    Debug.HttpC {writer $cmd -> ($result)}
	}
    }

    proc parse {lines} {
	# we have a complete header - parse it.
	set r {}
	set last ""
	foreach line $lines {
	    if {[string index $line 0] in {" " "\t"}} {
		# continuation line
		dict append r $last " [string trim $line]"
	    } else {
		set value [join [lassign [split $line ":"] key] ":"]
		set key [string tolower [string trim $key "- \t"]]
		
		if {[dict exists $r $key]} {
		    dict append r $key ",$value"
		} else {
		    dict set r $key [string trim $value]
		}
	    }
	}
	return $r
    }

    variable reader {
	Debug.HttpC {reader: $args}
	# unpack all the passed-in args
	foreach {_var _val} $args {
	    set $_var $_val
	}

	# construct consumer
	set _consumer ${uniq}C
	coroutine ${uniq}C ::apply $consumer

	set cmd [lindex [info level 0] 0]	;# who are we?
	set timer [after $timeout $cmd TIMEOUT]

	# keep receiving input resulting from our requests
	while {1} {
	    # get whole header
	    set headering 1
	    while {$headering} {
		set result [yield]
		while {$headering && [chan gets $socket line] != -1} {
		    Debug.HttpC {reader got line: ($line)}
		    if {[string trim $line] eq ""} {
			set headering 0
		    } else {
			lappend lines $line
		    }
		}
		if {[chan eof $socket]} {
		    # socket has closed
		    if {[catch {
			$_consumer {EOF HEADER}
		    }] && [info command ::HttpC::$_consumer] == {}} {
			Debug.HttpC {reader: consumer error or gone on EOF}
			return
		    }
		}
	    }

	    set header [lindex $lines 0]
	    set r [parse [lrange $lines 1 end]]	;# parse the header
	    dict set r -message [join [lassign [split $header] version code]]
	    dict set r -version $version
	    dict set r -code $code
	    Debug.HttpC {reader header: $header ($r)}

	    # now we have to fetch the entity (if any)
	    if {[dict exists $r content-length]} {
		set left [dict get $r content-length]
		set entity ""
		chan configure $socket -encoding binary -translation {binary binary}
		Debug.HttpC {reader getting entity of length ($left)}
		while {$left > 0} {
		    set result [yield]
		    set chunk [chan read $socket $left]
		    incr left -[string length $chunk]
		    Debug.HttpC {reader getting remainder of entity of length ($left)}
		    append entity $chunk
		    if {[chan eof $socket]} {
			# socket has closed while reading entity
			if {[catch {
			    $_consumer {EOF ENTITY}
			}] && [info command ::HttpC::$_consumer] == {}} {
			    Debug.HttpC {reader: consumer error or gone on EOF}
			    return
			}
		    }
		    Debug.HttpC {reader got whole entity}
		}

		# we have the entire entity
		dict set r -entity $entity
	    }

	    if {[info command ::HttpC::$_consumer] == {}} {
		Debug.HttpC {reader: consumer gone}
		return
	    }

	    after 1 ::HttpC::$_consumer [list $r]
	    Debug.HttpC {reader: sent response, waiting for next}
	}
    }

    variable rxtimeout 100000
    variable txtimeout 100000

    proc connect {consumer url args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}

	set urld [Url parse $url]
	dict set args host [dict get $urld -host]
	if {[dict exists $urld -port]} {
	    dict set args port [dict get $urld -port]
	} else {
	    dict set args port 80 
	}
	dict set args consumer $consumer

	# create the socket
	set socket [socket -async [dict get $args host] [dict get $args port]]

	# condition the socket
	chan configure $socket -blocking 0 -buffering none -encoding binary -translation {crlf binary}

	# create the reader and writer coroutines
	set uniq [uniq]

	variable reader; variable rxtimeout
	chan event $socket readable [list ::HttpC::${uniq}R READ]
	coroutine ${uniq}R ::apply [list args $reader ::HttpC] socket $socket timeout $rxtimeout writer ${uniq}W uniq $uniq {*}$args

	variable writer; variable txtimeout
	coroutine ${uniq}W ::apply [list args $writer ::HttpC] socket $socket timeout $txtimeout reader ${uniq}R {*}$args get $url
	return ::HttpC::${uniq}W
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    HttpC connect {{args} {
	while {1} {
	    set args [lassign [yield] op]
	    if {$op eq "EOF"} {
		puts stderr "Connection closed"
		return
	    }
	    puts stderr "GOT: $args"
	}
	puts stderr "consumer fallen through - can't happen"
    }} http://google.com/

    set done 0
    vwait done
}
