# HttpC - HTTP 1.1 client

# import the relevant commands
namespace eval ::tcl::unsupported {namespace export coroutine yield infoCoroutine}
namespace import ::tcl::unsupported::coroutine ::tcl::unsupported::yield ::tcl::unsupported::infoCoroutine

lappend ::auto_path [file join [file dirname [file dirname [file normalize [info script]]]] Utilities]

package require Url

package require Debug
Debug off HttpC 10

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

    # Yield wrapper with command dispatcher
    proc Yield {{retval ""}} {
	upvar \#1 timer timer timeout timeout cmd cmd consumer consumer
	set time $timeout
	while {1} {
	    if {$time > 0} {
		lappend timer [after $time ::HttpC::$cmd [list [list TIMEOUT $cmd]]]	;# set new timer
	    }
	    Debug.HttpC {timer '$cmd' $timer}

	    # wait for an event
	    set yield [::yield $retval]	;# wait for some input
	    set args [lassign $yield op]
	    Debug.HttpC {yield '$cmd' ($retval) -> ($yield)}

	    # cancel all outstanding timers for this coro
	    foreach t $timer {
		catch {
		    after cancel $t	;# cancel old timer
		} e eo
		Debug.HttpC {cancel '$cmd' $t - $e ($eo)}
	    }
	    set timer {}

	    # dispatch on command
	    switch -- [string toupper $op] {
		TIMEOUT {
		    # we've timed out - oops
		    if {[catch {
			$consumer [list TIMEOUT $cmd]
		    }] && [info commands ::HttpC::$consumer] == {}} {
			Debug.HttpC {reader: consumer error or gone on EOF}
			return -code return
		    }
		    set time -1
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
	upvar \#1 socket socket sent sent cmd cmd host host port port http http
	Debug.HttpC {send $cmd $method ($args)}

	set T [dict merge $http $args [Url parse $url]]
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

    variable template {
	accept */*
    }

    # writer - coro to send HTTP requests to a server
    variable writer {
	Debug.HttpC {writer: $args}
	if {[llength $args]%2} {
	    set ops [list get [lindex $args end]]
	    set args [lrange $args 0 end-1]
	}

	# unpack all the passed-in args
	set http {}
	foreach {_var _val} $args {
	    if {[string tolower $_var] in {get put post delete}} {
		# collect protocol operations
		lappend ops [string toupper $_var] $_val
	    } else {
		set $_var $_val
	    }
	}

	# construct a request template
	variable template
	set http [dict merge $template $http]
	dict set http User-Agent "HttpC/[package present HttpC]"
	#lappend http accept-encoding gzip

	# send any pending ops
	if {[info exists ops]} {
	    Debug.HttpC {initial ops: $ops}
	    foreach {_op _val} $ops {
		set entity [lassign $_val url]
		send $_op $url {*}$entity
	    }
	}

	while {1} {
	    set result [Yield]
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

    proc Gets {} {
	upvar \#1 socket socket

	set result [Yield]
	while {[chan gets $socket line] != -1
	       && [chan blocked $socket]
	   } {
	    set result [Yield]
	}

	if {![chan eof $socket]} {
	    Debug.HttpC {Gets: '$line' [chan blocked $socket] [chan eof $socket]}
	    return $line
	}

	upvar \#1 consumer consumer

	# socket has closed
	catch {chan event $socket readable {}}
	catch {chan event $socket writeable {}}
	if {[catch {
	    $consumer {EOF HEADER}
	}] && [info commands ::HttpC::$consumer] == {}} {
	    Debug.HttpC {reader: consumer error or gone on EOF}
	    return -code return
	}
    }

    proc Read {size} {
	upvar \#1 socket socket
	set chunk ""
	while {$size && ![chan eof $socket]} {
	    set result [Yield]
	    set chunklet [chan read $socket $size]
	    incr size -[string length $chunklet]
	    append chunk $chunklet
	}

	if {![chan eof $socket]} {
	    Debug.HttpC {Read: '$chunk'}
	    return $chunk
	}

	# socket has closed while reading entity
	upvar \#1 consumer consumer
	catch {chan event $socket readable {}}
	catch {chan event $socket writeable {}}
	if {[catch {
	    $consumer {EOF ENTITY}
	}] && [info commands ::HttpC::$consumer] == {}} {
	    Debug.HttpC {reader: consumer error or gone on EOF}
	    return -code return
	}
    }

    variable reader {
	Debug.HttpC {reader: $args}
	# unpack all the passed-in args
	dict with $args {}

	# keep receiving input resulting from our requests
	while {1} {
	    set r {}	;# empty header
	    # get whole header
	    set headering 1
	    while {$headering} {
		set lines {}
		while {$headering} {
		    set line [Gets]
		    Debug.HttpC {reader got line: ($line)}
		    if {[string trim $line] eq ""} {
			set headering 0
		    } else {
			lappend lines $line
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
		chan configure $socket -translation {binary binary}
		Debug.HttpC {reader getting entity of length ($left)}
		while {$left > 0} {
		    set chunk [Read $left]
		    incr left -[string length $chunk]
		    Debug.HttpC {reader getting remainder of entity of length ($left)}
		    dict append r -entity $chunk
		    Debug.HttpC {reader got whole entity}
		}

	    } elseif {[dict exists $r transfer-encoding]} {
		switch -- [dict get $r transfer-encoding] {
		    chunked {
			set chunksize 1
			while {$chunksize} {
			    chan configure $socket -translation {crlf binary}
			    set chunksize 0x[Gets]
			    chan configure $socket -translation {binary binary}
			    if {$chunksize eq "0x"} {
				Debug.HttpC {Chunks all done}
				break
			    }
			    set chunk [Read $chunksize]
			    Gets
			    Debug.HttpC {Chunk: $chunksize ($chunk)}
			    dict append r -entity $chunk
			}
		    }
		    default {
			error "Unknown transfer encoding"
		    }
		}
	    }

	    # reset to header config
	    chan configure $socket -encoding binary -translation {crlf binary}

	    if {[info commands ::HttpC::$consumer] == {}} {
		Debug.HttpC {reader: consumer gone $consumer}
		return
	    }

	    after 1 ::HttpC::$consumer [list [list INCOMING $r]]
	    Debug.HttpC {reader: sent response, waiting for next}
	}
    }

    # receiver and transmitter timeouts - by default none
    variable rxtimeout -1
    variable txtimeout -1

    proc connect {consumer url args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}

	# default timeouts
	variable txtimeout; variable rxtimeout
	set args [dict merge [list txtime $txtimeout rxtime $rxtimeout] $args]

	# parse url
	set urld [Url parse $url]
	dict set args host [dict get $urld -host]
	if {[dict exists $urld -port]} {
	    dict set args port [dict get $urld -port]
	} else {
	    dict set args port 80 
	}

	# create the socket
	set socket [socket -async [dict get $args host] [dict get $args port]]

	# condition the socket
	chan configure $socket -blocking 0 -buffering none -encoding binary -translation {crlf binary}

	# construct consumer
	set cr C[uniq]
	coroutine $cr ::apply $consumer

	# create reader coroutine
	variable reader
	chan event $socket readable [list ::HttpC::${socket}R READ]
	coroutine ${socket}R ::apply [list args $reader ::HttpC] socket $socket timeout [dict get $args rxtime] writer ${socket}W cmd ${socket}R consumer $cr {*}$args

	# create writer coroutine
	variable writer
	coroutine ${socket}W ::apply [list args $writer ::HttpC] socket $socket timeout [dict get $args txtime] cmd ${socket}W reader ${socket}R consumer $cr {*}$args get $url

	return ::HttpC::${socket}W
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    HttpC connect {{args} {
	while {1} {
	    set args [lassign [yield] op]
	    if {$op eq "EOF"} {
		puts stderr "EOF Connection closed - leaving consumer"
		return
	    }
	    puts stderr "$op: $args"
	}
	puts stderr "consumer fallen through - can't happen"
    }} http://www.google.com.au/ txtime 5000 rxtime 5000

    set done 0
    vwait done
}
