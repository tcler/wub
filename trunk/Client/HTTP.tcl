# HTTP 1.1 client

package require TclOO
namespace import oo::*

if {[info exists argv0] && ($argv0 eq [info script])} {
    set wubdir [file dirname [file dirname [file normalize [info script]]]]
    lappend ::auto_path [file join $wubdir Utilities] [file join $wubdir extensions]
}
# import the relevant commands
package require Http
package require Url

package require Debug
Debug on HTTP 10

package require know
know {[string match http://* [lindex $args 0]]} {
    HTTP new {*}$args
}

package provide HTTP 1.0

class create HTTP {

    # send - send an op HTTP request to the server
    method send {method url {entity ""} args} {
	corovars socket sent host http
	Debug.HTTP {send method:$method url:$url entity: [string length $entity] ($args)}

	set T [dict merge $http $args [Url parse $url]]
	set T [dict merge $T [list -method $method date [Http Date] host $host]]

	# format entity
	if {$entity ne ""} {
	    # encode entity body
	    dict set T content-length [string length $entity]
	} else {
	    unset entity
	}

	# format up header
	set request "$method [Url http $T] HTTP/1.1\r\n"
	dict for {n v} [dict filter $T key {[a-zA-Z]*}] {
	    if {[string length $v] > 100} {
		# break long lines into partial lines
		set sv {}
		while {[string length $v] > 100} {
		    lappend sv [string range $v 0 99]
		    set v [string range $v 100 end]
		}
		set v [join $sv "\r\n "]
	    }
	    append request "$n: $v\r\n"
	}
	append request "\r\n"	;# signal end of header
	chan puts -nonewline $socket $request

	if {[info exists entity]} {
	    # send the entity
	    chan puts -nonewline $socket $entity
	}
	incr outstanding
	chan event $socket readable [list [self] reader READ]

	Debug.HTTP {sent $method $url - $outstanding outstanding}
    }

    method parse {lines} {
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

    method gets {} {
	corovars socket

	set line ""
	while {![eof $socket]
	       && [chan gets $socket line] != -1
	       && [chan blocked $socket]
	   } {
	    yield
	}

	if {[chan eof $socket]} {
	    set reason "EOF reading HEADER"
	    [self] destroy
	} else {
	    Debug.HTTP {gets: '$line' [chan blocked $socket] [chan eof $socket]}
	    return $line
	}
    }

    method read {size} {
	corovars socket
	Debug.HTTP {Reading $size}
	set chunk ""
	while {$size && ![chan eof $socket]} {
	    yield	;# wait for read event
	    set chunklet [chan read $socket $size]	;# get some
	    append chunk $chunklet			;# remember it
	    incr size -[string length $chunklet]	;# how much left?
	}

	if {[chan eof $socket]} {
	    set reason "EOF reading ENTITY"
	    [self] destroy
	} else {
	    # we have successfully read our chunk of $size
	    Debug.HTTP {Read: '$chunk' of size $size}
	    return $chunk
	}
    }

    variable closing outstanding reader writer consumer socket reason

    destructor {
	Debug.HTTP {[self]: $socket closed because: $reason}

	catch {close $socket}

	# alert consumer
	if {[catch {
	    after 1 $consumer [list [list CLOSING [info coroutine] $reason]]
	}] && [info commands $consumer] == {}} {
	    Debug.HTTP {reader: consumer error or gone on EOF}
	}
    }

    constructor {url _consumer args} {
	set closing 0		;# signals EOF to both reader and writer
	set outstanding 0	;# counts outstanding packets
	set reason "none given"	;# reason for closure
	set consumer $_consumer	;# who's consuming this?

	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
 
	# receiver and transmitter timeouts - by default none
	set rxtimeout 0; dict unset args rxtimeout
	set txtimeout 0; dict unset args txtimeout
	set template {accept */*}	;# http template

	set ops [list get $url]
	foreach {n v} $args {
	    if {$n in {get put post delete close}} {
		lappend ops $n $v
	    } else {
		set $n $v
	    }
	}

	# construct consumer if necessary
	if {[llength $consumer] > 1} {
	    lassign $consumer argl function ns

	    # make functional's ns relative to parent dir
	    if {$ns eq ""} {
		set ns [uplevel namespace current]
	    } elseif {![string match :* $ns]} {
		set ns [uplevel namespace current]::$ns
	    }

	    coroutine consumer ::apply [list $argl $function $ns]
	}

	# parse url
	set urld [Url parse $url]
	set host [dict get $urld -host]
	if {[dict exists $urld -port]} {
	    set port [dict get $urld -port]
	} else {
	    set port 80 
	}

	# create the socket
	set socket [socket -async $host $port]
	
	# condition the socket
	chan configure $socket -blocking 0 -buffering none -encoding binary -translation {crlf binary}
	
	# create reader coroutine
	set reader [self]::${socket}R
	coroutine $reader ::apply [list args {
	    Debug.HTTP {reader: $args}
	    # unpack all the passed-in args
	    dict with args {}
	    yield

	    # keep receiving input resulting from our requests
	    while {![eof $socket]} {
		set r {}	;# empty header
		# get whole header
		set headering 1
		while {$headering} {
		    set lines {}
		    while {$headering} {
			set line [my gets]
			Debug.HTTP {reader got line: ($line)}
			if {[string trim $line] eq ""} {
			    set headering 0
			} else {
			    lappend lines $line
			}
		    }
		}
		
		# got the header
		set header [lindex $lines 0]
		set r [my parse [lrange $lines 1 end]]	;# parse the header

		# split out some interesting parts of the first header line
		dict set r -message [join [lassign [split $header] version code]]
		dict set r -version $version
		dict set r -code $code
		Debug.HTTP {reader header: $header ($r)}
		
		# now we have to fetch the entity (if any)
		if {[dict exists $r content-length]} {
		    set left [dict get $r content-length]
		    set entity ""
		    chan configure $socket -translation {binary binary}
		    Debug.HTTP {reader getting entity of length ($left)}
		    while {$left > 0} {
			set chunk [my read $left]
			incr left -[string length $chunk]
			Debug.HTTP {reader getting remainder of entity of length ($left)}
			dict append r -content $chunk
			Debug.HTTP {reader got whole entity}
		    }
		} elseif {[dict exists $r transfer-encoding]} {
		    switch -- [dict get $r transfer-encoding] {
			chunked {
			    set chunksize 1
			    while {$chunksize} {
				chan configure $socket -translation {crlf binary}
				set chunksize 0x[my gets]
				chan configure $socket -translation {binary binary}
				if {!$chunksize} {
				    my gets
				    Debug.HTTP {Chunks all done}
				    break
				}
				set chunk [my read $chunksize]
				my gets	;# get the closing \n
				Debug.HTTP {Chunk: $chunksize ($chunk)}
				dict append r -content $chunk
			    }
			}
			default {
			    error "Unknown transfer encoding"
			}
		    }
		}
		
		# reset to header config
		chan configure $socket -encoding binary -translation {crlf binary}

		# hand consumer the result
		variable consumer; after 1 [list $consumer [list RESPONSE $r]]

		# count the outstanding responses left
		# close if there are none
		variable outstanding
		incr outstanding -1
		Debug.HTTP {outstanding: $outstanding}

		variable closing
		if {$closing && !$outstanding} {
		    set reason "requested by WRITER"
		    [self] destroy
		} elseif {!$outstanding} {
		    # nothing to read
		    chan event $socket readable {}
		}
		Debug.HTTP {reader: sent response, waiting for next}
		yield
	    }
	    [self] destroy
	} [self]] socket $socket timeout $rxtimeout
	objdefine [self] forward reader $reader	;# forward the method to the coro

	# create writer coroutine
	set writer [self]::${socket}W 
	coroutine $writer ::apply [list args {
	    # writer - coro to send HTTP requests to a server
	    Debug.HTTP {writer: $args}
	    
	    # unpack all the passed-in args
	    set ops {}
	    set http {}
	    foreach {var val} $args {
		if {[string tolower $var] in {get put post delete close}} {
		    # collect protocol operations
		    lappend ops [string tolower $var] $val
		} elseif {$var eq "ops"} {
		    lappend ops {*}$val
		} else {
		    set $var $val
		}
	    }
	    
	    # construct a request template
	    set http [dict merge $template $http]
	    dict set http User-Agent "HTTP/[package present HTTP]"
	    #lappend http accept-encoding gzip
	    
	    # send any ops we were passed
	    if {[info exists ops]} {
		Debug.HTTP {initial ops: $ops}
		foreach {op val} $ops {
		    if {$op eq "close"} {
			# we've been asked to close
		    }
		    set entity [lassign $val url]
		    my send $op $url {*}$entity
		}
	    }

	    variable closing
	    set retval ""
	    while {!$closing} {
		# unpack event
		if {[catch {
		    set args [lassign [::yield $retval] op]; set retval ""
		} e eo]} {
		    Debug.HTTP {[info coroutine] yield: $e ($eo)}
		    return
		}
		
		Debug.HTTP {writer $cmd -> ($result) ($eof)}
		set op [string tolower $op]
		if {$closing || $op eq "close"} {
		    Debug.HTTP {close: $op / $closing}
		    variable reason "Requested by Consumer"
		    set closing 1
		    return
		} elseif {$op in {get put post delete}} {
		    # got a protocol operator from consumer
		    set entity [lassign $args url]
		    my send $op $url {*}$entity
		}
	    }
	} [self]] socket $socket timeout $txtimeout ops $ops template $template host $host
	objdefine [self] forward writer $writer	;# forward the method to the coro

	return $writer
    }
    
    namespace export -clear *
    namespace ensemble create -subcommands {}
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    proc echo {arg} {
	puts "ECHO: $arg"
    }
    http://www.google.com.au/ echo get http://www.google.com.au/

    set done 0
    vwait done
}
