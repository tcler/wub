package require Http
package require Url
package require Debug
package require Cookies

# process background errors by disconnecting
interp bgerror {} bgerror
proc bgerror {error eo} {
    Debug.error {Thread [::thread::id]: $error ($eo)}
    if {[dict get $eo -code] == 1} {
	variable R
	disconnect $error $eo
    }
}

# arrange gzip Content Encoding
if {![catch {package require zlib}]} {
    # we can handle gzip encoding via zlib package
    variable ce_encodings {gzip}
} else {
    # we can't handle gzip encoding
    variable ce_encodings {}
}

variable me [::thread::id]	;# my thread id
variable R {}		;# received message

# create an alias 'parent' for sending messages to parent
interp alias {} parent {} ::thread::send -async $::thread::parent

# template for sent messages
variable template {accept-encoding gzip}

variable sock ""		;# socket for connection
variable stalled 0		;# a POST has been sent, so pipeline stalled.
variable pending 0
variable count -1
variable txcount -1
variable pipeline {}

if {0} {
    # debugging: trace mods to sock
    trace add variable sock write sockmod
    proc sockmod {args} {
	catch {info level -1} l eo
	puts stderr "MOD sock to '$::sock' from '$l'"
    }
}

# two timers are required
package require Timer
Timer txtimer; variable txtime 30000	;# inter-write timeout
Timer rxtimer; variable rxtime 5000	;# inter-read timeout
variable enttime 30000	;# entity inter-read timeout

# armour - rewrite a string to avoid malicious HTML
interp alias {} armour {} string map {& &amp; < &lt; > &gt; \" &quot; ' &#39;}

# next - we're now ready to receive the next response
proc next {} {
    variable R {}	;# discard the response

    # we're now ready to receive the next response
    Debug.socket {gzip readable: getH} 4
    variable sock; chan event $::sock readable getH
}

# connect - connect this thread to a given $host,$port
#
# $args are a dict of protocol elements to add to requests
# (obviously, this facility must be used with care.)
proc connect {_host _port args} {
    Debug.socket {connect: host:$_host port:$_port args:$args}
    if {[llength $args] == 1} {
	set args [lindex $args 0]
    }

    # remember worker's host and port
    variable host $_host port $_port

    # merge $args dict with request template
    variable template
    set template [dict merge $template [list host $_host] $args]
    Debug.socket {template: $template}

    # connect socket, binary non-blocking
    variable sock [socket -async $_host $_port]
    chan configure $sock -translation {binary binary} -blocking 0
    next
}

# indication that we're disconnected
proc disconnect {{msg "closed"} {eo {}}} {
    # cancel the timers, they're meaningless now
    rxtimer cancel; txtimer cancel

    Debug.socket {disconnect readable: {}} 4
    catch {chan event $sock readable {}}	;# completed reading

    # reset protocol state variables
    variable stalled 0 pending 0 pipeline {} template {} count -1 txcount -1
    catch {::close $::sock}

    # inform the parent thread of our disconnection.
    # we'll be returned to the pool by parent
    ::parent [list ::Http::disconnect $::me $msg $eo]
}

# stalled? - enquire as to whether protocol's stalled
# (as by POST pending)
proc stalled? {} {
    variable stalled; return $stalled
}

# identity - read an identity transfer encoded entity off the wire
# copy $content-length bytes into the response -entity field
proc identity {} {
    variable R
    Debug.socket {identity: $R} 3

    # calculate how much entity is left to read, accumulate it
    set clen [dict get $R content-length]
    set diff [expr $clen - [string length [dict get $R -entity]]]
    dict append R -entity [read $::sock $diff]

    if {($clen - [string length [dict get $R -entity]]) == 0} {
	# we've completely received the response R,
	# asynchronously report this to the parent
	::parent [list ::Http::received $::me $R]
	next
    }
}

# stread - pop $len chars from the front of var
proc stread {var {len 1}} {
    upvar var var
    set result [string range $var 0 $len-1]
    set var [string range $var $len end]
    return $result
}

# streadz - pop a null-terminated string from the front of var
proc streadz {var} {
    upvar var var
    set result ""
    while {[set c [stread var]] ne "\0"} {
	lappend result $c
    }
    return $result
}

# decode_gzip - given a response entity which is gzipped,
# unzip it and place it back in the response.
proc decode_gzip {} {
    variable R

    # now gzip decode the content
    set body [dict get $R -entity]
    if {[stread body 2] ne "\x1f\x8b"} {
	# error - content not gzipped
    }
    if {[stread body] ne "\x8"} {
	# error "GZip channel $file: unknown compression method."
    }
    binary scan [stread body] b5 FLAGS
    foreach {FTEXT FHCRC FEXTRA FNAME FCOMMENT} [split $FLAGS ""] {}
    binary scan [stread body 4] i MTIME
    set XFL [stread body]
    set OS [stread body]

    if {$FEXTRA} {
	binary scan [stread body 2] S XLEN
	set EXTRA [stread body $XLEN]
    } else {
	set EXTRA ""
    }

    foreach v {FNAME FCOMMENT} {
	if {[set $v]} {
	    set [string trimleft $v F] [streadz body]
	} else {
	    set [string trimleft $v F] ""
	}
    }

    if {$FHCRC} {
	set HCRC [stread body 2]
    } else {
	set HCRC ""
    }

    binary scan [string range $body end-3 end] i ISIZE
    binary scan [string range $body end-7 end-4] i CRC
    
    set gzip [string range $body 0 end-8]
    dict set R -entity [zlib inflate $gzip]
    dict set R -gzip [dict create]
    foreach v {FTEXT HCRC EXTRA NAME COMMENT MTIME ISIZE CRC XFL OS} {
	dict set R -gzip $v [set $v]
    }
}

# gzip - read a gzipped transfer encoded entity off the wire
proc gzip {} {
    variable R
    Debug.socket {gzip: $R} 3

    # calculate how much entity is left to read, accumulate it
    set clen [dict get $R content-length]
    set diff [expr $clen - [string length [dict get $R -entity]]]
    dict append R -entity [read $::sock $diff]

    if {($clen - [string length [dict get $R -entity]]) == 0} {
	# we've completely received the response R,
	# asynchronously report this to the parent

	decode_gzip	;# un-gzip the entity
	::parent [list ::Http::received $::me $R]
	next
    }
}

# chunk - read a chunk of entity off the wire
proc chunk {} {
    variable R

    # read next chunk into response R
    set chunk [read $::sock [dict get $R -chunk_size]]
    dict append R -chunk $chunk
    dict incr R -chunk_size -[string length $chunk]

    if {[dict get $R -chunk_size] <= 0} {
	# we have read all of the entity
	dict append R -entity [dict get $R -chunk]
	Debug.socket {chunk readable: chunks} 4
	chan event $::sock readable chunks	;# read next chunk
    }
}

# chunks - read a chunk encoded entity off the wire
proc chunks {} {
    variable R

    set rc [gets $::sock ch]	;# read a hex-encoded length line
    if {$rc == -1} {
	if {[chan eof $::sock]} {
	    # remote end closed - just forget it
	    disconnect "Remote closed connection" $eo
	} else {
	    # no full line yet - wait
	}
    } else {
	Debug.http {chunks '$ch'} 3

	# got our line.
	set ch [split [string trim $ch]]
	if {$ch eq {}} {
	    return	;# it's an empty line
	}

	Debug.socket {chunks done readable: {}} 4
	chan event $::sock readable {}

	# calculate size of next chunk
	set size "0x[lindex $ch 0]"
	dict set R -chunk_size $size
	dict set R -chunk ""
	dict lappend R -chunk_ext [lrange $ch 1 end]

	if {$size == 0} {
	    # server signals we've read all the chunks
	    set ce [dict get? $R content-encoding]
	    switch -- $ce {
		gzip {
		    # the chunked entity was further gzip-encoded
		    decode_gzip
		}

		default -
		identity {
		    # we have the whole entity
		}
	    }

	    ::parent [list ::Http::received $::me $R]
	    next
	} else {
	    # read our chunk
	    Debug.socket {chunks readable: chunk} 4
	    chan event $::sock readable chunk	;# read next chunk
	}
    }

    Debug.socket {chunks: $ch}
}

# Parse the entire header in {$R -header}
proc parse {} {
    variable R

    Debug.socket {parse: $R} 3
    set header [dict get $R -header]

    # parse request-line
    dict set R -message [string trim [join [lassign [split [lindex $header 0]] version code]]]
    set version [lindex [split $version /] 1]
    dict set R -code $code
    dict set R -version $version

    # parse header body
    set key ""
    foreach line [lrange $header 1 end] {
	if {[string index $line 0] in " \t"} {
	    # header continuation line
	    # add to the key we're currently assembling
	    dict append R $key " [string trim $line]"
	} else {
	    # this is a new field:value pair
	    set value [string trim [join [lassign [split $line ":"] key] ":"]]
	    set key [string tolower [string trim $key "- \t"]]

	    if {[dict exists $R $key]} {
		dict append R $key ",$value"
	    } else {
		dict set R $key $value
	    }
	}
    }
    dict unset R -header	;# dispose of raw header

    # we have completely parsed the header body.

    # handle cookies
    # cookies are stored in the ::template, for return to server
    set R [Cookies load_server_cookies $R]
    if {[dict exists $R -cookies]} {
	dict set ::template -cookies [dict get $R -cookies]
    }

    # rfc2616 14.10:
    # A system receiving an HTTP/1.0 (or lower-version) message that
    # includes a Connection header MUST, for each connection-token in this
    # field, remove and ignore any header field(s) from the message with
    # the same name as the connection-token.
    if {[dict get $R -version] < 1.1
	&& [dict exists $R connection]
    } {
	foreach token [split [dict get $R connection] ","] {
	    catch {dict unset R [string trim $token]}
	}
	dict unset R connection
    }

    # is there an entity to be read?
    if {[dict exists $R content-length]
	&& [dict get $R content-length] > 0
    } {
	# read the entity
	dict set R -entity ""
	if {![dict exists $R content-encoding]} {
	    Debug.socket {parse readable: identity} 4
	    chan event $::sock readable identity	;# read entity
	} else {
	    switch -- [dict get $R content-encoding] {
		gzip {
		    Debug.socket {parse readable: gzip} 4
		    chan event $::sock readable gzip	;# read entity
		}
		default {
		    Debug.socket {parse readable: identity} 4
		    chan event $::sock readable identity	;# read entity
		}
	    }
	}
    } elseif {[dict exists $R transfer-encoding]
	      && [dict get $R transfer-encoding] eq "chunked"
	  } {
	chan event $::sock readable chunks	;# read chunks
    } else {
	# complete response received
	if {[incr ::pending -1]} {
	    variable stalled 0	;# we've cleared the pipeline
	}
	::parent [list ::Http::received $::me $R]
	next
    }
}

# get lines of header until it's complete
proc getH {} {
    variable sock
    variable R
    rxtimer cancel

    Debug.socket {get: $R} 5

    if {[catch {chan gets $sock line} result eo]} {
	disconnect "Failed read" $eo	;# inform parent that we're done
    }

    if {$result == -1} {
	if {[chan eof $sock]} {
	    # remote end closed - just forget it
	    disconnect "Remote closed connection" $eo
	} else {
	    # fblocked - there's not enough input to fill a line - just wait
	}

	rxtimer after $::enttime [list disconnect "pre-read timeout"]
    } elseif {[string trim $line] eq ""} {
	if {[dict exists $R -header]} {
	    # \n terminates the header - go parse it
	    Debug.socket {getH readable: {}} 4
	    chan event $sock readable {}	;# completed reading header

	    parse	;# handle the incoming
	}
    } else {
	# accumulate header lines
	rxtimer after $::rxtime [list disconnect "inter-read timeout"]
	dict lappend R -header $line
    }
}

# transmission event
proc tx {} {
    variable pipeline
    Debug.socket {tx [llength $pipeline]}
    if {$pipeline eq {}} {
	chan event $::sock writable {}	;# stop output
	return
    }

    # send all pending messages
    set pcount -1
    while {[set rq [lindex $pipeline [incr pcount]]] ne ""} {
	incr ::pending
	puts -nonewline $::sock "$rq\r\n"
	Debug.socket {TX $::pending ($rq)}
	set pipeline [lrange $pipeline 1 end]
    }
    chan flush $::sock

    Debug.socket {tx complete} 3
}

# Client requests transmission of a packet
proc send {method url args} {
    # ensure we don't violate pipeline constraints
    if {$::stalled} {
	# if we've sent a PUT,
	# we can't pipeline until the old pipeline is clear
	error "Can't pipeline request"
    } elseif {$method eq "PUT"} {
	set ::stalled 1
    }

    # merge template with expired cookies
    variable template
    Debug.socket {send $method $url $args over '$template'}
    set template [Cookies expire_record $template]	;# expire old cookies stored template
    set T [dict merge $template $args [list -method $method date [Http Date]] [Url parse $url]]

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

    # format up and send each cookie
    if {[dict exists $T -cookies]} {
	foreach cookie [Cookies format4client [dict get $T -cookies]] {
	    append header "cookie: $cookie\r\n"
	}
    }

    if {[info exists body]} {
	append header \r\n $body
    } else {
	append header \r\n
    }

    # start or resume transmission
    lappend ::pipeline $header	;# append header to outgoing pipeline
    incr ::txcount

    catch {chan event $::sock writable ::tx}	;# start output train
    Debug.socket {sent $method $url $args}
    return $::txcount
}

# get - issue GET request
proc get {url args} {
    return [send GET $url {*}$args]
}

# head - issue HEAD request
proc head {url args} {
    return [send HEAD $url {*}$args]
}

# post - issue POST request
proc post {url body args} {
    return [send POST $url -entity $body {*}$args]
}

# put - issue PUT request
proc put {url body args} {
    set ::stalled 1
    return [send PUT $url -entity $body {*}$args]
}

# delete - issue DELETE request
proc delete {url args} {
    return [send DELETE $url {*}$args]
}

Debug on socket 5
Debug on http 3
Debug on cookies 100

# now we're able to process commands
thread::wait
#puts stderr "~Thread: [thread::id]"
