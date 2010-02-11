# Httpd - near HTTP/1.1 protocol server.
#

if {[info exists argv0] && ($argv0 eq [info script])} {
    # test Httpd
    lappend auto_path [pwd] ../Utilities/ ../extensions/
    package require Http
}

package require Debug
Debug off Httpd 10
Debug off HttpdLow 10
Debug off Watchdog 10
Debug off Entity 10
Debug on slow 10

package require Listener
package require Chan

package require Query
package require Html
package require Url
package require Http

package provide Httpd 4.0

set API(Server/Httpd) {
    {
	Httpd is the low-level core Wub HTTP protocol module.  It parses HTTP traffic, dispatches on URL and handles pipelined responses.  It interfaces with other modules to provide blocking, caching, logging and other useful functionality.  It is easily configurable in code, and dispatch is configurable by means of the [../Domains/Nub Nub] module.

	Httpd is intended to be non-permissive in its handling of requests.  In general, legitimate browsers and bots are well-behaved and conform closely to the RFC.  In rejecting ill-formed requests, Httpd is expected to somewhat reduce the impact and load of spammers on the server.

	While Httpd can be customised and used almost stand-alone in a minimal server system, it is designed as the protocol front-end to a series of modules known as [../Domains Domains] which provide a wide range of semantics to the site-author.  The module [../Domains/Nub Nub] generates dispatch and interface code to link these [../Domains Domains] to Httpd for processing.  By analogy with Apache, [../Domains/Nub Nub] performs the dispatch functions of Apaches .htaccess files.

	== Quickstart ==
	Use [../Domains/Nub Nub] to define a URL mapping to instances of the various [../Domains Domains].  A complete site can be constructed using nothing but Nub over Domains.

	== Interface ==
	;Connect: Initializes an HTTP 1.1 connection and pipeline, may be called by some external connection handler (such as [Listener], by default) to initiate HTTP 1.1 protocol interaction and processing.

	;[[Http Resume $response]]: will resume a suspended request and pipeline the response out to the client.

	All processing of requests (ie: transformation of requests into responses) is performed by ::Httpd::do, which can be defined directly by the user (see Customisation) or can be defined indirectly by [../Domains/Nub Nub].

	== Customisation ==

	To customise Httpd, the semantics of request processing need to be defined in a couple of plugged-in commands.  These commands have sensible minimally functional defaults, but are expected to be customised.

	Httpd expects ''::Httpd::do'' to operate within the coroutine to process a request and return a response (or error.)  The [../Domains/Nub Nub] module generates such a command, which dispatches on URL to Domains specified in the configuration.

	Command ::Httpd::do will be called with REQUEST and a ''pre''-processed request expecting that the call will return a response.

	Command ::Httpd::do will be called with TERMINATE when a connection has closed.  A custom consumer may use this notification to clean up before termination.  No socket or reader interaction is possible at this point.

	Httpd expects ''::Httpd::pre'' to pre-process the request within the coroutine's context before handing it to ::Httpd::do.  By default, ''pre'' unpacks [../Utility/Cookies Cookies] in the request.

	Httpd expects ''::Httpd::post'' to post-process the response from ::Httpd::do.  By default, the [Convert] module is invoked to perform content-negotiation.  Other useful functionality might be [Cookie] handling, Session management, etc.

	Httpd defines an ''::Httpd::reader'' command for protocol interaction and an ::Httpd::do for dispatching on URL, post-processing via ::Httpd::post and sending pipelined responses back to the client via ::Httpd::reader.  It's not expected that these should be modified, but (in keeping with the goal of extensibility) it is possible.

	== Architecture ==
	Httpd constructs one coroutine for each open connection in response to a '''Connect''' call from a [Listener].  The coroutine (whose semantics are contained in ::Httpd::reader) parses HTTP 1.1 from the socket into a request dict, which it then dispatches via [[::Httpd::do]].

	=== Reader Protocol ===
	;READ: [[fileevent readable]] - there's input to be read and processed
	;CLOSING: [[fileevent readable]] - there's input, but we're half-closed, not processing any more input, and merely waiting for all pending responses to drain.
	;WRITABLE: [[fileevent writable]] - there's space on the output queue.  We'll send any responses queued for sending, and unblock the reader if this makes output buffer space available.
	;SEND: a ''consumer'' has a response for us to queue for delivery.
	;SUSPEND: a ''consumer'' indicates that current response processing is suspended.  New requests will be processed, but the pipeline stalls until the consumer is RESUMEd, and generates a response to the current request.  This indication has little effect, but to extend some grace time to the connection which keeps the reaper away from it.
	;REAPED: this connection has been reaped due to inactivity.
	;TERMINATE: this connection is being closed due to unrecoverable error or a ''consumer'' request.

	=== Reaping ===
	The time of each event processed by the reader and consumer coroutines is logged.  If a (configurable) period of idleness occurs, a per-connection timer causes the connection to be reaped.  A consumer which suspends is given a little (configurable) grace time to produce its response.

	== Ancillary Functionality ==
	Httpd is not intended to be a minimal HTTP server, and so performs generally useful (but safe) optimisations and traffic management.

	;[Cache]: Httpd (configurably) interacts with a server cache (also known as a reverse proxy) which automatically caches responses and serves them (as appropriate) to clients upon matching request.  The Cache is transparent to ''consumer'', which will not be invoked if cached content can be supplied.  Caching policy is determined entirely by response and request protocol elements - that is, if a response is publicly cacheable, the [Cache] module will capture and reproduce it, handling conditional fetches and such.
	;Exhaustion: Httpd maintains a count of the current connections for each IP address, and may refuse service if they exceed a configured limit.  In practice, this has not been found to be very useful.
	;[Block]ing: refuses service to blocked IP addresses
	;[../Utility/spider Spider] detection: looks up User-Agent in a db of known bad-bots, to attempt to refuse them service.  In practice, this has not been found to be very useful.
	;[../Utility/UA UA] classification: parses and classifies User-Agent in an attempt to classify interactions as 'bot', 'broser', 'spider'. 'spammer'.  Limited success.
    }
}

proc bgerror {args} {
    Debug.error {bgerror: $args}
}
interp bgerror {} bgerror

# define a default [pest] predicate, allow it to be overriden by pest.tcl
proc pest {req} {return 0}
catch {source [file join [file dirname [info script]] pest.tcl]}

# this is a shim for fcopy
proc coroshim_fcopy {coro args} {
    $coro $args
}

namespace eval Httpd {
    variable server_port ;# server's port (if different from Listener's)
    variable server_id "Wub/[package provide Httpd]"
    variable cid 0		;# unique connection ID
    variable exhaustion	20	;# how long to wait on exhaustion
    variable maxconn		;# max connections
    variable generation		;# worker/connection association generation

    # common log format log - per request, for log analysis
    variable log ""	;# fd of open log file - default none
    variable logfile ""	;# name of log file - "" means none
    variable customize ""	;# file to source to customise Httpd


    # limits on header size
    variable maxline 2048	;# max request line length
    variable maxfield 4096	;# max request field length
    variable maxhead 1024	;# maximum number of header lines
    variable maxurilen 1024	;# maximum URI length
    variable maxentity -1	;# maximum entity size

    # timeout - by default none
    variable timeout 60000

    # activity log - array used for centralised timeout
    variable activity

    variable ce_encodings {gzip}
    variable te_encodings {chunked}

    variable uniq [pid]	;# seed for unique coroutine names

    # give a uniq looking name
    proc uniq {} {
	variable uniq
	return [incr uniq]
    }

    # rdump - return a stripped request for printing
    proc rdump {req} {
	dict set req -content "<ELIDED [string length [dict get? $req -content]]>"
	dict set req -entity "<ELIDED [string length [dict get? $req -entity]]>"
	dict set req -gzip "<ELIDED [string length [dict get? $req -gzip]]>"
	
	return [regsub {([^[:print:]])} $req .]
    }

    # wrapper for chan ops - alert on errors
    proc chan {args} {
	set code [catch {uplevel 1 [list ::chan {*}$args]} e eo]
	if {$code} {
	    if {[info coroutine] ne ""} {
		Debug.Httpd {[info coroutine]: chan error $code - $e ($eo)}
		if {[lindex $args 0] ne "close"} {
		    terminate $e	;# clean up and close unless we're already closing
		}
	    } else {
		Debug.error {chan error $code - $e ($eo)}
	    }
	} else {
	    return $e
	}
    }

    # shut down socket and reader
    proc terminate {{reason ""}} {
	# this is the reader - trying to terminate
	Debug.Httpd {[info coroutine] terminate: ($reason)}

	# disable inactivity reaper for this coro
	variable activity
	catch {unset activity([info coroutine])}

	# don't fear the reaper
	variable reaper
	catch {
	    after cancel $reaper([info coroutine])
	    unset reaper([info coroutine])
	}
	variable crs; unset crs([info coroutine])	;# destroy the coroutine record

	# forget whatever higher level connection info
	corovars cid socket ipaddr

	# clean up all open files
	# - the only point where we close $socket
	variable files
	if {[dict exists $files [info coroutine]]} {
	    foreach fd [dict keys [dict get $files [info coroutine]]] {
		catch {chan close $fd}
	    }
	    dict unset files [info coroutine]
	}

	# destroy reader - that's all she wrote
	Debug.Httpd {reader [info coroutine]: terminated}
	rename [info coroutine] ""; ::yield	;# terminate coro
    }

    proc unwritable {} {
	corovars socket events
	chan event $socket writable ""
	dict unset events writable
    }
    proc writable {{what WRITE}} {
	corovars socket events
	dict set events writable $what
    }

    proc unreadable {} {
	corovars socket events
	chan event $socket readable ""
	dict unset events readable
    }
    proc readable {{what READ}} {
	corovars socket events
	dict set events readable $what
    }

    # close? - should we close this connection?
    proc close? {r} {
	# don't honour 1.0 keep-alives - why?
	set close [expr {[dict get $r -version] < 1.1}]
	Debug.HttpdLow {version [dict get $r -version] implies close=$close}

	# handle 'connection: close' request from client
	foreach ct [split [dict get? $r connection] ,] {
	    if {[string tolower [string trim $ct]] eq "close"} {
		Debug.HttpdLow {Tagging close at connection:close request}
		set close 1
		break	;# don't need to keep going
	    }
	}

	if {$close} {
	    # we're not accepting more input but defer actually closing the socket
	    # until all pending transmission's complete
	    corovars status closing socket
	    set closing 1	;# flag the closure
	    lappend status CLOSING
	    readable CLOSING
	}

	return $close
    }

    # arrange gzip Transfer Encoding
    variable chunk_size 4196	;# tiny little chunk size
    variable gzip_bugged {}	;# these browsers can't take gzip

    # gzip_content - gzip-encode the content
    proc gzip_content {reply} {
	if {[dict exists $reply -gzip]} {
	    return $reply	;# it's already been gzipped
	}

	# prepend a minimal gzip file header:
	# signature, deflate compression, no flags, mtime,
	# xfl=0, os=3
	set content [dict get $reply -content]
	set gztype [expr {[string match text/* [dict get $reply content-type]]?"text":"binary"}]
	set gzip [zlib gzip $content -header [list crc 0 time [clock seconds] type $gztype]]

	dict set reply -gzip $gzip
	return $reply
    }

    # CE - find and effect appropriate content encoding
    proc CE {reply args} {
	# default to identity encoding
	set content [dict get $reply -content]
	Debug.http {CE -encoding:[dict get? $args -encoding]}
	if {![dict exists $reply -gzip]
	    && ("gzip" in [dict get? $args -encoding])
	    && ![string match image/* [dict get? $reply content-type]] 
	} {
	    set reply [gzip_content $reply]
	}

	# choose content encoding - but not for MSIE
	variable chunk_size
	variable gzip_bugged
	if {[dict get? $reply -ua id] ni $gzip_bugged
	    && [dict exists $reply accept-encoding]
	    && ![dict exists $reply content-encoding]
	} {
	    foreach en [split [dict get $reply accept-encoding] ","] {
		lassign [split $en ";"] en pref
		set en [string trim $en]
		if {$en in [dict get? $args -encoding]} {
		    switch $en {
			"gzip" { # substitute the gzipped form
			    if {[dict exists $reply -gzip]} {
				set content [dict get $reply -gzip]
				dict set reply content-encoding gzip
				#set reply [Http Vary $reply Accept-Encoding User-Agent]
				if {[dict get $reply -version] > 1.0} {
				    # this is probably redundant, since 1.0
				    # doesn't define accept-encoding (does it?)
				    #dict set reply -chunked $chunk_size
				    #dict set reply transfer-encoding chunked
				}
				break
			    }
			}
		    }
		}
	    }
	}
	return [list $reply $content]
    }

    # charset - ensure correctly encoded content
    proc charset {reply} {
	if {[dict exists $reply -chconverted]} {
	    return $reply	;# don't re-encode by charset
	}

	# handle charset for text/* types
	lassign [split [dict get? $reply content-type] {;}] ct
	if {[string match text/* $ct] || [string match */*xml $ct]} {
	    if {[dict exists $reply -charset]} {
		set charset [dict get $reply -charset]
	    } else {
		set charset utf-8	;# default charset
	    }
	    dict set reply -charset $charset
	    dict set reply -chconverted $charset
	    dict set reply content-type "$ct; charset=$charset"
	    dict set reply -content [encoding convertto $charset [dict get $reply -content]]
	}
	return $reply
    }

    # make GET/HEAD conditional
    # this will transform a request if there's a conditional which
    # applies to it.
    proc conditional {r} {
	if {[dict get $r -code] != 200} {
	    return $r
	}

	set etag [dict get? $r etag]
	# Check if-none-match
	if {[Http any-match $r $etag]} {
	    # rfc2616 14.26 If-None-Match
	    # If any of the entity tags match the entity tag of the entity
	    # that would have been returned in the response to a similar 
	    # GET request (without the If-None-Match header) on that 
	    # resource, or if "*" is given and any current entity exists 
	    # for that resource, then the server MUST NOT perform the 
	    # requested method, unless required to do so because the 
	    # resource's modification date fails to match that
	    # supplied in an If-Modified-Since header field in the request.
	    if {[string toupper [dict get $r -method]] in {"GET" "HEAD"}} {
		# if the request method was GET or HEAD, the server 
		# SHOULD respond with a 304 (Not Modified) response, including
		# the cache-related header fields (particularly ETag) of one 
		# of the entities that matched.
		Debug.cache {unmodified [dict get $r -uri]}
		#counter $cached -unmod	;# count unmod hits
		return [Http NotModified $r]
		# NB: the expires field is set in $r
	    } else {
		# For all other request methods, the server MUST respond with
		# a status of 412 (Precondition Failed).
		#return [Http PreconditionFailed $r]
	    }
	} elseif {![Http if-match $r $etag]} {
	    #return [Http PreconditionFailed $r]
	} elseif {![Http if-range $r $etag]} {
	    catch {dict unset r range}
	    # 14.27 If-Range
	    # If the entity tag given in the If-Range header matches the current
	    # entity tag for the entity, then the server SHOULD provide the
	    # specified sub-range of the entity using a 206 (Partial content)
	    # response. If the entity tag does not match, then the server SHOULD
	    # return the entire entity using a 200 (OK) response.
	}
	return $r
    }

    # format4send - format up a reply for sending.
    proc format4send {reply args} {
	Debug.Httpd {format4send $args}
	set file ""
	set sock [dict get $reply -sock]
	set cache [expr {[dict get? $args -cache] eq "1"}]
	if {[catch {
	    # unpack and consume the reply from replies queue
	    if {![dict exists $reply -code]} {
		set code 200	;# presume it's ok
	    } else {
		set code [dict get $reply -code]
	    }

	    if {$code < 4} {
		# this was a tcl error code, not an HTTP code
		set code 500
	    }

	    # make reply conditional
	    set reply [conditional $reply]
	    set code [dict get $reply -code]

	    # Deal with content data
	    set range {}	;# default no range
	    switch -glob -- $code {
		204 - 304 - 1* {
		    # 1xx (informational),
		    # 204 (no content),
		    # and 304 (not modified)
		    # responses MUST NOT include a message-body
		    Debug.HttpdLow {format4send: code is $code}
		    set reply [Http expunge $reply]
		    set content ""
		    catch {dict unset reply -content}
		    catch {dict unset reply -file}
		    set cache 0	;# can't cache these
		    set empty 1
		}

		default {
		    set empty 0
		    if {[dict exists $reply -content]} {
			# correctly charset-encode content
			set reply [charset $reply]

			#Debug.HttpdLow {pre-CE content length [string length [dict get $reply -content]]}
			# also gzip content so cache can store that.
			lassign [CE $reply {*}$args] reply content
			set file ""	;# this is not a file

			# ensure content-length is correct
			dict set reply content-length [string length $content]
			#Debug.HttpdLow {post-CE content length [string length $content]}
		    } elseif {[dict exists $reply -file]} {
			# the app has returned the pathname of a file instead of content
			set file [dict get $reply -file]
			dict set reply content-length [file size $file]
			set content ""
		    } else {
			Debug.HttpdLow {format4send: response empty - no content in reply}
			set content ""	;# there is no content
			set file ""	;# this is not a file
			set empty 1	;# it's empty
			dict set reply content-length 0
			#puts stderr "NOCACHE empty $code: $cache"
			set cache 0	;# can't cache no content
		    }

		    if {!$empty && $code == 200} {
			# handle range for 200
			set ranges [dict get? $reply range]
			if {$ranges ne ""} {
			    set ranges [lindex [lassign [split $ranges =] unit] 0]
			    set ranges [split $ranges ,]
			    set ranges [lindex $ranges 0]	;# only handle one range
			    foreach rr $ranges {
				lassign [split $rr -] from to
				lassign [split $to] to
				set size [dict get $reply content-length]
				if {$from eq ""} {
				    set from [expr {$size-$to+1}]
				    set to $size
				} elseif {$to > $size || $to eq ""} {
				    set to [expr {$size-1}]
				}
				
				lappend range $from $to	;# remember range to send
			    }

			    # send appropriate content range and length fields
			    set code 206	;# partial content
			    dict set reply content-range "bytes $from-$to/$size"
			    dict set reply content-length [expr {$from-$to+1}]

			    Debug.Httpd {range: [dict get $reply content-range]}
			}
		    }
		}
	    }

	    # set the informational header error message
	    if {[dict exists $reply -error]} {
		set errmsg [dict get $reply -error]
	    }
	    if {![info exists errmsg] || ($errmsg eq "")} {
		set errmsg [Http ErrorMsg $code]
	    }

	    # format header
	    set header "$code $errmsg\r\n"	;# note - needs prefix

	    # format up the headers
	    if {$code != 100} {
		append header "Date: [Http Now]" \r\n
		set si [dict get? $reply -server_id]
		if {$si eq ""} {
		    set si "The Wub"
		}
		append header "Server: $si" \r\n
	    }

	    # add in cookies already formatted up
	    foreach hdr {set-cookie} {
		if {[dict exists $reply set-cookie]} {
		    append header $hdr: " " [dict get $reply $hdr] \n
		}
	    }
	    
	    # format up and send each cookie
	    if {[dict exists $reply -cookies]} {
		Debug.cookies {Http processing: [dict get $reply -cookies]}
		set c [dict get $reply -cookies]
		foreach cookie [Cookies format4server $c] {
		    Debug.cookies {Http set: '$cookie'}
		    append header "set-cookie: $cookie\r\n"
		}
	    } else {
		Debug.cookies {Http processing: no cookies}
	    }

	    # handle Vary field and -vary dict
	    dict set reply -vary Accept-Encoding 1
	    if {[dict exists $reply -vary]} {
		if {[dict exists $reply -vary *]} {
		    dict set reply vary *
		} else {
		    dict set reply vary [join [dict keys [dict get $reply -vary]] ,]
		}
		dict unset reply -vary
	    }

	    # now attend to caching generated content.
	    if {$empty || [dict get $reply content-length] == 0} {
		set cache 0	;# don't cache no content
	    } elseif {$cache} {
		# use -dynamic flag to avoid caching even if it was requested
		set cache [expr {
				 ![dict exists $reply -dynamic]
				 || ![dict get $reply -dynamic]
			     }]

		if {$cache && [dict exists $reply cache-control]} {
		    set cacheable [split [dict get $reply cache-control] ,]
		    foreach directive $cacheable {
			set body [string trim [join [lassign [split $directive =] d] =]]
			set d [string tolower [string trim $d]]
			if {$d in {no-cache private}} {
			    set cache 0
			    break
			}
		    }
		}
	    }

	    # add in Auth header elements - TODO
	    foreach challenge [dict get? $reply -auth] {
		append header "WWW-Authenticate: $challenge" \r\n
	    }

	    if {[dict get $reply -method] eq "HEAD"} {
		# All responses to the HEAD request method MUST NOT
		# include a message-body but may contain all the content
		# header fields.
		set empty 1
		set content ""
	    }

	    if {$code >= 500} {
		# Errors are completely dynamic - no caching!
		set cache 0
	    }

	    # strip http fields which don't have relevance in response
	    dict for {n v} $reply {
		set nl [string tolower $n]
		if {[string match x-* $nl]} {
		    append header "$n: $v" \r\n
		} elseif {$nl ni {server date}
			  && [info exists ::Http::headers($nl)]
			  && $::Http::headers($nl) ne "rq"
		      } {
		    append header "$n: $v" \r\n
		}
	    }
	} r eo]} {
	    if {![info exists code] || $code >= 500} {
		# Errors are completely dynamic - no caching!
		set cache 0
	    }

	    Debug.error {Sending Error: '$r' ($eo) Sending Error}
	} else {
	    Debug.HttpdLow {format4send: ($header)}
	}
	return [list $reply $header $content $file $empty $cache $range]
    }

    # our fcopy has completed
    proc fcopy_complete {fd bytes written {error ""}} {
	corovars replies closing socket
	Debug.Httpd {[info coroutine] fcopy_complete: $fd $bytes $written '$error'}
	watchdog

	catch {close $fd}	;# remove file descriptor
	variable files
	dict unset files [info coroutine] $fd

	set gone [catch {chan eof $socket} eof]
	if {$gone || $eof} {
	    # detect socket closure ASAP in sending
	    Debug.Httpd {[info coroutine] Lost connection on fcopy}
	    if {$error eq ""} {
		set error "eof on $socket in fcopy"
	    }
	}

	# if $bytes != $written or $error ne "", we have a problem
	if {$gone || $eof || $bytes != $written || $error ne ""} {
	    if {$error eq ""} {
		set error "fcopy failed to send $bytes, only sent $written."
	    }
	    Debug.error $error
	    terminate "$error in fcopy"
	    return
	} elseif {![chan pending output $socket]} {
	    # only when the client has consumed our output do we
	    # restart reading input
	    Debug.HttpdLow {[info coroutine] fcopy_complete: restarting reader}
	    readable
	} else {
	    Debug.HttpdLow {[info coroutine] fcopy_complete: suspending reader [chan pending output $socket]}
	}

	# see if the writer needs service
	writable
    }

    # extract some information from Httpd to aid in debugging
    proc status {} {
	variable crs	;# array of running coroutine transitions
	variable activity ;# array of coroutine activity
	variable files	;# dict of open files per coroutine
	variable reaper ;# when will this be reaped?

	set now [clock milliseconds]
	lappend result "[<th> coro] [<th> activity] [<th> reaping] [<th> self] [<th> fd] [<th> peer] [<th> connections] [<th> transitions] [<th> files]"
	foreach coro [info commands rc*] {
	    set line [<th> [namespace tail $coro]]
	    if {[info exists activity($coro)]} {
		append line [<td> [expr {$now - $activity($coro)}]]
	    } else {
		append line [<td> ""]
	    }
	    if {[info exists reaper($coro)]} {
		append line [<td> [expr {$reaper($coro) - now}]]
	    } else {
		append line [<td> ""]
	    }

	    lassign [split $coro _] conn
	    set conn [namespace tail $conn]
	    if {![catch {chan configure $conn} conf]} {
		set conf [dict merge $conf [chan configure [dict get $conf -fd]]]
	    } else {
		set conf {}
	    }
	    foreach n {self fd peername connections} {
		append line [<td> [dict get? $conf -$n]]
	    }

	    if {[info exists crs($coro)]} {
		append line [<td> $crs($coro)]
	    } else {
		append line [<td> ""]
	    }

	    if {[dict exists $files $coro]} {
		append line [<td> [dict keys [dict get $files $coro]]]
	    } else {
		append line [<td> ""]
	    }
	    lappend result $line
	}
	set result <tr>[join $result </tr><tr>]</tr>

	return [<table> border 1 width 90% $result]
    }

    # respond to client with as many consecutive responses as he can consume
    proc respond {} {
	corovars replies response sequence generation satisfied transaction closing unsatisfied socket
	if {[string match DEAD* [info coroutine]]} {
	    Debug.Httpd {[info coroutine] appears to be dead}
	    terminate "oops - we're dead in respond"
	    return
	}
	if {$closing && ![dict size $unsatisfied]} {
	    # we have no more requests to satisfy and we want to close
	    Debug.Httpd {[info coroutine] closing as there's nothing pending}
	    terminate "finally close in responder"
	    return
	}

	# shut down responder if there's nothing to write
	if {![dict size $replies]} {
	    unwritable	;# no point in trying to write
	}

	variable activity

	# send all responses in sequence from the next expected to the last available
	Debug.Httpd {[info coroutine] pending to send: ([dict keys $replies])}
	foreach next [lsort -integer [dict keys $replies]] {
	    watchdog	;# tickle the watchdog

	    set gone [catch {chan eof $socket} eof]
	    if {$gone || $eof} {
		# detect socket closure ASAP in sending
		Debug.Httpd {[info coroutine] Lost connection on transmit}
		terminate "eof on $socket"
		return 1	;# socket's gone - terminate session
	    }

	    # ensure we don't send responses out of sequence
	    if {$next != $response} {
		# something's blocking the response pipeline
		# so we don't have a response for the next transaction.
		# we must therefore wait until all the preceding transactions
		# have something to send
		Debug.Httpd {[info coroutine] no pending or $next doesn't follow $response}
		unwritable	;# no point in trying to write
		
		if {[chan pending output $socket]} {
		    # the client hasn't consumed our output yet
		    # stop reading input until he does
		    unreadable
		} else {
		    # there's space for more output, so accept more input
		    readable
		}

		return 0
	    }
	    set response [expr {1 + $next}]	;# move to next response
	    
	    # respond to the next transaction in trx order
	    # unpack and consume the reply from replies queue
	    # remove this response from the pending response structure
	    lassign [dict get $replies $next] head content file close empty range
	    dict unset replies $next		;# consume next response

	    # connection close after transmission required?
	    # NB: we only consider closing if all pending requests
	    # have been satisfied.
	    if {$close} {
		# inform client of intention to close
		Debug.HttpdLow {close requested on $socket - sending header}
		append head "Connection: close" \r\n	;# send a close just in case
		# Once this header's been sent, we're committed to closing
	    }

	    # send headers with terminating nl
	    chan puts -nonewline $socket "$head\r\n"
	    Debug.Httpd {[info coroutine] SENT HEADER: $socket '[lindex [split $head \r] 0]' [string length $head] bytes} 4
	    chan flush $socket	;# try to flush as early as possible
	    Debug.HttpdLow {[info coroutine] flushed $socket} 4

	    # send the content/entity (if any)
	    # note: we must *not* send a trailing newline, as this
	    # screws up the content-length and confuses the client
	    # which doesn't then pick up the next response
	    # in the pipeline
	    if {!$empty} {
		if {$file ne ""} {
		    # send content of file descriptor using fcopy
		    set fd [open $file r]
		    variable files; dict set files [info coroutine] $fd 1
		    set bytes [file size $file]

		    chan configure $socket -translation binary
		    chan configure $fd -translation binary
		    unreadable	;# stop reading input while fcopying
		    unwritable	;# stop writing while fcopying
		    grace -1	;# stop the watchdog resetting the link

		    if {[llength $range]} {
			lassign $range from to
			chan seek $fd $from start
			set bytes [expr {$to-$from+1}]
			Debug.Httpd {[info coroutine] FCOPY RANGE: '$file' hytes $from-$to/$bytes} 8
			chan copy $fd $socket -command [list ::coroshim_fcopy [info coroutine] FCOPY $fd $bytes]

		    } else {
			Debug.Httpd {[info coroutine] FCOPY ENTITY: '$file' $bytes bytes} 8
			chan copy $fd $socket -command [list ::coroshim_fcopy [info coroutine] FCOPY $fd $bytes]
		    }
		    break	;# we don't process any more i/o on $socket
		} elseif {[llength $range]} {
		    # send literal content
		    lassign $range from to
		    chan puts -nonewline $socket [string range $content $from $to]
		    Debug.Httpd {[info coroutine] SENT RANGE: bytes $from-$to/[string length $content] bytes} 8
		} else {
		    chan puts -nonewline $socket $content	;# send the content
		    Debug.Httpd {[info coroutine] SENT ENTITY: [string length $content] bytes} 8
		}
	    }
	    #chan flush $socket

	    # only send for unsatisfied requests
	    catch {dict unset unsatisfied $next}

	    if {$close} {
		return 1	;# terminate session on request
	    }

	    if {[chan pending output $socket]} {
		# the client hasn't consumed our output yet - stop sending more
		break
	    }
	}

	if {[chan pending output $socket]} {
	    # the client hasn't consumed our output yet
	    # stop reading input until he does
	    unreadable
	} else {
	    # there's space for more output, so accept more input
	    readable
	}
    }

    # we have been told we can write a reply
    # write is the entry point for response from cached content
    proc write {r cache} {
	corovars replies response sequence generation satisfied transaction closing unsatisfied socket last

	# keep pipeline open while we have unsatisfied requests
	if {$closing && ![dict size $unsatisfied]} {
	    # we have no more requests to satisfy and we want to close
	    terminate "finally close in write"
	}

	# process suspension at lowest level
	if {[dict exists $r -suspend]} {
	    return 0	;# this reply has been suspended - we haven't got it yet
	    # so we simply return.  The lack of a response for the corresponding
	    # pipelined request has the effect of suspending the pipeline until
	    # the response has been delivered.
	    # requests will still be processed while the pipeline's suspended,
	    # but their responses will only be returned in strict and close order.
	}

	Debug.Httpd {write: [info coroutine] ([rdump $r]) satisfied: ($satisfied) unsatisfied: ($unsatisfied)}

	# fetch transaction from the caller's identity
	if {![dict exists $r -transaction]} {
	    # can't Send reply: no -transaction associated with request
	    Debug.error {Send discarded: no transaction ($r)}
	    return 1	;# close session
	}
	set trx [dict get $r -transaction]

	# discard duplicate responses
	if {[dict exists $satisfied $trx]} {
	    # a duplicate response has been sent - discard this
	    # this could happen if a dispatcher sends a response,
	    # then gets an error.
	    Debug.error {Send discarded: duplicate ([rdump $r])}
	    return {0 0}	;# duplicate response - just ignore
	}

	# only send for unsatisfied requests
	if {![dict exists $unsatisfied $trx]} {
	    Debug.error {Send discarded: satisfied duplicate ([rdump $r])}
	    return {0 0}	;# duplicate response - just ignore
	}

	# record the behaviour
	variable crs
	set now [::tcl::clock::milliseconds]
	lappend crs([info coroutine])  [expr {$now - $last}] SENT
	dict set r -behaviour $crs([info coroutine])
	set crs([info coroutine]) {}

	# generate a log line
	variable log
	if {$log ne "" && [catch {
	    puts $log [Http clf $r]	;# generate a log line
	    chan flush $log
	} le leo]} {
	    Debug.error {log error: $le ($leo)}
	}

	# wire-format the reply transaction - messy
	variable ce_encodings	;# what encodings do we support?
	lassign [format4send $r -cache $cache -encoding $ce_encodings] r header content file empty cache range
	set header "HTTP/1.1 $header" ;# add the HTTP signifier

	# record transaction reply and kick off the responder
	# response has been collected and is pending output
	# queue up response for transmission
	#
	# response is broken down into:
	# header - formatted to go down the line in crlf mode
	# content - content to go down the line in binary mode
	# close? - is the connection to be closed after this response?
	# chunked? - is the content to be sent in chunked mode?
	# empty? - is there actually no content, as distinct from 0-length content?
	Debug.Httpd {[info coroutine] ADD TRANS: ([dict keys $replies])}
	dict set replies $trx [list $header $content $file [close? $r] $empty $range]
	dict set satisfied $trx {}	;# record satisfaction of transaction

	if {[chan pending output $socket]} {
	    # the client hasn't consumed our output yet
	    # stop reading input until he does
	    unreadable
	} else {
	    # there's space for more output, so accept more input
	    readable
	}

	set now [::tcl::clock::milliseconds]
	lappend crs([info coroutine])  [expr {$now - $last}] READY

	# having queued the response, we allow it to be sent on 'socket writable' event
	writable

	return [list 0 $cache]
    }

    # send --
    #	deliver in-sequence transaction responses
    #
    # Arguments:
    #
    # Side Effects:
    #	Send transaction responses to client
    #	Possibly close socket, possibly cache response
    proc send {r {cache 1}} {
	Debug.Httpd {[info coroutine] send: ([rdump $r]) $cache [expr {[dict get? $r -ua_class] ni {browser unknown}}]}
	dict set r -sent [clock microseconds]

	# precheck generation
	corovars generation
	if {[dict exists $r -generation] && [dict get $r -generation] != $generation} {
	    dict set r -code 599	;# signal a really bad packet
	}

	# if this isn't a browser - do not cache!
	if {[dict get? $r -ua_class] ni {browser unknown}} {
	    Debug.Httpd {not a browser - do not cache [dict get $r -uri]}
	    set cache 0	;# ??? TODO
	}

	# check generation
	if {![dict exists $r -generation]} {
	    # there's no generation here - hope it's a low-level auto response
	    # like Block etc.
	    Debug.log {[info coroutine] Send without -generation ($r)}
	    dict set r -generation $generation
	} elseif {[dict get $r -generation] != $generation} {
	    # report error to sender, but don't die ourselves
	    Debug.error {Send discarded: out of generation [dict get $r -generation] != $generation ($r)}
	    return ERROR
	}

	# global consequences - caching
	if {$cache} {
	    # handle caching (under no circumstances cache bot replies)
	    set r [Cache put $r]	;# cache it before it's sent
	    dict set r -caching inserted
	} else {
	    Debug.Httpd {Do Not Cache put: ([rdump $r]) cache:$cache}
	}

	if {[catch {
	    # send all pending responses, ensuring we don't send out of sequence
	    write $r $cache
	} result eo]} {
	    Debug.error {FAILED write $result ($eo) IP [dict get $r -ipaddr] ([dict get? $r user-agent]) wanted [dict get $r -uri]}

	    terminate closed
	}

	lassign $result close cache

	# deal with socket closure
	if {$close} {
	    terminate closed
	}
    }

    variable crs

    # yield wrapper with command dispatcher
    proc yield {{retval ""}} {
	corovars cmd status unsatisfied socket last events
	if {![info exists last]} {
	    set last [::tcl::clock::milliseconds]
	}
	variable crs
	while {1} {
	    Debug.HttpdLow {coro [info coroutine] yielding}
	    set x [after info]
	    if {[llength $x] > 10} {
		Debug.log {After: [llength $x]}
	    }

	    # unpack event
	    if {[catch {
		set now [::tcl::clock::milliseconds]
		lappend crs([info coroutine]) [expr {$now - $last}] YIELD; set last $now

		dict for {k v} $events {
		    chan event $socket $k [list [info coroutine] $v]
		}

		set args [lassign [::yield $retval] op]; set retval ""

		foreach k {readable writable} {
		    chan event $socket $k ""
		}
		set now [::tcl::clock::milliseconds]
		lappend crs([info coroutine]) [expr {$now - $last}] $op; set last $now
	    } e eo]} {
		Debug.HttpdLow {yield crashed $e ($eo)}
		terminate yieldcrash
	    }

	    set gone [catch {chan eof $socket} eof]
	    if {$gone || $eof || [string match DEAD* [info coroutine]]} {
		Debug.HttpdLow {[info coroutine] yield - eof $socket}
		terminate "oops - we're dead in yield"
		return
	    }

	    lappend status $op
	    Debug.HttpdLow {back from yield [info coroutine] -> $op}

	    # record a log of our activity to fend off the reaper
	    variable activity

	    # dispatch on command
	    switch -- [string toupper $op] {
		STATS {
		    set retval {}
		    foreach x [uplevel \#1 {info locals}] {
			catch [list uplevel \#1 [list set $x]] r
			lappend retval $x $r
		    }
		}

		READ {
		    # fileevent tells us there's input to be read
		    # check the channel
		    set gone [catch {eof $socket} eof]
		    if {$gone || $eof} {
			Debug.Httpd {[info coroutine] eof detected from yield}
			terminate "EOF on reading"
		    } else {
			watchdog
			return $args
		    }
		}

		CLOSING {
		    # fileevent tells us there's input, but we're half-closed
		    # and won't process any more input, but we want to send
		    # all pending responses
		    set gone [catch {chan eof $socket} eof]
		    if {$gone || $eof} {
			# remote end closed - just forget it
			terminate "socket is closed"
		    } else {
			# just read incoming data
			watchdog
			set x [chan read $socket]
			Debug.Httpd {[info coroutine] is closing, read [string length $x] bytes}
		    }
		}

		SEND {
		    # send a response to client
		    watchdog
		    set retval [send {*}$args]
		}

		WRITE {
		    # there is space available in the output queue
		    set retval [respond {*}$args]
		}

		SUSPEND {
		    #puts stderr "SUSPEND: ($args)"
		    grace [lindex $args 0]	;# a response has been suspended
		}

		REAPED {
		    # we've been reaped
		    corovars satisfied ipaddr closing headering
		    Debug.Watchdog {[info coroutine] Reaped - status:($status) satisfied:($satisfied) unsatisfied:($unsatisfied) ipaddr:$ipaddr closing:$closing headering:$headering}
		    
		    terminate {*}$args
		}

		TERMINATE {
		    # we've been informed that the socket closed
		    terminate {*}$args
		}

		TIMEOUT {
		    # we've timed out - oops
		    terminate TIMEOUT
		}

		FCOPY {
		    fcopy_complete {*}$args
		}

		default {
		    error "[info coroutine]: Unknown op $op $args"
		}
	    }
	}
    }

    # handle - handle a protocol error
    proc handle {req {reason "Error"}} {
	Debug.error {handle $reason: ([rdump $req])}

	# we have an error, so we're going to try to reply then die.
	corovars transaction generation status closing socket
	lappend status ERROR
	if {[catch {
	    dict set req connection close	;# we want to close this connection
	    if {![dict exists $req -transaction]} {
		dict set req -transaction [incr transaction]
	    }
	    dict set req -generation $generation

	    # send a response to client
	    send $req 0	;# queue up error response (no caching)
	} r eo]} {
	    dict append req -error "(handler '$r' ($eo))"
	    Debug.error {'handle' error: '$r' ($eo)}
	}

	# return directly to event handler to process SEND and STATUS
	set closing 1
	readable CLOSING

	Debug.error {'handle' closing}
	return -level [expr {[info level] - 1}]	;# return to the top coro level
	#rename [info coroutine] ""; ::yield	;# terminate coro
    }

    # coroutine-enabled gets
    proc get {socket {reason ""}} {
	Debug.HttpdLow {[info coroutine] get started}
	variable maxline
	set result [yield]
	set line ""
   	set gone [catch {eof $socket} eof]
	while {[set status [chan gets $socket line]] == -1 && !$gone && !$eof} {
	    Debug.HttpdLow {[info coroutine] gets $socket - status:$status '$line'}
	    set result [yield]
	    if {$maxline && [chan pending input $socket] > $maxline} {
		error "Line too long (over $maxline) '[string range $line 0 20]..."
	    }
	    set gone [catch {eof $socket} eof]
	}
	Debug.HttpdLow {[info coroutine] get - success:$status}

	set gone [catch {chan eof $socket} eof]
	if {$gone || $eof} {
	    Debug.HttpdLow {[info coroutine] eof in get}
	    terminate $reason	;# check the socket for closure
	}

	# return the line
	Debug.HttpdLow {[info coroutine] get: '$line' [chan blocked $socket] [chan eof $socket]}
	return $line
    }
    
    # coroutine-enabled read
    proc read {socket size} {
    	# read a chunk of size bytes
	set chunk ""
	set gone [catch {chan eof $socket} eof]
	while {$size && !$gone && !$eof} {
	    set result [yield]
	    set chunklet [chan read $socket $size]
	    incr size -[string length $chunklet]
	    append chunk $chunklet
	    set gone [catch {chan eof $socket} eof]
	}

	set gone [catch {chan eof $socket} eof]
	if {$gone || $eof} {
	    Debug.HttpdLow {[info coroutine] eof in read}
	    terminate "eof in reading entity - $size"	;# check the socket for closure
	}
	
	# return the chunk
	Debug.HttpdLow {[info coroutine] read: '$chunk'}
    	return $chunk
    }

    proc parse {lines} {
	# we have a complete header - parse it.
	set r {}
	set last ""
	set size 0
	foreach line $lines {
	    if {[string index $line 0] in {" " "\t"}} {
		# continuation line
		dict append r $last " [string trim $line]"
		set key $last	;# remember key for length checking
	    } else {
		set value [join [lassign [split $line ":"] key] ":"]
		set key [string tolower [string trim $key "- \t"]]
		
		if {[dict exists $r $key]} {
		    dict append r $key ",$value"
		} else {
		    dict set r $key [string trim $value]
		}
	    }

	    # limit size of each field
	    variable maxfield
	    if {$maxfield
		&& [string length [dict get $r $key]] > $maxfield
	    } {
		handle [Http Bad $r "Illegal header: '[string range $line 0 20]...' [string length $dict get $r $key] is too long"] "Illegal Header - [string length $dict get $r $key] is too long"
	    }
	}

	return $r
    }

    proc reader {args} {
	Debug.Httpd {create reader [info coroutine] - $args}

	# unpack all the passed-in args
	set replies {}	;# dict of replies pending
	set requests {}	;# dict of requests unsatisfied
	set satisfied {};# dict of requests satisfied
	set unsatisfied {} ;# dict of requests unsatisfied
	set response 1	;# which is the next response to send?
	set sequence -1	;# which is the next response to queue?
	set writing 0	;# we're not writing yet
	set ipaddr 0	;# ip address
	set events {}	;# readable/writable
	readable	;# kick off the readable event

	dict with args {}
	set transaction 0	;# count of incoming requests
	set status INIT	;# record transitions
	set closing 0	;# flag that we want to close
	variable files; dict set files [info coroutine] $socket {}

	# keep receiving input requests
	while {1} {
	    # get whole header
	    set headering 1
	    set lines {}
	    set hstart 0
	    while {$headering} {
		set line [get $socket HEADER]
		if {!$hstart} {
		    set hstart [clock microseconds]
		}
		Debug.HttpdLow {reader [info coroutine] got line: ($line)}
		if {[string trim $line] eq ""} {
		    # rfc2616 4.1: In the interest of robustness,
		    # servers SHOULD ignore any empty line(s)
		    # received where a Request-Line is expected.
		    if {[llength $lines]} {
			set headering 0
		    }
		} else {
		    lappend lines $line
		}
	    }

	    # parse the header into a request
	    set h [parse [lrange $lines 1 end]]	;# parse the header
	    set r [dict merge $prototype $h]

	    set start [clock microseconds]
	    dict set r -htime [expr {$start - $hstart}]
	    dict set r -received $start
	    dict set r -transaction [incr transaction]
	    dict set r -sock $socket
	    dict set r -clientheaders [dict keys $h]

	    # unpack the header line
	    set header [lindex $lines 0]
	    dict set r -header $header
	    dict set r -method [string toupper [lindex $header 0]]
	    switch -- [dict get $r -method] {
		CONNECT -
		LINK {
		    # stop the bastard SMTP spammers
		    Block block [dict get $r -ipaddr] "[dict get $r -method] method ([dict get? $r user-agent])"
		    handle [Http NotImplemented $r "Connect Method"] "CONNECT method"
		}

		GET - PUT - POST - HEAD {}

		default {
		    # Could check for and service FTP requests, etc, here...
		    dict set r -error_line $line
		    handle [Http Bad $r "Method unsupported '[lindex $header 0]'" 405] "Method Unsupported"
		}
	    }

	    # get and test HTTP version
	    dict set r -version [lindex $header end]		;# HTTP version
	    if {[string match HTTP/* [dict get $r -version]]} {
		dict set r -version [lindex [split [dict get $r -version] /] 1]
	    }
	    # Send 505 for protocol != HTTP/1.0 or HTTP/1.1
	    if {[dict get $r -version] ni {1.1 1.0}} {
		handle [Http Bad $r "HTTP Version '[dict get $r -version]' not supported" 505] "Unsupported HTTP Version"
	    }

	    # get request URL
	    # check URI length (per rfc2616 3.2.1
	    # A server SHOULD return 414 (Requestuest-URI Too Long) status
	    # if a URI is longer than the server can handle (see section 10.4.15).)
	    variable maxurilen
	    dict set r -uri [join [lrange $header 1 end-1]]	;# requested URL
	    if {$maxurilen && [string length [dict get $r -uri]] > $maxurilen} {
		# send a 414 back
		handle [Http Bad $r "URI too long '[dict get $r -uri]'" 414] "URI too long"
	    }

	    Debug.Httpd {[info coroutine] reader got request: ($r)}

	    # parse the URL
	    set r [dict merge $r [Url parse [dict get $r -uri] 1]]

	    # check the incoming ip for blockage
	    if {[Block blocked? [dict get? $r -ipaddr]]} {
		handle [Http Forbidden $r] Forbidden
		continue
	    }

	    # analyse the user agent strings.
	    dict set r -ua [UA parse [dict get? $r user-agent]]
	    dict set r -ua_class [UA classify [dict get? $r user-agent]]	;# classify client by UA
	    switch -- [dict get $r -ua_class] {
		blank {
		    handle [Http NotImplemented $r "Possible Spider Service - set your User-Agent"] "Spider"
		}
		spammer {
		    Block block [dict get $r -ipaddr] "spider UA ([dict get? $r user-agent])"
		    handle [Http NotImplemented $r "Spider Service"] "Spider"
		}

		browser {
		    # let the known browsers through
		}

		unknown {
		    #Debug.log {unknown UA: [dict get $r user-agent]}
		}

		default {
		    # dict set r -dynamic 1	;# make this dynamic
		}
	    }

	    # ensure that the client sent a Host: if protocol requires it
	    if {[dict exists $r host]} {
		# client sent Host: field
		if {[string match http*:* [dict get $r -uri]]} {
		    # rfc 5.2 1 - a host header field must be ignored
		    # if request-line specified an absolute URL host/port
		    set r [dict merge $r [Url parse [dict get $r -uri]]]
		    dict set r host [Url host $r]
		} else {
		    # no absolute URL was specified by the request-line
		    # use the Host field to determine the host
		    foreach c [split [dict get $r host] :] f {host port} {
			dict set r -$f $c
		    }
		    dict set r host [Url host $r]
		    set r [dict merge $r [Url parse http://[dict get $r host][dict get $r -uri]]]
		}
	    } elseif {[dict get $r -version] > 1.0} {
		handle [Http Bad $r "HTTP 1.1 required to send Host"] "No Host"
	    } else {
		# HTTP 1.0 isn't required to send a Host request but we still need it
		if {![dict exists $r -host]} {
		    # make sure the request has some idea of our host&port
		    dict set r -host $host
		    dict set r -port $port
		    dict set r host [Url host $r]
		}
		set r [dict merge $r [Url parse http://[Url host $r]/[dict get $r -uri]]]
	    }
	    dict set r -url [Url url $r]	;# normalize URL

	    # rfc2616 14.10:
	    # A system receiving an HTTP/1.0 (or lower-version) message that
	    # includes a Connection header MUST, for each connection-token in this
	    # field, remove and ignore any header field(s) from the message with
	    # the same name as the connection-token.
	    if {[dict get $r -version] < 1.1 && [dict exists $r connection]} {
		foreach token [split [dict get $r connection] ","] {
		    catch {dict unset r [string trim $token]}
		}
		dict unset r connection
	    }

	    # completed request header decode - now dispatch on the URL
	    Debug.Httpd {[info coroutine] reader complete: $header ([rdump $r])}

	    # rename fields whose names are the same in request/response
	    foreach n {cache-control pragma} {
		if {[dict exists $r $n]} {
		    dict set r -$n [dict get $r $n]
		    dict unset r $n
		}
	    }

	    # rfc2616 4.3
	    # The presence of a message-body in a request is signaled by the
	    # inclusion of a Content-Length or Transfer-Encoding header field in
	    # the request's headers.
	    if {[dict exists $r transfer-encoding]} {
		set te [dict get $r transfer-encoding]
		Debug.entity {got transfer-encoding: $te}

		# chunked 3.6.1, identity 3.6.2, gzip 3.5, compress 3.5, deflate 3.5
		set tels {}
		array set params {}

		variable te_encodings
		variable te_params
		foreach tel [split $te ,] {
		    set param [lassign [split $tel ";"] tel]
		    set tel [string trim $tel]
		    if {$tel ni $te_encodings} {
			# can't handle a transfer encoded entity
			Debug.log {Got a $tel transfer-encoding which we can't handle}
			handle [Http NotImplemented $r "$tel transfer encoding"] "Unimplemented TE"
			continue
			# see 3.6 - 14.41 for transfer-encoding
			# 4.4.2 If a message is received with both
			# a Transfer-EncodIing header field
			# and a Content-Length header field,
			# the latter MUST be ignored.
		    } else {
			lappend tels $tel
			set params($tel) [split $param ";"]
		    }
		}

		dict set r -te $tels
		dict set r -te_params [array get params]
	    } elseif {[dict get $r -method] in {POST PUT}
		      && ![dict exists $r content-length]} {
		dict set r -te {}
		
		# this is a content-length driven entity transfer
		# 411 Length Required
		handle [Http Bad $r "Length Required" 411] "Length Required"
	    }

	    if {[dict get $r -version] >= 1.1
		&& [dict exists $r expect]
		&& [string match *100-continue* [string tolower [dict get $r expect]]]
	    } {
		# the client wants us to tell it to continue
		# before reading the body.
		# Do so, then proceed to read
		puts -nonewline $socket "HTTP/1.1 100 Continue\r\n"
	    }

	    # fetch the entity (if any)
	    if {"chunked" in [dict get? $r -te]} {
		Debug.entity {got chunked content [dict get? $r -te]}
		set chunksize 1
		while {$chunksize} {
		    chan configure $socket -translation {crlf binary}
		    set chunksize 0x[get $socket CHUNK]
		    chan configure $socket -translation {binary binary}
		    if {$chunksize eq "0x"} {
			Debug.HttpdLow {[info coroutine] Chunks all done}
			break	;# collected all the chunks
		    }
		    set chunk [read $socket $chunksize]
		    Debug.HttpdLow {[info coroutine] Chunk: $chunksize ($chunk)}
		    get $socket CHUNK
		    dict append r -entity $chunk

		    # enforce server limits on Entity length
		    variable maxentity
		    if {$maxentity > 0
			&& [string length [dict get $r -entity]] > $maxentity} {
			# 413 "Request Entity Too Large"
			handle [Http Bad $r "Request Entity Too Large" 413] "Entity Too Large"
		    }
		}
		Debug.entity {got chunked entity of length [string length [dict get $r -entity]]}

	    } elseif {[dict exists $r content-length]} {
		set left [dict get $r content-length]
		Debug.entity {content-length: $left}

		# enforce server limits on Entity length
		variable maxentity
		if {$maxentity > 0 && $left > $maxentity} {
		    # 413 "Request Entity Too Large"
		    handle [Http Bad $r "Request Entity Too Large" 413] "Entity Too Large"
		}

		if {$left == 0} {
		    dict set r -entity ""
		    # the entity, length 0, is therefore already read
		    # 14.13: Any Content-Length greater than
		    # or equal to zero is a valid value.
		} else {
		    set entity ""
		    chan configure $socket -translation {binary binary}
		    Debug.HttpdLow {[info coroutine] reader getting entity of length ($left)}
		    while {$left > 0} {
			set chunk [read $socket $left]
			incr left -[string length $chunk]
			Debug.HttpdLow {[info coroutine] reader getting remainder of entity of length ($left)}
			dict append r -entity $chunk
			Debug.HttpdLow {[info coroutine] reader got whole entity}
		    }
		}
		Debug.entity {entity of length: [string length [dict get $r -entity]]}
	    }

	    # reset socket to header config, having read the entity
	    chan configure $socket -encoding binary -translation {crlf binary}

	    # now we postprocess/decode the entity
	    Debug.entity {entity read complete - '[dict get? $r -te]'}
	    if {"gzip" in [dict get? $r -te]} {
		dict set r -entity [zlib deflate [dict get $r -entity]]
	    }

	    # remove 'netscape extension' length= from if-modified-since
	    if {[dict exists $r if-modified-since]} {
		dict set r if-modified-since [lindex [split [dict get $r if-modified-since] {;}] 0]
	    }

	    # trust x-forwarded-for if we get a forwarded request from a local ip
	    # (presumably local ip forwarders are trustworthy)
	    set forwards {}
	    if {[dict exists $r x-forwarded-for]} {
		foreach xff [split [dict get? $r x-forwarded-for] ,] {
		    set xff [string trim $xff]
		    set xff [lindex [split $xff :] 0]
		    if {$xff eq ""
			|| $xff eq "unknown"
			|| [Http nonRouting? $xff]
		    } continue
		    lappend forwards $xff
		}
	    }
	    #lappend forwards [dict get $r -ipaddr]
	    dict set r -forwards $forwards
	    #dict set r -ipaddr [lindex $forwards 0]

	    # check Cache for match
	    if {[dict size [set cached [Cache check $r]]] > 0} {
		# reply directly from cache
		dict set unsatisfied [dict get $cached -transaction] {}
		dict set cached -caching retrieved
		dict set cached -sent [clock microseconds]

		Debug.Httpd {[info coroutine] sending cached [dict get $r -uri] ([rdump $cached])}
		lappend status CACHED
		if {[catch {
		    write [dict merge $r $cached] 0	;# write cached response directly into outgoing structs
		} result eo]} {
		    Debug.error {FAILED write $result ($eo) IP [dict get $r -ipaddr] ([dict get? $r user-agent]) wanted [dict get $r -uri]}
		    terminate closed
		}
		#lassign result close cache
		continue	;# go get the next request
	    }

	    # process the request
	    dict set unsatisfied [dict get $r -transaction] {}
	    lappend status PROCESS

	    dict set r -send [info coroutine]	;# remember the coroutine
	    catch {
		do REQUEST [pre $r]
	    } rsp eo	;# process the request

	    # handle response code from processing request
	    switch -- [dict get $eo -code] {
		0 -
		2 {
		    # does application want to suspend?
		    if {[dict size $rsp] == 0 || [dict exists $rsp -suspend]} {
			if {[dict size $rsp] == 0} {
			    set duration 0
			} else {
			    set duration [dict get $rsp -suspend]
			}
			
			Debug.Httpd {SUSPEND: $duration}
			grace $duration	;# response has been suspended
			continue
		    } elseif {[dict exists $rsp -passthrough]} {
			# the output is handled elsewhere (as for WOOF.)
			# so we don't need to do anything more.
			continue
		    }
		    
		    # ok - return
		    if {![dict exists $rsp -code]} {
			set rsp [Http Ok $rsp]	;# default to OK
		    }
		}
		
		1 { # error - return the details
		    set rsp [Http ServerError $r $rsp $eo]
		}
	    }

	    watchdog
	    if {[catch {
		post $rsp
	    } rspp eo]} {
		# post-processing error - report it
		Debug.error {[info coroutine] postprocess error: $rspp ($eo)} 1
		watchdog

		# report error from post-processing
		send [::Convert do [Http ServerError $r $rspp $eo]]
	    } else {
		# send the response to client
		Debug.Httpd {[info coroutine] postprocess: [rdump $rspp]} 10
		watchdog

		# does post-process want to suspend?
		if {[dict size $rspp] == 0 || [dict exists $rspp -suspend]} {
		    if {[dict size $rspp] == 0} {
			# returning a {} from postprocess suspends it ... really?
			set duration 0
		    } else {
			# set the grace duration as per request
			set duration [dict get $rspp -suspend]
		    }
		    
		    Debug.Httpd {SUSPEND in postprocess: $duration}
		    grace $duration	;# response has been suspended for $duration
		    continue
		} elseif {[dict exists $rspp -passthrough]} {
		    # the output is handled elsewhere (as for WOOF.)
		    # so we don't need to do anything more.
		    continue
		} else {
		    send $rspp	;# send the response through to client
		}
	    }
	}
    }

    # handle responses from a client
    proc client {op connection args} {
	variable client
	if {[info exists client($connection)]} {
	    apply $client($connection) $op $connection {*}$args
	}
    }

    # return a bunch of status information about sock procs
    proc stats {} {
	set result {}
	foreach coro [info commands ::Httpd::sock*] {
	    lappend result $coro [$coro STATS]
	}
	return $result
    }

    # return a bunch of data about all the channels in use by Httpd
    proc chans {} {
	foreach chan [chan names] {
	    catch {
		list eof [chan eof $chan] input [chan pending input $chan] output [chan pending output $chan] blocked [chan blocked $chan] readable [chan event $chan readable] writable [chan event $chan writable] {*}[chan configure $chan]
	    } el
	    lappend result "name $chan $el"
	}
	return $result
    }

    # tickle the watchdog
    proc watchdog {} {
	variable activity
	# record fact of activity on this coro, which will prevent its being reaped
	set activity([info coroutine]) [clock milliseconds]
    }

    # grant the caller some timeout grace
    proc grace {{grace 20000}} {
	variable activity
	if {$grace < 0} {
	    # take this coro off the reaper's list until next activity
	    catch {unset activity([info coroutine])}
	} else {
	    set activity([info coroutine]) [expr {$grace + [clock milliseconds]}]
	}
    }

    # format something to suspend this packet
    proc Suspend {r {grace -1}} {
	Debug.Httpd {Suspending [rdump $r]}
	dict set r -suspend $grace
	return $r
    }

    # resume this request
    proc Resume {r {cache 1}} {
	Debug.Httpd {Resuming [rdump $r]}
        # ask socket coro to send the response for us
	# we inject the SEND event into the coro so Resume may be called from any
	# event, thread or coroutine
	set r [post $r]
	return [catch {{*}[dict get $r -send] [list SEND $r]}]
    }

    # every script
    proc every {interval script} {
	variable everyIds
	if {$interval eq "cancel"} {
	    after cancel $everyIds($script)
	    return
	}
	set everyIds($script) [after $interval [info level 0]]	;# restart the timer
	set rc [catch {
	    uplevel #0 $script
	} result eo]
	if {$rc == [catch break]} {
	    after cancel $everyIds($script)
	    set rc 0
	} elseif {$rc == [catch continue]} {
	    # Ignore - just consume the return code
	    set rc 0
	} elseif {$rc == [catch error]} {
	    Debug.error {every: $interval ($script) - ERROR: $result ($eo)}
	}

	# TODO: Need better handling of errorInfo etc...
	#return -code $rc $result
	return $result
    }

    proc kill {args} {
	Debug.Watchdog {killing: "$args"}
	variable files
	foreach what $args {
	    if {[catch {
		rename $what {}	;# kill this coro right now
		unset crs($what) ;# remove record of coroutine activity
	    } e eo]} {
		Debug.error {killed $what: '$r' ($eo)}
	    }

	    if {[dict exists $files $what]} {
		foreach fd [dict keys [dict get $files $what]] {
		    if {[catch {chan close $fd} e eo]} {
			# close coro's file
			Debug.error {closing $what's $fd: '$e' ($eo)}
		    }
		}
		dict unset files $what
	    }
	}
    }

    variable reaper	;# array of hardline events 
    proc reaper {} {
	variable timeout
	set now [clock milliseconds]
	set then [expr {$now - $timeout}]
	Debug.Watchdog {Reaper Running [Http Now]}

	# kill any moribund coroutines
	variable reaper
	foreach {n v} [array get reaper] {
	    unset reaper($n)
	    if {$v < $now} {
		catch {kill $n}
	    }
	}

	# close any files at EOF
	foreach s [chan names] {
	    catch {
		if {[catch {chan eof $s} eof] || $eof} {
		    if {[catch {chan close $s} e eo]} {
			Debug.Watchdog {closing $s: $e ($eo)}
		    }
		}
	    }
	}

	# schedule inactive coroutines for reaping
	variable activity
	foreach {n v} [array get activity] {
	    catch {
		if {[info commands $n] eq {}} {
		    catch {unset activity($n)}	;# this is bogus
		} elseif {$v < $then} {
		    Debug.Watchdog {Reaping $n}
		    catch {unset activity($n)}	;# prevent double-triggering
		    catch {$n REAPED}	;# alert coro to its fate
		    set reaper($n) [expr {$now + 2 * $timeout}]	;# if it doesn't respond, kill it.
		}
	    }
	}
    }

    proc pre {r} {
	package require Cookies
	proc pre {r} {
	    # default request pre-process
	    catch {::pest $r}
	    set r [::Cookies 4Server $r]	;# fetch the cookies
	    set r [Human track $r]	;# track humans by cookie
	    return $r
	}
	return [pre $r]
    }

    proc post {r} {
	package require Convert
	proc post {r} {
	    if {[dict exists $r -postprocess]} {
		set r [{*}[dict get $r -postprocess] $r]
	    }
	    set r [::Convert do $r]	;# got to do post-conversion
	    return $r
	}
	return [post $r]
    }

    # Authorisation
    variable realms
    proc addRealm {realm args} {
	set realms($realm) $args
    }

    proc Auth {r realm} {
	variable realms
	dict set r -realm $realm
	if {[dict exists $r code]} {dict unset r code}
	set r [{*}$realms($realm) $r {*}$realm]
	if {[dict exists $r code]} {
	    return -level 1 $r	;# return from the caller (do)
	}
    }

    proc do {op req} {
	#Debug on Httpd 10
	#Debug on HttpdLow 10
	if {[info commands ::wub] eq {}} {
	    package require Mason
	    Mason create ::wub -url / -root $::Site::docroot -auth .before -wrapper .after
	}

	proc do {op req} {
	    switch -- $op {
		REQUEST {
		    switch -glob -- [dict get $req -path] {
			/ -
			/* {
			    # redirect / to /wub
			    return [::wub do $req]
			}
		    }
		}
		TERMINATE {
		    return
		}
		RESPONSE {
		    # HTTP client has sent us a response
		}
		CLOSED {
		    # HTTP client has closed.
		}
		default {
		    error "[info coroutine] OP $op not understood by consumer"
		}
	    }
	}
	return [do REQUEST $req]
    }

    # connect - process a connection request
    proc Connect {sock ipaddr rport args} {
	Debug.Httpd {Connect $sock $ipaddr $rport $args}
	if {[catch {
	    set s [Socket new chan $sock -file sock.dump -capture 0]
	    chan create {read write} $s
	} ns eo]} {
	    # failed to connect.  This can be due to overconnecting
	    Debug.error {connection error from $ipaddr:$rport - $ns ($eo)}
	    
	    variable exhaustion
	    variable server_id
	    set msg [dict get? [Http Unavailable {} "$ns ($eo)" $exhaustion] -content]
		
	    puts $sock "HTTP/1.1 503 Exhaustion\r"
	    puts $sock "Date: [Http Now]\r"
	    puts $sock "Server: $server_id\r"
	    puts $sock "Connection: Close\r"
	    puts $sock "Content-Length: [string length $msg]\r"
	    puts $sock \r
	    puts $sock -nonewline $msg
	    flush $sock
	    close $sock
	    return ""
	} else {
	    set sock $ns
	}

	# the socket must stay in non-block binary binary-encoding mode
	chan configure $sock -blocking 0 -translation {binary binary} -encoding binary

	# check for Block on this ipaddress
	switch -- [::ip::type $ipaddr] {
	    "normal" {
		# check list of blocked ip addresses
		if {[Block blocked? $ipaddr]} {
		    # dump this connection with a minimum of fuss.
		    variable server_id
		    puts $sock "HTTP/1.1 403 Forbidden\r"
		    puts $sock "Date: [Http Now]\r"
		    puts $sock "Server: $server_id\r"
		    puts $sock "Connection: Close\r"
		    puts $sock "Content-Length: 0\r"
		    puts $sock \r
		    flush $sock
		    close $sock
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

	# record connection id - unique over the life of this server process
	variable cid; set id [incr cid]
	dict set args -cid $id

	# record significant values
	dict set args -sock $sock
	dict set args -ipaddr $ipaddr
	dict set args -rport $rport
	dict set args -received_seconds [clock seconds]

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
	dict set args -version 1.1	;# HTTP/1.1

	# condition the socket
	chan configure $sock -buffering none -translation {crlf binary}

	# generate a connection record prototype
	variable generation	;# unique generation
	set gen [incr generation]
	set args [dict merge $args [list -generation $gen]]

	# send that we accept ranges
	dict set args accept-ranges bytes

	# create reader coroutine
	variable reader
	set R ::Httpd::${sock}_[uniq]

	# the old socket stuff hasn't yet been cleaned up.
	# this is potentially very bad.
	foreach n [info commands ::Httpd::${sock}_*] {
	    Debug.log {reader $R not dead yet, rename to ${R}_DEAD to kill it.}
	    catch {rename $n DEAD_$n}
	    catch {DEAD_$n [list TERMINATE "socket's gone"]}	;# ensure the old reader's dead
	    catch {rename DEAD_$n ""}
	}

	# construct the reader
	variable timeout
	variable log
	set result [coroutine $R ::Httpd::reader socket $sock prototype $args generation $gen cid $cid log $log]

	return $result
    }

    # configure - set Httpd protocol defaults
    proc configure {args} {
	variable {*}$args

	# open the web analysis log
	variable logfile
	variable log
	if {$logfile ne "" && $log eq ""} {
	    if {![catch {
		open $logfile a
	    } log] || ![catch {
		open [file join /tmp [file tail $logfile]] a
	    } log]} {
		# we want to try to make writes atomic
		fconfigure $log -buffering line
	    }
	}

	# source in local customisations for Httpd
	# mainly useful for [pre] [post] and [pest]
	variable customize
	if {$customize ne ""} {
	    set eo {}
	    catch {source $customize} result eo
	    Debug.log {Httpd Customisations from '$customize'->$result ($eo)}
	}

	variable maxconn
	if {[info exists maxconn]} {
	    Socket new -maxconnections $maxconn
	}
    }

    # called by logrotate to rotate log file
    proc logrotate {} {
	# open the web analysis log
	variable logfile
	variable log
	if {$log ne ""} {
	    close $log
	}
	if {$logfile ne ""} {
	    set log [open $logfile a]		;# always add to the end
	    fconfigure $log -buffering line	;# we want to try to make writes atomic
	}
    }

    proc start {} {
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

    set listener [Listener %AUTO% -port 8080 -sockets Httpd -httpd {-dispatch "puts"}]
    set forever 0
    vwait forever
}

Httpd every $Httpd::timeout {Httpd reaper}	;# start the inactivity reaper
