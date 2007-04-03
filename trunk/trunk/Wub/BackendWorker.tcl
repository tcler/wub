interp bgerror {} bgerror
proc bgerror {error eo} {
    puts stderr "Thread [::thread::id]: $error ($eo)"
    if {[dict get $eo -code] == 1} {
	disconnect $error [Http ServerError $::request $error $eo]
    }
}

package require Debug
package require Url
package require struct::queue
package require Http
package require Cookies
package require Session
package require Form

# initialise the SessionWorker
Session init $config(sdb) session $config(bfl)

# create a queue of pending work
::struct::queue inQ

variable request ""

# conversion of results
package require Convert
Convert convert

# some directories are just files
package require File
package require File
foreach {dom expiry} {css {tomorrow} images 0 templates 0 scripts {tomorrow} img {next week}} {
    File $dom -root [file join $config(docroot) $dom] -expires $expiry
}

# the rest require masonic intervention
package require Mason
Mason mason -root $config(docroot)

package require Direct
Direct direct -namespace ::DirectTest

foreach {d ns} {login ::Login A ::Admin} {
    Direct $d -namespace $ns 
}

proc do {args} {
    variable response
    variable request
    set code [catch {{*}$args} r eo]
    switch -- $code {
	1 {
	    set response [Http ServerError $request $r $eo]
	    return 1
	}
	default {
	    set response $r
	    if {$code == 0} {
		set code 200
	    }
	    if {![dict exists $response -code]} {
		dict set response -code $code
	    }
	    return 0
	}
    }
}

proc incoming {req} {
    inQ put $req

    variable response
    variable request
    while {([dict size $request] == 0)
	   && ([catch {inQ get} req eo] == 0)
       } {
	set request $req

	set path [dict get $request -path]
	set host [dict get $request -host]
	dict set request -Query [Query parse $request]	;# parse the query
	set cookie [Session fetch $request]

	# get a plausible prefix/suffix split
	dict set request -suffix [file join {} {*}[lassign [file split $path] -> fn]]
	dict set request -prefix "/$fn"
	
	Debug.dispatch {matching domain: $host,$path - '[file split $path]'}
	switch -glob -- $host,$path {
	    *,/favicon.ico {
		# need to silently redirect /favicon.ico
		Debug.wiki {favicon hack}
		dict set request -suffix [file join {} {*}[lrange [file split $path] 1 end]]
		dict set request -prefix "/images"
		do images do $request
	    }

	    *,/css/* -
	    *,/images/* -
	    *,/templates/* -
	    *,/scripts/* -
	    *,/direct -
	    *,/includes/* {
		Debug.http {matched $path}
		dict set request -suffix [file join {*}[lassign [file split $path] -> fn]]
		do $fn do $request
	    }

	    *,/css -
	    *,/images -
	    *,/templates -
	    *,/scripts -
	    *,/direct -
	    *,/includes {
		# redirect any set dir without a trailing /
		dict set request -path $path/	;# missing trailing /
		do Http Redirect $req [Url uri $request]
	    }

	    default {
		Debug.http {no match $path}
		dict set request -suffix $path
		do mason do $request
	    }
	}

	set response [Session store $response $cookie]

	# send response
	do convert do $response
	dict set response -transaction [dict get $request -transaction]
	dict set response -generation [dict get $request -generation]

	::thread::send -async [dict get $request -worker] [list send $response]
	set request [dict create]	;# go idle
    }
}

Debug off direct 10
Debug off file 10
Debug off convert 10
Debug off db 
Debug off http
Debug off session 10
Debug off form 10

thread::wait
