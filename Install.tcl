# Install.tcl -- download or update Wub from its svn repository

if {[catch {package require Tcl 8.6}]} {
    puts stderr "Tcl 8.6 required, you have [package provide Tcl]"
}
if {[catch {package require fileutil}]} {
    puts stderr "tcllib required, doesn't appear to be present"
}

package require http
package provide Install 1.0

namespace eval Install {
    variable base http://wub.googlecode.com/svn/trunk/
    variable home [file dirname [info script]]

    proc gotfile {file token} {
	if {[::http::status $token] ne "ok"} {
	    puts stderr "Failed to fetch file $file"
	    getter [list FILE $file]
	    error "Failed to fetch file $file"
	}
	if {[catch {
	    variable home
	    ::fileutil::writeFile -encoding binary [file join $home $file] [::http::data $token]
	    ::http::cleanup $token
	} e eo]} {
	    puts stderr "gotfile error: $e ($eo)"
	}
	getter [list FILE $file]
    }

    proc gotdir {dir token} {
	if {[::http::status $token] ne "ok"} {
	    puts stderr "Failed to fetch dir $dir"
	    getter [list DIR $file]
	    error "Failed to fetch dir $dir"
	}
	if {[catch {
	    variable home
	    if {![file exists [file join $home $dir]]} {
		puts stderr "Making directory '$dir' in '$home'([namespace current])"
		if {[catch {file mkdir [file join $home $dir]} e eo]} {
		    error $e
		}
	    }
	    
	    set body [::http::data $token]
	    set urls [regexp -inline -all -- {href="([^\"]+)"} $body]
	    puts "gotdir '$dir' URLS: $urls"
	    set urls [dict values $urls]
	    variable base
	    foreach name $urls {
		set name [string map [list $base/ ""] $name]
		switch -glob -- $name {
		    http://* -
		    .* {
			puts "discarding $name"
			continue
		    }
		    */ {
			puts "processing dir $name"
			getter [list dir [file join $dir $name]/]
		    }
		    default {
			puts "processing file $name"
			getter [list file [file join $dir $name]]
		    }
		}
	    }
	    puts "processed dir $dir"
	    ::http::cleanup $token
	} e eo]} {
	    puts "gotdir error $e ($eo)"
	}
	getter [list DIR $dir]
    }

    proc getC {args} {
	variable queue
	variable base
	variable limit
	variable loading 0
	variable loaded 0
	variable pending {}

	while {1} {
	    if {[catch {
		lassign $args op path

		# first process any completed fetches
		switch -- $op {
		    FILE -
		    DIR {
			incr loaded
			incr loading -1
			dict unset pending $path
			puts stderr "DONE $loaded: $op $path ($loading/$limit) queue: [llength $queue] pending: [dict keys $pending]"
			set queue [lassign $queue op path]
		    }
		}
		
		switch -- $op {
		    "" {
			# nothing more queued yet.
		    }
		    file -
		    dir {
			if {$loading < $limit} {
			    incr loading 1
			    variable base
			    set cmd [list ::http::geturl $base/$path -command [namespace code [list got$op $path]]]
			    puts stderr "GETTING: $op $path $loading/$limit ($cmd)"
			    puts stderr "$cmd"
			    dict set pending $path $op
			    {*}$cmd
			} else {
			    lappend queue $op $path
			    puts stderr "QUEUEING: $op $path $loading/$limit queued: [llength $queue] pending:[dict keys $pending]"
			}
		    }
		    op {}
		    
		    default {
			error "getter doesn't do $op $path"
		    }
		}
	    } e eo]} {
		puts stderr "CORO error: $e ($eo)"
	    }
	    set args [yield]
	}	 
    }

    proc waiter {} {
	variable queue
	variable loading
	while {1} {
	    vwait loading
	    puts "countdown: $loading/$limit queued: [llength $queue]"
	    if {$loading == 0} {
		variable loaded
		return $loaded
	    }
	}
    }

    proc fetch {args} {
	variable limit 10
	variable {*}$args

	variable home [file normalize $home]
	variable base [string trimright $base /]
	puts "Install fetch $base to $home"
	coroutine ::Install::getter getC dir
	if {[info exists wait] && $wait} {
	    waiter
	}
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

Install fetch {*}$argv
Install waiter
