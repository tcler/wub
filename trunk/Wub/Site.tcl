# Site - simple configuration for single-threaded Wub Server.

package require fileutil

package provide Site 1.0

namespace eval Site {
    # simple rc reader
    proc rc {text} {
	set accum ""
	set result {}
	foreach line [split $text \n] {
	    set line [string trim $line]
	    if {$line eq ""} continue
	    lassign [split $line {\#;}] line
	    append accum " " $line
	    if {[info complete $accum]} {
		uplevel 1 variable [uplevel 1 [list subst $accum]]
		set accum ""
	    }
	}
    }

    #### Configuration
    # set some default configuration flags and values
    foreach {name val} [rc {
	host [info hostname]
	multi 0

	globaldocroot 1
	backends 5
	cmdport 8082

	home [file normalize [file dirname [info script]]]
	wubdir [file join $home ..]
	local [file join [file dirname [info script]] local.tcl]
	vars [file join [file dirname [info script]] vars.tcl]

	listener {-port 8080}
	scgi {-port 8088 -scgi_send {::scgi Send}}

	backend [subst {
	    scriptdir [file dirname [info script]]
	    scriptname Worker.tcl
	    dispatch Backend
	}]

	httpd {
	    max 1
	    incr 1
	    over 40
	    dispatch ""
	}
    }] {
	variable $name
	set $name $val
    }

    # load site configuration script (not under SVN control)
    if {$vars ne ""} {
	catch {eval [::fileutil::cat $vars]}
    }

    # load command-line configuration vars
    foreach {name val} $::argv {
	variable $name
	set $name $val	;# set global config vars
    }

    if {[info exists ::starkit::topdir]} {
	# starkit startup
	variable topdir $::starkit::topdir
	variable docroot [file join $topdir docroot]
    } else {
	# unpacked startup
	lappend ::auto_path $home

	# find Wub stuff
	variable topdir [file normalize $wubdir]
	foreach lib {Mime extensions Wub Domains Utilities stx} {
	    lappend ::auto_path [file join $topdir $lib]
	}

	# find docroot
	if {$globaldocroot} {
	    variable docroot [file join $topdir docroot]
	} else {
	    variable docroot [file join $home docroot]
	}
    }

    # uncomment to turn off caching for testing
    # package provide Cache 2.0 ; proc Cache args {return {}}
    foreach package {
	Debug Http Html Cache Listener Block
	File Mason Convert Direct Mime
	Url Query Form Cookies
	Sitemap stx Responder
    } {
	package require $package
    }

    # install default conversions
    Convert init
    Convert Namespace ::MConvert	;# add Mason conversions

    #### initialize Block
    Block init logdir $docroot

    #### Cache init
    Cache init maxsize 204800

    #### Mime init
    Mime::Init -dsname [file join $home ext2mime.tie]

    #### Console init
    if {$cmdport eq ""} {
	package require Stdin
	Stdin start	;# start a command shell on stdin
    } elseif {$cmdport > 0} {
	package require Stdin
	Stdin start $cmdport ;# start a command shell on localhost,$cmdport
    }

    #### Debug init - set some reasonable Debug narrative levels
    Debug on error 100
    Debug on log 10
    Debug on block 10
    
    Debug off socket 10
    Debug off http 2
    Debug off cache 10
    Debug off cookies 10
    Debug off dispatch 10
    Debug off wikit 10
    Debug off direct 10

    # backend may be in this thread. store its config in ::config()
    variable backend
    foreach {n v} $backend {
	set ::config($n) $v
    }

    proc start {args} {
	if {$args ne {}} {
	    variable {*}$args
	}

	variable multi
	if {$multi} {
	    error "This isn't set up for Multithreading"
	    Debug.log {STARTING BACKENDS [clock format [clock seconds]]}

	    package require Backend
	    variable backends
	    set Backend::incr $backends	;# reduce the backend thread quantum for faster testing

	    variable backend
	    Backend configure {*}$backend mkmutex [thread::mutex create]
	    
	    package require HttpdThread	;# choose multithreaded
	} else {
	    package require HttpdSingle	;# choose singlethreaded
	}

	#### start Httpd protocol
	variable httpd
	Httpd configure server_id "Wub [package present Httpd]" {*}$httpd

	variable server_port
	if {[info exists server_port]} {
	    # the listener and server ports differ
	    Httpd configure server_port $server_port
	}

	variable host

	#### start Listener
	variable listener
	if {[dict exists $listener -port] && ([dict get $listener -port] > 0)} {
	    Listener listen -host $host -httpd Httpd {*}$listener
	}

	#### start scgi Listener
	variable scgi
	if {[dict exists $scgi -port] && ([dict get $scgi -port] > 0)} {
	    package require scgi
	    Debug on scgi 10
	    Listener listen -host $host -httpd scgi {*}$scgi
	}

	#### Load local semantics from ./local.tcl
	variable local
	if {$local ne "" && [file exists $local]} {
	    catch {source $local} r eo
	    Debug.log {Site LOCAL: '$r' ($eo)}
	}

	variable done 0
	while {!$done} {
	    vwait done
	}

	Debug.log {Shutdown top level}
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

# Disconnected - courtesy indication that we've been disconnected
proc Disconnected {args} {
    # we're pretty well stateless
}

# Responder::post - postprocess response by converting
proc Responder::post {rsp} {
    return [::Convert do $rsp]
}

# this will be used to send responses to processed requests
# since we're in a single thread, it's got to be fairly simple
proc ::Send {r} {
    if {[dict exists $r -send]} {
	{*}[dict get $r -send] $r
    } else {
	HttpdWorker Send $r
    }
}
