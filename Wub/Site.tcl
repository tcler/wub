# Site - simple configuration for single-threaded Wub Server.

package require fileutil

package provide Site 1.0

namespace eval Site {
    # return all the configuration state of Site in a handy form
    proc vars {args} {
	set vars {}
	#puts stderr [info vars ::Site::*]
	foreach var [info vars ::Site::*] {
	    if {[info exists $var]} {
		set svar [namespace tail $var] 
		lappend vars $svar [set $var]
	    }
	}
	#puts stderr "VARS: $vars"
	return $vars
    }

    # simple rc reader
    proc rc {text} {
	set accum ""
	foreach line [split $text \n] {
	    set line [string trim $line]
	    if {$line eq ""} continue
	    lassign [split $line {\#;}] line
	    append accum " " [string trim $line]
	}
	set accum [string trim $accum]
	set result [uplevel 1 [list subst $accum]]
	puts stderr "RC: $result"
	return $result
    }

    variable home
    if {![info exists home]} {
	# if another package hasn't set home, then it's here
	set home [file normalize [file dirname [info script]]]
    }

    #### Configuration
    # set some default configuration flags and values
    foreach {name val} [rc {
	host [info hostname]	;# default home for relative paths
	multi 0			;# we're single-threaded

	globaldocroot 1
	backends 5
	cmdport 8082

	application ""

	wubdir [file join [file dirname [info script]] ..]
	scriptdir [file normalize [file dirname [info script]]]
	local [file join $home local.tcl]
	vars [file join $home vars.tcl]

	listener [list [rc {
	    -port 8080
	}]]

	scgi [list [rc {
	    -port 8088
	    -scgi_send {::scgi Send}
	}]]
	
	backend [list [rc {
	    scriptname Worker.tcl
	    dispatch Backend
	}]]

	varnish [list [rc { ;# don't use varnish cache by default
	}]]
	cache [list [rc { ;# use in-RAM cache by default
	    maxsize 204800
	}]]

	httpd [list [rc {
	    max 1
	    incr 1
	    over 40
	    dispatch ""
	}]]
    }] {
	puts "Default: $name '$val'"
	variable $name
	if {![info exists $name]} {
	    set $name $val
	}
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

    variable url "http://$host:[dict get $listener -port]/"

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
	Httpd
	Debug Http Html Listener Block
	File Mason Convert Direct Mime
	Url Query Form Cookies
	Sitemap stx Responder
    } {
	package require $package
    }

    # install default conversions
    Convert init
    Convert Namespace ::MConvert	;# add Mason conversions

    #### Debug init - set some reasonable Debug narrative levels
    Debug on error 100
    Debug on log 10
    Debug on block 10

    Debug off socket 10
    Debug off http 2
    Debug off cache 10
    Debug off cookies 10
    Debug off direct 10
    Debug off dispatch 10
    Debug off query 10
    Debug off direct 10
    Debug off convert 10
    Debug off cookies 10

    # backend may be in this thread. store its config in ::config()
    variable backend
    foreach {n v} $backend {
	set ::config($n) $v
    }

    proc start {args} {
	if {$args ne {}} {
	    variable {*}$args
	}
	variable docroot

	#### initialize Block
	Block init logdir $docroot

	variable varnish

	if {[info exists varnish] && ($varnish ne {})} {
	    #### Varnish cache
	    package require Varnish
	    if {![catch {
		Varnish init {*}$varnish
	    } r eo]} {
		Debug.error {varnish: $r ($eo)}
		package forget Varnish
		catch {unset cache}
	    }
	}

	variable cache
	if {[info exists cache] && ($cache ne {})} {
	    #### in-RAM Cache
	    package require Cache 
	    Cache init {*}$cache
	} else {
	    #### Null Cache
	    package provide Cache 2.0
	    proc Cache args {return {}}
	}

	#### Mime init
	variable home
	Mime::Init -dsname [file join $home ext2mime.tie]

	#### Console init
	variable cmdport
	if {$cmdport eq ""} {
	    package require Stdin
	    Stdin start	;# start a command shell on stdin
	} elseif {$cmdport > 0} {
	    package require Stdin
	    Stdin start $cmdport ;# start a command shell on localhost,$cmdport
	}

	variable multi
	if {$multi} {
	    error "This isn't set up for Multithreading"
	    Debug.log {STARTING BACKENDS [clock format [clock seconds]]}

	    package require Backend
	    variable backends
	    set Backend::incr $backends	;# reduce the backend thread quantum for faster testing

	    variable backend
	    Backend configure [vars] {*}$backend mkmutex [thread::mutex create]

	    package require HttpdThread	;# choose multithreaded
	} else {
	    package require HttpdSingle	;# choose singlethreaded
	    array set ::config [vars]

	    variable application
	    if {[info exists application] && $application ne ""} {
		package require {*}$application
	    }
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
	variable docroot

	#### start Listener
	variable listener
	if {[dict exists $listener -port] && ([dict get $listener -port] > 0)} {
	    Listener listen -host $host -httpd Httpd {*}$listener

	    Debug.log {Listening on http://$host:[dict get $listener -port]/ using docroot $docroot}
	}

	#### start scgi Listener
	variable scgi
	if {[dict exists $scgi -port] && ([dict get $scgi -port] > 0)} {
	    package require scgi
	    Debug on scgi 10
	    Listener listen -host $host -httpd scgi {*}$scgi
	    Debug.log {Listening on scgi $host [dict get $scgi -port] using docroot $docroot}
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
