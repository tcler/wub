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
	return $result
    }

    variable configuration {
	home [file normalize [file dirname [info script]]] ;# home of application script
	host [info hostname]	;# default home for relative paths
	multi 0			;# we're single-threaded

	globaldocroot 1		;# do we use Wub's docroot, or caller's
	backends 5		;# number of backends to add on demand
	cmdport 8082		;# Console listening socket

	application ""		;# package to require as application

	wubdir [file normalize [file join [file dirname [info script]] ..]] ;# where's wub
	scriptdir [file normalize [file dirname [info script]]] ;# scripts for Backend
	local [file normalize [file join $home local.tcl]] ;# post-init localism
	vars [file normalize [file join $home vars.tcl]] ;# pre-init localism
	# topdir	;# Where to look for Wub libs - don't change
	# docroot	;# Where to look for document root.

	# HTTP Listener configuration
	listener [list [rc {
	    -port 8080	;# Wub listener port
	    #-host	;# listening host (default [info hostname]
	    #-http	;# dispatch handler (default Http)
	    #-tls	;# array passed to tls (default none=disabled)
	}]]

	# SCGI Listener configuration
	scgi [list [rc {
	    -port 8088			;# what port does SCGI run on
	    -port 0			;# disable SCGI - comment to enable
	    -scgi_send {::scgi Send}	;# how does SCGI communicate incoming?
	}]]
	
	# Backend configuration
	backend [list [rc {
	    scriptname Worker.tcl
	    dispatch Backend
	    # scriptdir ;# directory to find scripts
	    # script ""	;# script to prime backend
	    # max 257	;# maximum number of backend threads
	    # incr 20	;# number of threads to add on exhaustion
	}]]

	# Varnish configuration
	varnish [list [rc { ;# don't use varnish cache by default
	    # vaddress localhost	;# where is varnish running?
	    # vport 6082		;# on what port is varnish control?
	}]]

	# Internal Cach configuration
	cache [list [rc { ;# use in-RAM cache by default
	    maxsize 204800	;# maximum size of object to cache
	    # high 100	;# high water mark for cache
	    # low 90	;# low water mark for cache
	    # weight_age 0.02	;# age weight for replacement
	    # weight_hits -2.0	;# hits weight for replacement
	    # CC 0	;# do we bother to parse cache-control?
	    # obey_CC 0	;# do we act on cache-control? (Not Implemented)
	}]]

	# Httpd protocol engine configuration
	httpd [list [rc {
	    max 1		;# max # of worker threads
	    incr 1		;# number of threads to add on exhaustion
	    over 40		;# degree of thread overcommittment
	    dispatch ""		;# dispatcher for incoming requests

	    # max_conn 10	;# max connections per IP
	    # no_really 3	;# how many times to complain about max_conn
	    # dispatch "Backend" ;# backend dispatcher
	    # server_port	;# server's port, if different from Listener's
	    # server_id		;# server ID to client (default "Wub")
	    # retry_wait	;# 
	}]]
    }

    proc Variable {name value} {
	variable $name
	if {![info exists $name]} {
	    set $name $val
	}
    }

    #### Configuration
    # set some default configuration flags and values
    foreach {name val} [rc $config] {
	Variable $name $val
    }

    # load site configuration script (not under SVN control)
    variable vars
    if {$vars ne ""} {
	catch {
	    eval [::fileutil::cat [file join $home $vars]]
	}
    }

    # load command-line configuration vars
    foreach {name val} $::argv {
	variable $name
	set $name $val	;# set global config vars
    }

    Variable url "http://$host:[dict get $listener -port]/"

    if {[info exists ::starkit::topdir]} {
	# starkit startup
	Variable topdir $::starkit::topdir
	Variable docroot [file join $topdir docroot]
    } else {
	# unpacked startup
	lappend ::auto_path $home

	# find Wub stuff
	Variable topdir [file normalize $wubdir]
	foreach lib {Mime extensions Wub Domains Utilities stx} {
	    lappend ::auto_path [file join $topdir $lib]
	}

	# find docroot
	if {$globaldocroot} {
	    Variable docroot [file join $topdir docroot]
	} else {
	    Variable docroot [file join $home docroot]
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
	variable home
	if {$local ne ""} {
	    if {[file pathtype $local] eq "relative"} {
		set local [file normalize [file join $home $local]]
	    }
	    if {[file exists $local]} {
		catch {source $local} r eo
		Debug.log {Site LOCAL: '$r' ($eo)}
	    }
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

# Resume a suspended response
proc ::Resume {rsp} {
    catch {dict unset rsp -suspend}
    if {[catch {Responder post $rsp} r eo]} { ;# postprocess response
	set rsp [Http ServerError $rsp $r $eo]
    } else {
	set rsp $r
    }
    ::Send $rsp
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
