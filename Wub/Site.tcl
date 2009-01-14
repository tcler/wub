# Site - simple configuration for single-threaded Wub Server.

package require fileutil

package provide Site 1.0

namespace eval Site {
    # return all the configuration state of Site in a handy form
    proc vars {args} {
	set vars {}
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
	set result {}
	foreach line [split $text \n] {
	    set line [string trim $line]
	    if {$line eq ""} continue
	    lassign [split $line {\#;}] line
	    append accum " " [string trim $line]
	    if {[info complete $accum]} {
		set pass [uplevel 1 list $accum]
		lappend result {*}$pass
		set accum ""
	    }
	}
	return $result
    }

    variable configuration {
	home [file normalize [file dirname [info script]]] ;# home of application script
	host [info hostname]	;# default home for relative paths

	globaldocroot 1		;# do we use Wub's docroot, or caller's
	backends 5		;# number of backends to add on demand
	cmdport 8082		;# Console listening socket

	application ""		;# package to require as application

	wubdir [file normalize [file join [file dirname [info script]] ..]] ;# where's wub
	scriptdir [file normalize [file dirname [info script]]] ;# scripts for Backend
	local [file normalize [file join [list $home] local.tcl]] ;# post-init localism
	vars [file normalize [file join [list $home] vars.tcl]] ;# pre-init localism
	# topdir	;# Where to look for Wub libs - don't change
	# docroot	;# Where to look for document root.

	stx_scripting 0	;# permit stx scripting?

	# HTTP Listener configuration
	listener [rc {
	    -port 8080	;# Wub listener port
	    #-host	;# listening host (default [info hostname]
	    #-http	;# dispatch handler (default Http)
	}]

	# HTTPS Listener configuration
	https [rc {
	    -port 8081	;# Wub listener port
	    #-host	;# listening host (default [info hostname]
	    #-http	;# dispatch handler (default Http)
	    -tls {}
	}]

	# SCGI Listener configuration
	scgi [rc {
	    -port 8088			;# what port does SCGI run on
	    -port 0			;# disable SCGI - comment to enable
	    -scgi_send {::scgi Send}	;# how does SCGI communicate incoming?
	}]
	
	# Backend configuration
	backend [rc {
	    scriptname Worker.tcl
	    dispatch Backend
	    # scriptdir ;# directory to find scripts
	    # script ""	;# script to prime backend
	    # max 257	;# maximum number of backend threads
	    # incr 20	;# number of threads to add on exhaustion
	}]

	# Varnish configuration
	varnish [rc { ;# don't use varnish cache by default
	    # vaddress localhost	;# where is varnish running?
	    # vport 6082		;# on what port is varnish control?
	}]

	# Internal Cach configuration
	cache [rc { ;# use in-RAM cache by default
	    maxsize 204800	;# maximum size of object to cache
	    # high 100	;# high water mark for cache
	    # low 90	;# low water mark for cache
	    # weight_age 0.02	;# age weight for replacement
	    # weight_hits -2.0	;# hits weight for replacement
	    # CC 0	;# do we bother to parse cache-control?
	    # obey_CC 0	;# do we act on cache-control? (Not Implemented)
	}]

	# Httpd protocol engine configuration
	httpd [rc {
	    max 1		;# max # of worker threads
	    incr 1		;# number of threads to add on exhaustion
	    over 40		;# degree of thread overcommittment

	    # logfile ""	;# log filename for common log format loggin
	    # max_conn 10	;# max connections per IP
	    # no_really 3	;# how many times to complain about max_conn
	    # server_port	;# server's port, if different from Listener's
	    # server_id		;# server ID to client (default "Wub")
	    # retry_wait	;# 
	}]
    }

    proc Variable {name value} {
	variable $name
	if {![info exists $name]} {
	    set $name $value
	}
    }

    #### Configuration
    # set some default configuration flags and values
    foreach {name val} [rc $configuration] {
	Variable $name $val
    }
    unset configuration

    # load site configuration script (not under SVN control)
    variable vars
    if {$vars ne ""} {
	if {[catch {
	    set x [::fileutil::cat [file join $home $vars]] 
	    eval $x
	} e eo]} {
	    puts stderr "ERROR reading '$vars' config file: '$e' ($eo) - config is incomplete."
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
	foreach lib {Mime extensions stx Wub Domains Utilities} {
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
	Url Query Form Cookies CGI
	Sitemap stx stx2html
    } {
	package require $package
    }

    # install default conversions
    Convert init

    #### Debug init - set some reasonable Debug narrative levels
    Debug on error 100
    Debug on log 10
    Debug on block 10

    Debug off timer 10
    Debug off socket 10
    Debug off http 2
    Debug off cache 10
    Debug off cookies 10
    Debug off direct 10
    Debug off query 10
    Debug off direct 10
    Debug off convert 10
    Debug off cookies 10
    Debug off scgi 10

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

	#### stx init
	variable stx_scripting
	stx2html init script $stx_scripting

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

	array set ::config [vars]

	variable application
	if {[info exists application] && $application ne ""} {
	    package require {*}$application
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
	if {[dict exists $listener -port]
	    && ([dict get $listener -port] > 0)
	} {
	    Listener listen -host $host -httpd Httpd {*}$listener

	    Debug.log {Listening on http://$host:[dict get $listener -port]/ using docroot $docroot}
	}

	#### start HTTPS Listener
	variable https
	if {[dict exists $https -port]
	    && ([dict get $https -port] > 0)
	    && ![catch {
		package require tls
	    }]
	} {
	    #### Simplistic Certificate Authority
	    #package require CA
	    #CA init dir $home/CA host $host port [dict get $https -port]
	    #dict lappend https -tls -cafile [CA cafile] -certfile [CA certificate $host] 
	    Listener listen -host $host -httpd Httpd {*}$https
	    Debug.log {Listening on https://$host:[dict get $https -port]/ using docroot $docroot}
	}

	#### start scgi Listener
	variable scgi
	if {[dict exists $scgi -port] && ([dict get $scgi -port] > 0)} {
	    package require scgi
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
