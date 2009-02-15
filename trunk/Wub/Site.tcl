# Site - simple configuration for single-threaded Wub Server.

package require fileutil
package require inifile

proc findpaths {} {
    foreach el $::auto_path {
	dict set apath [file normalize $el] {}
    }
    set nousrlib [catch {dict unset apath /usr/lib}]

    if {![info exists ::starkit::topdir]} {
	# unpacked startup
	set home [file normalize [file dirname [info script]]]
	dict set apath $home {}

	# find Wub stuff
	set top [file dirname $home]
	foreach lib {Mime extensions stx Wub Domains Utilities} {
	    dict set apath [file join $top $lib] {}
	}
    } else {
	# starkits handle the auto_path for us
	# but do they handle the home for us?
    }

    if {!$nousrlib} {
	dict set apath /usr/lib {}	;# put the fallback libdir at the end
    }

    set ::auto_path [dict keys $apath]

    puts stderr "AUTOPATH: $::auto_path"
}
findpaths

foreach package {
    Httpd Debug Http Html Listener
    Url Query Form Cookies CGI
    File Mason Convert Direct
    Block Mime
    Sitemap stx stx2html
} {
    package require $package
}
package provide Site 1.0

namespace eval Site {
    # record wub's home
    set home [file normalize [file dirname [info script]]]

    # uncomment to turn off caching for testing
    # package provide Cache 2.0 ; proc Cache args {return {}}

    # return all the configuration state of Site in a handy form
    proc vars {args} {
	set vars {}
	foreach var [info vars ::Site::*] {
	    if {[info exists $var]} {
		set svar [namespace tail $var] 
		catch {lappend vars $svar [set $var]}
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

    proc Variable {name value} {
	variable $name
	if {![info exists $name]} {
	    #puts stderr "Variable: $name $value"
	    set $name $value
	}
	uplevel variable $name
    }

    proc do_ini {file} {
	variable modules
	#puts stderr "INI file: $file [file exists $file]"
	if {![file exists $file]} return
	set ini [::ini::open $file]
	foreach sect [::ini::sections $ini] {
	    set cs [string tolower $sect]
	    set modules($cs) {}
	    foreach key [::ini::keys $ini $sect] {
		set v [::ini::value $ini $sect $key]
		if {$cs eq "wub"} {
		    #puts stderr "INI: ::Site::$sect $key $v"
		    set ::Site::$key $v
		} else {
		    #puts stderr "INI: ::Site::$sect $key $v"
		    dict set ::Site::$sect $key $v
		}
	    }
	}
	::ini::close $ini
    }

    proc write_ini {file} {
	variable modules
	set ini [::ini::open $file w]
	foreach var [info vars ::Site::*] {
	    if {[catch {
		set val [set $var]
		set vvar [namespace tail $var]
	    }]} continue

	    if {[info exists modules($vvar)]} {
		# it's a module
		dict for {k v} $val {
		    ::ini::set $ini $vvar $k $v
		}
	    } else {
		# it's in wub
		catch {
		    if {$val ne ""} {
			::ini::set $ini wub $vvar $val
		    }
		}
	    }
	}
	::ini::commit $ini
	::ini::close $ini
    }

    variable wubdir [file normalize [file join [file dirname [info script]] ..]] ;# where's wub
    variable configuration {
	home [file normalize [file dirname [info script]]] ;# home of application script
	host [info hostname]	;# default home for relative paths
	ini site.ini		;# init files
	globaldocroot 0		;# do we use Wub's docroot, or caller's
	cmdport 8082		;# Console listening socket

	application ""		;# package to require as application

	local [file normalize [file join [list $home] local.tcl]] ;# post-init localism
	vars [file normalize [file join [list $home] vars.tcl]] ;# pre-init localism
	# topdir	;# Where to look for Wub libs - don't change
	# docroot	;# Where to look for document root.

	stx_scripting 0	;# permit stx scripting?

	# HTTP Listener configuration
	@listener [rc {
	    -port 8080	;# Wub listener port
	    #-host	;# listening host (default [info hostname]
	    #-http	;# dispatch handler (default Http)
	}]

	# HTTPS Listener configuration
	@https [rc {
	    -port 8081	;# Wub listener port
	    #-host	;# listening host (default [info hostname]
	    #-http	;# dispatch handler (default Http)
	    -tls {}
	}]

	# SCGI Listener configuration
	@scgi [rc {
	    -port 8088			;# what port does SCGI run on
	    -port 0			;# disable SCGI - comment to enable
	    -scgi_send {::scgi Send}	;# how does SCGI communicate incoming?
	}]

	# Varnish configuration
	@varnish [rc { ;# don't use varnish cache by default
	    # vaddress localhost	;# where is varnish running?
	    # vport 6082		;# on what port is varnish control?
	}]

	# Internal Cach configuration
	@cache [rc { ;# use in-RAM cache by default
	    maxsize 204800	;# maximum size of object to cache
	    high 100		;# high water mark for cache
	    low 90		;# low water mark for cache
	    weight_age 0.02	;# age weight for replacement
	    weight_hits -2.0	;# hits weight for replacement
	    # CC 0	;# do we bother to parse cache-control?
	    # obey_CC 0	;# do we act on cache-control? (Not Implemented)
	}]

	@nub [rc {
	    nubdir [file join $home nubs]
	    nubs {nub.nub bogus.nub}
	}]

	# Httpd protocol engine configuration
	@httpd [rc {
	    logfile "wub.log"	;# log filename for common log format logging
	    max_conn 20		;# max connections per IP
	    no_really 30	;# how many times to complain about max_conn
	    # server_port	;# server's port, if different from Listener's
	    # server_id		;# server ID to client (default "Wub")
	    retry_wait	20	;# how long to advise client to wait on exhaustion
	    timeout 60000	;# ms of idle to tolerate
	}]
	password ""		;# account (and general) root password
    }

    proc init {args} {
	# can pass in 'configuration' dict like above
	variable configuration
	if {[dict exists $args configuration]} {
	    set configuration [dict merge $configuration [dict get $args configuration]]
	    dict unset args configuration
	}

	# args to Site::init become initial variable values
	if {$args ne {}} {
	    variable {*}$args
	}

	# configuration variable contains defaults
	# set some default configuration flags and values
	variable modules
	variable configuration
	foreach {name val} [namespace eval ::Site [list rc $configuration]] {
	    if {[string match @* $name]} {
		set name [string tolower [string trim $name @]]
		set modules($name) {}
	    }
	    Variable $name $val
	}
	unset configuration

	variable home	;# application's home

	# load ini files from app's home
	variable ini
	foreach i $ini {
	    if {[file pathtype $i] eq "relative"} {
		set i [file join [file normalize $home] $i] 
	    }
	    do_ini $i
	}

	# load site configuration script vars.tcl (not under SVN control)
	variable vars
	if {$vars ne ""} {
	    if {[file exists [file join $home $vars]] && [catch {
		set x [::fileutil::cat [file join $home $vars]] 
		eval $x
		unset x
	    } e eo]} {
		puts stderr "ERROR reading '$vars' config file: '$e' ($eo) - config is incomplete."
	    }
	}

	# command-line configuration of vars
	foreach {name val} $::argv {
	    variable $name $val	;# set global config vars
	}
	unset name; unset val

	# we can write all this stuff back out, if desired
	if {[info exists ::Site::write_ini]} {
	    unset e; unset eo
	    write_ini [file normalize $::Site::write_ini]
	}

	variable host; variable listener
	Variable url "http://$host:[dict get $listener -port]/"


	# now we're configured set some derived values
	if {[info exists ::starkit::topdir]} {
	    # starkit startup
	    Variable topdir $::starkit::topdir
	    Variable docroot [file join $topdir docroot]
	} else {
	    # unpacked startup
	    lappend ::auto_path $home	;# add the app's home dir to auto_path
	    
	    # find Wub stuff
	    variable wubdir; variable topdir
	    Variable topdir [file normalize $wubdir]

	    #foreach lib {Mime extensions stx Wub Domains Utilities} {
	    #lappend ::auto_path [file join $topdir $lib]
	    #}
	    
	    # find docroot
	    if {$globaldocroot} {
		Variable docroot [file join $topdir docroot]
	    } else {
		Variable docroot [file join $home docroot]
	    }
	}

	# install default conversions
	Convert new

	proc init {args} {}	;# ensure init can't be called twice
    }

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

    proc start {args} {
	init {*}$args
	variable docroot

	#### initialize Block
	Block new logdir $docroot

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
	    Cache new {*}$cache
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

	variable application
	if {[info exists application] && $application ne ""} {
	    package require $application
	    
	    # install variables defined by local, argv, etc
	    variable modules
	    if {[info exists modules([string tolower $application])]} {
		variable [string tolower $application]
		namespace eval $application [list variable {*}[set [string tolower $application]]]
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

	package require Nub
	variable nub
	Nub init {*}$nub
	#puts stderr "NUB:$nub"
	if {[dict exists $nub nubs] && [llength [dict get $nub nubs]]} {
	    foreach file [dict get $nub nubs] {
		Nub configF $file
	    }
	} else {
	    # no nubs supplied
	    Nub config	;# start with the builtin
	}
	Nub apply	;# now apply them

	variable done 0
	while {!$done} {
	    vwait done
	}

	Debug.log {Shutdown top level}
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
