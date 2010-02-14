# Site - simple configuration for single-threaded Wub Server.
package require Tcl 8.6	;# minimum version of tcl required

namespace eval ::Site {}

# keep track of sourced files
if {$::tcl_platform(os) eq "Linux"} {
    rename source source_org
    proc ::source {args} {
	set fn [lindex $args end]
	if {[lindex [file split $fn] end] ne "pkgIndex.tcl"} {
	    set f [file normalize [lindex $args end]]
	    dict set ::Site::sourced [list source $f] $args
	    puts stderr "source $f"
	}
	return [uplevel source_org {*}$args]
    }

    rename load load_org
    proc ::load {args} {
	set f [file normalize [lindex $args 0]]
	dict set ::Site::sourced [list load $f] $args
	puts stderr "load $f"
	return [uplevel load_org {*}$args]
    }
}

# this will make some necessary changes to auto_path so we find Wub
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
	foreach lib {extensions Wub Domains Utilities Client} {
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

package require Debug
package require Dict

Debug on site 10
package provide Site 1.0

namespace eval ::Site {
    variable sourced [list [info script] [info script]]

    # record wub's home
    variable home [file normalize [file dirname [info script]]]
    variable wubroot $home
    variable wubtop [file dirname $home]

    # uncomment to turn off caching for testing
    # package provide Cache 2.0 ; proc Cache args {return {}}

    # return a specific module Site var
    proc var {module args} {
	return [dict get $::Site::$module {*}$args]
    }

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
	    Debug.site {Variable $name: $value}
	    set $name $value
	}
	uplevel variable $name
    }

    # read the site ini file
    proc do_ini {file} {
	variable modules
	Debug.site {INI file: $file [file exists $file]}
	if {![file exists $file]} return
	package require inifile
	set ini [::ini::open $file r]

	foreach sect [::ini::sections $ini] {
	    set cs [string tolower $sect]	;# section name
	    set modules($cs) {}			;# record the elements
	    foreach key [::ini::keys $ini $sect] {
		set v [::ini::value $ini $sect $key]
		if {$cs eq "wub"} {
		    # global config file
		    Debug.site {INI global: $key $v}
		    set ::Site::$key $v
		} else {
		    Debug.site {INI module $cs: $key $v}
		    dict set ::Site::$cs $key $v
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
	globaldocroot 1		;# do we use Wub's docroot, or caller's

	@shell [rc {
	    load 0		;# want Console
	    port 8082		;# Console listening socket
	}]

	application ""		;# package to require as application

	local local.tcl	;# post-init localism
	vars vars.tcl	;# pre-init localism
	# topdir	;# Where to look for Wub libs - don't change
	# docroot	;# Where to look for document root.

	@stx [rc {
	    load 1	;# want STX by default
	    scripting 0	;# permit stx scripting?
	}]

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
	    load 0			;# don't want varnish
	    # vaddress localhost	;# where is varnish running?
	    # vport 6082		;# on what port is varnish control?
	}]

	@block [rc {
	    load 1		;# want block by default
	}]

	@human [rc {
	    load 1		;# want human by default
	}]

	@ua [rc {
	    load 1		;# want user agent classification by default
	}]

	@convert [rc {		;# cant content negotiation by default
	    load 1
	}]

	# Internal Cach configuration
	@cache [rc { ;# use in-RAM cache by default
	    load 1		;# want cache, by default
	    maxsize 204800	;# maximum size of object to cache
	    high 100		;# high water mark for cache
	    low 90		;# low water mark for cache
	    weight_age 0.02	;# age weight for replacement
	    weight_hits -2.0	;# hits weight for replacement
	    # CC 0	;# do we bother to parse cache-control?
	    # obey_CC 0	;# do we act on cache-control? (Not Implemented)
	}]

	@nub [rc {
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
	if {[dict exists $args debug]} {
	    set debug [dict get $args debug]
	    Debug on site $debug
	    dict unset args debug
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
	if {[dict exists $::argv home]} {
	    # set this most important of variables.
	    # It's the app's home, other stuff is made relative to it.
	    set home [dict get $::argv home]
	}

	# load ini files from app's home
	variable ini
	foreach i $ini {
	    do_ini $i
	}

	# load site configuration script vars.tcl (not under SVN control)
	variable vars
	if {$vars ne ""} {
	    if {[file exists $vars] && [catch {
		set fd [open $vars r]; set x [read $fd]; close $fd
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

	proc init {args} {}	;# ensure init can't be called twice
    }

    #### Debug init - set some reasonable Debug narrative levels
    Debug on error 100
    Debug on log 10
    Debug on block 10

    #### Load those modules needed for the server to run
    proc modules {} {
	variable docroot
	package require Httpd

	#### Load Convert module - content negotiation
	variable convert
	if {[info exists convert]
	    && [dict get? $convert load] ne ""
	    && [dict get? $convert load]
	} {
	    # install default conversions
	    package require Convert
	    Convert new {*}$convert
	    Debug.site {Module Convert: YES}
	} else {
	    Debug.site {Module Convert: NO}
	}

	#### Load Block module - blocks incoming by ipaddress
	variable block
	if {[info exists block]
	    && [dict get? $block load] ne ""
	    && [dict get? $block load]
	} {
	    #### initialize Block
	    Debug.site {Module Block: YES}
	    package require Block
	    Block new logdir $docroot {*}$block
	} else {
	    # NULL Block
	    Debug.site {Module Block: NO}
	    namespace eval ::Block {
		proc block {args} {}
		proc blocked? {args} {return 0}
		proc new {args} {}
		namespace export -clear *
		namespace ensemble create -subcommands {}
	    }
	}

	#### Load Human Module - redirects bad bots
	variable human
	if {[info exists human]
	    && [dict get? $human load] ne ""
	    && [dict get? $human load]
	} {
	    #### initialize Human
	    package require Human
	    Debug.site {Module Human: YES}
	    Human new logdir $docroot {*}$human
	} else {
	    # NULL Human
	    Debug.site {Module Human: NO}
	    namespace eval ::Human {
		proc track {r args} {return $r}
		namespace export -clear *
		namespace ensemble create -subcommands {}
	    }
	}

	#### Load UA Module - classifies by user-agent
	variable ua
	if {[info exists ua]
	    && [dict get? $ua load] ne ""
	    && [dict get? $ua load]
	} {
	    #### initialize UA
	    package require UA
	    Debug.site {Module UA: YES}
	} else {
	    # NULL UA classifier
	    Debug.site {Module UA: NO}
	    namespace eval ::UA {
		proc classify {args} {return browser}
		proc parse {args} {return ""}
		namespace export -clear *
		namespace ensemble create -subcommands {}
	    }
	}

	### Load Varnish Module - a kind of Cache
	variable varnish
	if {[info exists varnish]
	    && [dict get? $varnish load] ne ""
	    && [dict get? $varnish load]
	} {
	    #### Varnish cache
	    package require Varnish
	    if {![catch {
		Varnish init {*}$varnish
		Debug.site {Module Varnish: YES}
	    } r eo]} {
		Debug.error {varnish: $r ($eo)}
		package forget Varnish
		catch {unset cache}
	    }
	} else {
	    Debug.site {Module Varnish: NO}
	}

	#### Load Cache Module - server caching
	variable cache
	if {[info exists cache]
	    && [dict get? $cache load] ne ""
	    && [dict get? $cache load]
	} {
	    #### in-RAM Cache
	    package require Cache 
	    Cache new {*}$cache
	    Debug.site {Module Cache: YES}
	} else {
	    #### Null Cache
	    package provide Cache 2.0
	    proc Cache args {return {}}
	    Debug.site {Module Cache: NO}
	}

	#### Load STX Module - rich text conversion
	variable stx
	if {[info exists stx]
	    && [dict get? $stx load] ne ""
	    && [dict get? $stx load]
	} {
	    #### stx init
	    package require stx
	    package require stx2html

	    variable stx_scripting
	    stx2html init script [dict get? $stx scripting] {*}$stx
	    Debug.site {Module STX: YES}
	} else {
	    Debug.site {Module STX: NO}
	}

	#### Console init
	variable shell
	if {[info exists shell]
	    && [dict get? $shell load] ne ""
	    && [dict get? $shell load]
	} {
	    if {[catch {
		#### Shell init
		package require Shell
		Shell new {*}$shell
	    } err eo]} {
		Debug.error {Module Shell: Failed to Init. $err ($eo)}
	    }
	} else {
	    Debug.site {Module Shell: NO}
	}

	#### Load up nubs
	package require Nub
	variable nub
	variable nubs
	if {[info exists nubs] && [llength $nubs]} {
	    dict set nub nubs $nubs
	}

	Nub init {*}$nub
	Debug.site {NUB:$nub}
	if {[dict exists $nub nubs] && [llength [dict get $nub nubs]]} {
	    foreach file [dict get $nub nubs] {
		Nub configF $file
	    }
	} else {
	    # no nubs supplied
	    Nub config	;# start with the builtin
	}

	#### Load local semantics from ./local.tcl
	variable local
	variable home
	if {[info exists local] && $local ne ""} {
	    if {[file exists $local]} {
		if {[catch {source $local} r eo]} {
		    Debug.error {Site LOCAL ($local) error: '$r' ($eo)}
		}
	    }
	}

	# apply all collected Nubs
	Nub apply

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

	#### start Listeners
	variable listener
	if {![dict exists $listener -port]} {
	    dict set listener -port 80
	}
	set h {}
	if {[dict exists $listener -host]} {
	    set h [list -host [dict get $listener -host]]
	}
	Listener new {*}$h -httpd Httpd {*}$listener

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
	    Listener new {*}$h -httpd Httpd {*}$https
	}

	#### start scgi Listener
	variable scgi
	if {[dict exists $scgi -port] && ([dict get $scgi -port] > 0)} {
	    package require scgi
	    Listener listen -host $host -httpd scgi {*}$scgi
	    Debug.log {Listening on scgi $host [dict get $scgi -port] using docroot $docroot}
	}

    }

    # this will shut down the whole system
    proc shutdown {{reason "No Reason"}} {
	variable done 1
    }

    # Load the application, on first call also starts the server
    proc start {args} {
	Debug.site {start: $args}
	init {*}$args
	modules	;# start the listeners etc

	# can't run the whole start up sequence twice
	# can initialize the application
	proc start {args} {
	    #### load the application
	    set application [dict get? $args application]
	    if {$application ne ""} {
		package require $application
		
		# install variables defined by local, argv, etc
		variable modules
		set app [string tolower $application]
		if {[info exists modules([string tolower $app])]} {
		    variable $app
		    Debug.site {starting application $application - [list variable {*}[set $app]]}
		    Debug.site {app ns: [info vars ::${application}::*]}
		    namespace eval ::$application [list variable {*}[set $app]]
		    Debug.site {app ns: [info vars ::${application}::*]}
		} else {
		    Debug.site {not starting application $application, no module in [array names modules]}
		}
	    } else {
		Debug.site {No application specified}
	    }
	}
	if {[info exists application]} {
	    start application $application	;# init the application
	} else {
	    start
	}

	variable done 0
	while {!$done} {
	    # redefine ::vwait so we don't get fooled again
	    rename ::vwait ::Site::vwait
	    proc ::vwait {args} {
		catch {
		    info frame -1
		} frame
		puts stderr "Recursive VWAIT AAAARRRRRRGH! from '$frame'"
	    }
	    Debug.site {entered event loop}
	    ::Site::vwait done
	}

	Debug.log {Shutdown top level}
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
