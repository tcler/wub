# Human - try to detect robots by cookie behaviour
if {[catch {package require Debug}]} {
    proc Debug.chan {args} {puts stderr [uplevel subst [list $args]]}
} else {
    package require Debug
    Debug off human 10
}

package require Cookies
package require fileutil

package provide Human 1.0
catch {rename Human {}}	;# remove Human placeholder

set ::API(Server/Human) {
    {
	Attempts to distinguish browsers from bots on the questionable premise that bots never return cookies.  Hmmm.
    }
}

namespace eval Human {
    proc track {r} {
	variable cookie
	variable tracker
	variable logdir
	set ipaddr [dict get $r -ipaddr]

	# try to find the human cookie
	set cdict [dict get $r -cookies]
	set cl [Cookies match $cdict -name $cookie]
	if {[llength $cl]} {
	    # we know they're human - they return cookies (?)
	    set human [dict get [Cookies fetch $cdict -name $cookie] -value]
	    set human [lindex [split $human =] end]

	    # record human's ip addresses
	    if {[info exists tracker($human)]} {
		if {[lsearch -exact $tracker($human) $ipaddr] < 0} {
		    lappend tracker($human) $ipaddr	;# only add new ipaddrs
		    ::fileutil::appendToFile [file join $logdir human] "$human [list $tracker($human)]\n"
		}
	    } else {
		set tracker($human) $ipaddr
		::fileutil::appendToFile [file join $logdir human] "$human [list $ipaddr]\n"
	    }

	    if {[info exists tracker($ipaddr)] && [lindex $tracker($ipaddr) 0]} {
		if {[lsearch -exact $tracker($ipaddr) $human] < 0} {
		    lappend tracker($ipaddr) $human	;# only add new ipaddrs
		    ::fileutil::appendToFile [file join $logdir human] "$ipaddr [list $tracker($ipaddr)]\n"
		}
	    } else {
		set tracker($ipaddr) $human
		::fileutil::appendToFile [file join $logdir human] "$ipaddr [list $tracker($ipaddr)]\n"
	    }

	    dict set r -human $human		;# record supposition that they're human
	    dict set r -ua_class browser	;# classify the agent

	    return $r
	}

	# track the cookie-behaviour of our IP address
	if {[info exists tracker($ipaddr)]} {
	    # we've seen them, and they haven't returned the cookie robot?
	    switch -- [dict get? $r -ua_class] {
		browser {
		    # known to be a browser
		}
		default {
		    dict set r -ua_class robot
		}
	    }
	} else {
	    set tracker($ipaddr) 0	;# remember that we've seen them once
	    ::fileutil::appendToFile [file join $logdir human] "$ipaddr 0\n"
	}

	# add a cookie to reply
	if {[dict exists $r -cookies]} {
	    set cdict [dict get $r -cookies]
	} else {
	    set cdict [dict create]
	}
	set dom [dict get $r -host]	;# the domain on which the request arrived

	# include an optional expiry age
	variable expires
	if {$expires ne ""} {
	    if {[string is integer -strict $expires]} {
		# it's an age
		if {$expires != 0} {
		    set expiresC [Http Date [expr {[clock seconds] + $expires}]]
		    set expiresC [list -expires $expires]
		} else {
		    set expiresC {}
		}
	    } else {
		set expiresC [Http Date [clock scan $expires]]
		set expiresC [list -expires $expires]
	    }
	} else {
	    set expiresC {}
	}

	# add the human cookie
	variable path
	set value [clock microseconds]
	Debug.human {created human cookie $value}
	set cdict [Cookies add $cdict -path $path -name $cookie -value $value {*}$expiresC]

	dict set r -cookies $cdict
	return $r
    }

    variable tracker	;# array of ip->human human->ip
    variable path /	;# which url paths are to be detected/protected?
    variable cookie who	;# name of the cookie to plant
    variable expires ""	;# how long to leave the cookie in.
    variable logdir ""

    proc create {args} {
	error "Can't create a named Human domain - must be anonymous"
    }

    proc new {args} {
	variable tracker
	variable logdir
	variable {*}$args
	if {![info exists tracker]} {
	    # load in the human db
	    catch {
		set fn [file join $logdir human]
		array set tracker [fileutil::cat $fn]
		::fileutil::writeFile $fn [array get tracker]	;# compress back out
	    }
	}
	return ::Human
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
