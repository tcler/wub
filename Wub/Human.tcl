# Human - try to detect robots by cookie behaviour

package require Debug
Debug define human 10

package require Cookies
package require fileutil
package require Store	;# we use tdbc to store Human db

package provide Human 2.0

set ::API(Server/Human) {
    {
	Attempts to distinguish browsers from bots on the questionable premise that bots never return cookies.  Hmmm.
    }
    path {which url paths are to be detected/protected?  (default /)}
    cookie {name of the cookie to plant (default "human")}
    expires {how long to leave the cookie in. (default "next year")}
    logdir {which directory to write the human logfile into (default [pwd])}
}

oo::class create ::HumanC {
    method ip {r} {
	set ip 0
	foreach octet [split [dict r.-ipaddr] .] {
	    set octet [string trimleft $octet 0]
	    if {$octet eq ""} {
		set octet 0
	    }
	    set ip [expr {($ip * 256)+$octet}]
	}
	Debug.human {ip: $ip}
	return $ip
    }

    method getcookie {r} {
	variable cookie
	# try to find the application cookie
	set cl [Cookies Match $r -name $cookie]
	if {[llength $cl]} {
	    return [dict get [Cookies Fetch $r -name $cookie] -value]
	} else {
	    return ""
	}
    }

    method newhuman {r {value ""}} {
	if {$value eq ""} {
	    # create a new cookie
	    variable uniq; incr uniq
	    set value [::md5::md5 -hex $uniq[clock microseconds]]
	}

	# add a cookie to reply
	if {[dict exists $r -cookies]} {
	    set cdict [dict get $r -cookies]
	} else {
	    set cdict [dict create]
	}

	# include an optional expiry age
	variable expires
	if {[info exists expires]
	    && $expires ne ""
	} {
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

	# include optional -secure
	variable secure
	if {$secure} {
	    set S -secure
	} else {
	    set S {}
	}

	# add the cookie
	variable cookie; variable path
	set cdict [Cookies add $cdict -path $path -name $cookie -value $value {*}$expiresC {*}$S]
	Debug.human {created human cookie '$cookie' in ($cdict)}
	dict set r -cookies $cdict

	# add a human record
	my append human $value created [clock milliseconds] ip [my ip $r]]
	return $r
    }

    # robot - declare this ip to be a robot
    method robot {r ipaddr} {
	switch -- [dict get? $r -ua_class] {
	    browser {
		# known to be a browser
	    }
	    default {
		# found to be a robot
		my append robots ip $ipaddr
		dict set r -ua_class robot
	    }
	}
	return $r
    }

    # no cookie was returned
    # - investigate further before possibly declaring IP robotic
    method nocookie {r ipaddr} {
	set iprecords [my match ips ip $ipaddr]
	if {![llength $iprecords]} {
	    # we have never seen this IP address before
	    # - send it a cookie and see how it responds
	    Debug.human {never seen $ipaddr before}
	    append ips ip $ipaddr last [clock milliseconds]
	    dict set r -ua_class robot
	    return [my newhuman $r]
	} else {
	    # we have seen this IP address before
	    # - time to make a judgement

	    # calculate some stats on this IP
	    set humans {}
	    set recent {}
	    set bytime {}
	    set robotic {}
	    set now [clock milliseconds]
	    foreach visit $iprecords {
		set when [expr {$now - [dict visit.last]}]
		if {[dict exists $visit human]} {
		    set h [dict visit.human]
		    dict incr humans $h	;# how many times as human?
		    lappend bytime $when $h	;# all visits by time

		    # record most recent connections by human
		    if {[dict exists recent $h]} {
			if {[dict recent.$h] < [dict visit.last]} {
			    dict recent.$h [dict visit.last]
			}
		    } else {
			dict latest.$h [dict visit.last]
		    }
		} else {
		    # record robotic visits
		    lappend robotic $when	;# robotic vists by time
		    lappend bytime $when robotic
		}
	    }

	    Debug.log {Human: seen $ipaddr [llength $iprecords] times as human: ($humans) most recently ($recent) - [llength $robotic] times as a robot most recently [lindex [lsort -integer $robotic] 0] (bytime: $bytime)}

	    if {[llength $robotic] < 3} {
		# give it 2 goes to identify itself
		dict set r -ua_class browser	;# tentatively
		return [my newhuman $r]
	    }

	    return [my robot $r $ipaddr]
	}
    }

    method track {r} {
	variable cookie
	variable logdir
	variable path

	# only track cookies on given path
	if {![string match ${path}* [dict get $r -path]]} {
	    return $r
	}
	
	set human [my getcookie $r]		;# get human cookie
	set ipaddr [my ip $r]	;# and IP address

	if {$human ne ""} {
	    # we know they're human - they return cookies (?)
	    my delete robots ip $ipaddr	;# a reprieve - they returned a cookie

	    # record human's ip addresses and last connection time
	    set record [my fetch human $human]
	    if {[dict size $record]} {
		set iprecord [my fetch ips human $human ip $ipaddr]
		if {[dict size $iprecord]} {
		    # record human as connecting from this ip
		    set id iprecord.id
		    my incr iprecord.$id count
		    my set iprecord.$id last [clock milliseconds]
		} else {
		    # We have seen this human before,
		    # just not from this ip before
		    my append ips human $human ip $ipaddr count 1 last [clock milliseconds]
		}
		dict set r -human $human	;# suppose they're human
	    } else {
		# the returned cookie is unknown
		# - we should resend our cookie, and wait
		set r [my newhuman $r]
	    }
	    dict set r -ua_class browser	;# classify the agent
	} else {
	    # discover known robots fast
	    set iprecord [my fetch robots ip $ipaddr]
	    if {[dict size $iprecord]} {
		Debug.human {$ipaddr is a known robot}
		dict set r -ua_class robot
		return $r
	    }
	    return [my nocookie $r $ipaddr]
	}
    }

    superclass Store
    constructor {args} {
	variable path /	;# which url paths are to be detected/protected?
	variable cookie human	;# name of the cookie to plant
	variable expires "next year"	;# how long to leave the cookie in.
	variable logdir [file join [Site var? Wub topdir] data]
	variable secure 0
	variable debug 0
	variable {*}$args
	if {$debug} {
	    Debug on human
	    Debug on store
	}
	Debug.human {creating $args}

	next file [file join $logdir human.db] primary human schema {
	    PRAGMA foreign_keys = on;
	    CREATE TABLE human
	    (
	     id INTEGER PRIMARY KEY AUTOINCREMENT,
	     human TEXT,
	     created INTEGER,
	     ip TEXT
	     );
	    CREATE TABLE ips
	    (
	     ip INTEGER,
	     count INTEGER,
	     last INTEGER,
	     human TEXT
	     );
	    CREATE TABLE robots
	    (
	     ip INTEGER PRIMARY KEY
	     );
	}
	Debug.human {tables: [my db tables]}
    }
}
