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
    method ipaddr {ipaddr} {
	set ip 0
	foreach octet [split $ipaddr .] {
	    set octet [string trimleft $octet 0]
	    if {$octet eq ""} {
		set octet 0
	    }
	    set ip [expr {($ip * 256)+$octet}]
	}
	Debug.human {ip: $ip}
	return $ip
    }

    method ip {r} {
	return [my ipaddr [dict r.-ipaddr]]
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

    method newhuman {r} {
	# add a human record
	set value [clock microseconds]
	while {[dict size [my fetch id $value]]} {
	    set value [clock microseconds]	;# get unique human value
	}
	my append id $value ip [my ip $r]
	my append ips ip $ipaddr last [clock milliseconds]	;# we've seen this IP

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

	return $r
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

	if {$human ne ""
	    && [string is wideinteger -strict $human]
	    && $human != 0
	} {
	    # we think they're human - they return cookies (?)
	    my delete robots ip $ipaddr	;# a reprieve - they returned a cookie

	    # record human's ip addresses and last connection time
	    set record [my fetch id $human]	;# unique record

	    if {[dict size $record]} {
		# we have seen them before
		set iprecord [my fetch ips human $human ip $ipaddr]
		if {[dict size $iprecord]} {
		    # record human as connecting from this ip
		    set id [dict iprecord.id]
		    my incr ips.$id count
		    my set ips.$id last [clock milliseconds]
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
	    # they returned a cookie, we presume they're human
	} else {
	    # discover known robots fast
	    set robot [my fetch robots ip $ipaddr]
	    if {[dict size $robot]} {
		Debug.human {$ipaddr is a known robot}
		dict set r -ua_class robot
		return $r
		# they've not returned a cookie,
		# they're already known to us as a robot
	    } else {
		# they're not a known robot
		set iprecords [my match ips ip $ipaddr]
		if {![llength $iprecords]} {
		    Debug.human {never seen $ipaddr before}
		    dict set r -ua_class browser	;# assume it's a human
		    return [my newhuman $r]	;# create a record for it
		    # we have never seen this IP address before
		    # create a cookie for it, return that and
		    # see how it responds
		} else {
		    my append robots id $ipaddr
		    dict set r -ua_class robot
		    return $r
		    # we have seen this IP address before
		    # it has not returned a cookie
		    # it is a robot
		}
	    }
	}
    }

    method /ip {r ip} {
	set ipaddr [my ipaddr $ip]
	set rec [my fetch ips ip $ipaddr]
	append content [<p> "ip: $ip"]
	append content [<p> "id: [dict rec.id]"]
	append content [<p> "count: [dict rec.count]"]
	set last [clock scan [expr {[dict rec.last]/1000}]]
	append content [<p> "last: $last"]
	append content [<p> "human: [dict rec.human]"]

	set iprecord [my fetch robots ip $ipaddr]
	if {[dict size $iprecord]} {
	    append content [<p> "Known Robot"]
	} else {
	    set record [my fetch human [dict rec.human]]
	    
	}
    }

    method / {r} {
	return [my /ip $r [dict r.-ipaddr]]
    }

    superclass Store Direct
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
	     id INTEGER PRIMARY KEY,	/* associated cookie */
	     ip INTEGER			/* where first seen */
	     );

	    CREATE TABLE ips
	    (
	     id INTEGER PRIMARY KEY AUTOINCREMENT,
	     ip INTEGER,		/* ip address */
	     human INTEGER,		/* associated cookie */
	     count INTEGER,		/* seen how many times? */
	     last INTEGER		/* last seen (ms) */
	     );
	    CREATE INDEX ipid ON ips (ip,human);

	    CREATE TABLE robots
	    (
	     id INTEGER PRIMARY KEY	/* deemed to be a bot IP */
	     );
	}
	Debug.human {tables: [my db tables]}
    }
}
