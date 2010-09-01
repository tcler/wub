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

    method addcookie {r value} {
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

    method newhuman {r} {
	# add a human record
	set value [clock microseconds]
	while {[dict size [my fetch id $value]]} {
	    set value [clock microseconds]	;# get unique human value
	}
	set ipaddr [my ip $r]
	my append human $value ip $ipaddr last [clock milliseconds] count 0

	return [my addcookie $r $value]
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
	} {
	    # we think they're human - they return cookies (?)
	    # record human's ip addresses and last connection time
	    set records [my by human $human]	;# unique record
	    dict set r -ua_class browser	;# presume human

	    if {[llength $records]} {
		# we have seen them before
		set record [my fetch human $human ip $ipaddr]
		if {[dict size $record]} {
		    # record human as connecting from this ip
		    set id [dict record.id]
		    my incr $id count
		    my set $id last [clock milliseconds]
		} else {
		    # We have seen this human before,
		    # just not from this ip
		    my append human $human ip $ipaddr count 1 last [clock milliseconds]
		}
	    } else {
		# the returned cookie is unknown
		# - we should send a new cookie, and wait
		set r [my newhuman $r]
	    }
	    # they returned a cookie, we presume they're human
	} else {
	    # no cookie returned - flag them and return a cookie
	    set rec [lindex [my match ip $ipaddr count 0] 0]
	    dict set r -ua_class robot

	    if {![dict size $rec]} {
		Debug.human {never seen $ipaddr before}
		return [my newhuman $r]	;# create a new record for it
		# we have never seen this IP address before
		# create a cookie for it, return that and
		# see how it responds
	    } else {
		return [my addcookie $r [dict rec.human]]
		# we have seen this IP address before
		# it has not yet returned a cookie
		# it is a robot
	    }
	}
    }

    method /ip {r ip} {
	set ipaddr [my ipaddr $ip]
	foreach el {human count last} {
	    lappend row [<th> $el]
	}
	append table [<tr> [join $row \n]] \n
	foreach rec [my match ip $ipaddr] {
	    set row [<td> [dict rec.human]]
	    lappend row [<td> [dict rec.count]]
	    set last [clock scan [expr {[dict rec.last]/1000}]]
	    lappend row [<td> $last]
	    append table [<tr> [join $row \n]] \n
	}
	set table [join $table \n]
	return [Http Ok $r $table]
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
	    CREATE TABLE human
	    (
	     id INTEGER PRIMARY KEY AUTOINCREMENT,
	     human INTEGER PRIMARY KEY,	/* associated cookie */
	     ip INTEGER			/* ip address */
	     count INTEGER,		/* seen how many times? */
	     last INTEGER		/* last seen (ms) */
	     );
	    CREATE INDEX iphuman ON human (ip,human);
	}
	Debug.human {tables: [my db tables]}
    }
}
