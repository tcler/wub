# Session - toolkit for sessions in a backend
# maps a global array to a metakit subview

if {[info exists argv0] && ($argv0 eq [info script])} {
    # Unit Test
    set pwd [file normalize [file dirname [file normalize [info script]]]]
    lappend auto_path $pwd $pwd/../tclDb $pwd/../Utilities
}

package require Debug
#package require Cookies
package require Db
package require Thread

package provide Session 2.0

Debug off session 1
Debug off smutex 1

# TODO: test for races in the presence of heavy multithreaded contention

namespace eval Session {
    variable db	;# db containing session view
    variable vv	;# view of current session vars
    variable sv	;# view of all sessions
    variable sid	;# session id
    variable sk		;# session key
    variable mutex	;# session mutex per var
    array set mutex {}
    variable srow	;# row in session view
    variable bfl ""	;# big lock for all sessions
    variable in_transaction
    variable generation

    # pick the best available PRNG
    proc rseed {} {
	if {[file readable /dev/random]} {
	    catch {
		set f [open /dev/random r];
		fconfigure $f -translation binary -buffering none
		set bytes [read $f 4]
		close $f

		binary scan $bytes i r
		return $r
	    }
	}

	return [expr {[clock seconds]^[pid]}]
    }

    if {[catch {package require Random}]} {
	interp alias {} rand {} expr {rand()}
	expr {srand([rseed])}
    } else {
	interp alias {} rand {} expr {isaac_rand()}
	::random::isaac seed [rseed]	;# seed random
    }

    # initialize the Session db view
    proc init {_db viewname _bfl {svar Session}} {
	variable bfl $_bfl
	Debug.session {init: $_db $viewname}
	variable db
	if {[catch {Db $_db -db $_db} db($svar)]} {
	    set db($svar) $_db
	}

	Debug.session "$db($svar) views: [$db($svar) views]" 3
	variable sv
	if {$viewname ni [$db($svar) views]} {
	    set sv($svar) [$db($svar) layout $viewname {
		id:S	# session id
		key:S	# session key
		dead:I	# flags sessions for re-use
		user:I	# user id (optional)
		{vars {
		    var:S	# session variable name
		    value:S	# session variable value
		    dead:I	# flag variable for re-use
		}}
	    }]
	} else {
	    set sv($svar) [View $db($svar).$viewname]
	}
	Debug.session "$db($svar) views: [$db($svar) views]" 3

	catch {unset ::$svar}
	catch {array set ::$svar {}}
    }

    variable set; array unset set
    variable unset; array unset unset

    # wv - write trace - record modification for later commit
    # NB: Session can only be modified within a [transaction] context
    proc wv {svar n1 var op} {
	#if {$var in {id user key}} {
	#    error "Can't write $var"
	#}
	Debug.session {wv $svar $op $var [set ::${svar}($var)]} 3

	variable in_transaction
	if {!$in_transaction($svar)} {
	    error "wrote session variable while not in transaction"
	}

	variable unset
	if {[info exists unset($svar,$var)]} {
	    unset unset($svar,$var)	;# forget that we've previously unset var
	}

	variable set; incr set($svar,$var)	;#remember that we've set var
    }

    # uv - unset trace - record unset for later commit
    # NB: Session can only be modified within a [transaction] context
    proc uv {svar n1 var op} {
	Debug.session {uv $svar $op $var} 3

	variable in_transaction
	if {!$in_transaction($svar)} {
	    error "unset session variable while not in transaction"
	}

	variable set
	if {[info exists set($svar,$var)]} {
	    unset set($svar,$var)	;# forget that we've previously set var
	}

	variable unset; incr unset($svar,$var)	;# remember that we've unset var
    }

    proc 2cookie {{var Session}} {
	variable sid
	variable sk
	return "$sid($var),$sk($var)"
    }

    # rekey an existing session
    proc rekey {{var Session}} {
	Debug.session {rekey} 2
	variable sv
	variable srow
	variable sk; set sk($var) [string range [rand] 2 end]
	$sv($var) set $srow($var) key $sk($var)
	return $sk($var)
    }

    # new: create a completely new session
    # id uniquely identifies the session
    # key is used to protect the session against replay attack
    proc new {var args} {
	Debug.session {new $var $id $key} 2

	variable sv

	set id [dict get? $args id]
	if {$id eq ""} {
	    # supply a new unique session id
	    set id [string range [rand] 2 end]
	    while {![catch {$sv($var) find id $id dead 0} row eo]} {
		set id [string range [rand] 2 end]
	    }
	    dict set args id $id
	}

	set key [dict get? $args key]
	if {$key eq ""} {
	    # generate a random key
	    set key [lindex [string range [rand] 2 end]]
	    dict set args key $key
	}

	Debug.session {new2 $var $id $key} 3

	if {![catch {$sv($var) find id $id dead 0} row eo]} {
	    error "Session $id already exists"
	}

	if {[dict exists $args user]} {
	    set user [list user [dict get $args user]]
	} else {
	    set user {}
	}

	# new key
	set row [$sv($var) insert end id $id key $key {*}$user]	;# add the key
	tsv::set generation $id 0		;# inform others of the new key
	tsv::set scount $id 0			;# count the interest in the new key
	return [attach $id $key $var {*}$args]	;# attach the key to ::Session
    }

    # attach: session id (with mandatory key) to the Session variable
    proc attach {id key var args} {
	variable vv
	Debug.session {attach to $id $key ($var)} 2

	# create/open a subview for session array's elements
	variable sid
	variable sk
	if {[info exists vv($var)] && $vv($var) ne ""} {
	    if {($sid($var) eq $id) && ($sk($var) eq $key)} {
		return [list $id $key]	;# we have the session already
	    } else {
		detach $var	;# detach current session
	    }
	}

	variable sv
	variable srow
	if {[catch {$sv($var) find id $id key $key dead 0} srow($var) eo]} {
	    # no such session
	    error "Session $id doesn't exist"
	}

	set sid($var) $id
	set sk($var) $key
	Debug.session {found view $sv($var) for $id,$key on $srow($var) (var: $var) [array get sid]} 3

	tsv::incr scount $id 1	;# keep count of session attachments

	set vv($var) [$sv($var) open $srow($var) vars]
	Debug.session {created subview $vv($var) for $key on $srow($var)} 3
	fill $var	;# load values into Session
	variable in_transaction; set in_transaction($var) 0

	if {[llength $args] > 0} {
	    transaction {
		array set ::$var $args
	    }
	}

	Debug.session {done attach $id,$key [array get sid]}
	return [list $id $key]
    }

    # find an existing session with matching id or user
    proc user {id user {svar Session}} {
	Debug.session {find session id:'$id' user:'$user' on var:$svar}

	variable sv
	if {$id ne ""
	    && ![catch {$sv($svar) find id $id dead 0} row eo]} {
	    Debug.session {found old session id '$id'}
	    lassign [$sv($svar) get $row id key user] . id . key . suser
	    if {$suser ne $user} {
		if {$suser eq ""} {
		    # need to rewrite user
		    Debug.session {user $user claiming session $id}
		    $sv($svar) set $row user $user
		} else {
		    # change user?  No.
		    error "Can't change session $id user from $suser to $user"
		}
	    }

	    # got our old session, return it
	    return [attach $id $key $svar user $user]
	} elseif {![catch {$sv($svar) find user $user dead 0} row]} {
	    # got old user session, return it.
	    Debug.session {found old user session for $user} 2
	    lassign [$sv($svar) get $row id key] -> id -> key
	    return [attach $id $key $svar]
	} else {
	    # no matching session - create a new one
	    Debug.session {create new session}
	    return [new $svar user $user]
	}
    }

    # detach:
    # detach current Session
    proc detach {{svar Session}} {
	variable vv
	if {![info exists vv($svar)] || $vv($svar) eq ""} {return}
	$vv($svar) destroy; set vv($svar) ""	;# destroy variable view

	# remove ::Session traces
	foreach t [trace info variable ::$svar] {
	    catch {trace remove variable ::$svar {*}$t}
	}
	array unset ::$svar	;# clear variable

	# count decreased reference shared session 
	variable sid;
	if {[tsv::incr scount $sid($svar) -1] == 0} {
	    # clean up now-unused mutex
	    mutex $svar destroy
	}
	unset sid($svar)	;# keep count of sess refs

	# destroy local vars
	variable sid; catch {unset sid($var)}
	variable sk; catch {unset sk($svar)}
	variable mutex; unset mutex($svar)
	variable srow; unset srow($svar)
	variable generation; unset generation($svar)
    }

    # destroy: destroy Session namespace, close backend
    proc destroy {{svar Session}} {
	variable vv
	if {$vv($svar) ne ""} {
	    detach $svar;# detach the session view
	}

	variable sv; $sv($svar) destroy
	variable db; $db($svar) destroy
    }

    # delete:  delete current Session
    # completely removes a Session from backing store.
    proc delete {{svar Session}} {
	variable sid
	mutex $svar wlock
	if {[tsv::get scount $sid($svar)] > 1} {
	    error "Can't destroy $sid($svar) - [tsv::get scount $sid($svar)] attachments"
	    mutex $svar unlock
	}

	variable sv
	variable sk
	if {[catch {$sv($svar) find id $sid($svar) key $sk($svar) dead 0} row eo]} {
	    error "No such Session $sid($svar)"
	} else {
	    # delete all of the stored session vars
	    variable vv
	    $vv($svar) all v {
		$vv($svar) set [dict get $v ""] dead 1
	    }
	    detach $svar	;# detach the session
	    
	    $sv($svar) set $row id "" key "" dead 1	;# delete key
	}

	if {[catch {
	    #mutex unlock
	    mutex $svar destroy
	} r eo]} {
	    puts stderr "Mutex destroy error: $r ($eo)"
	}
    }

    # filler: replace current ::Session with contents of persistent session.
    # NB: not for user code (offers no mutex protection) use [fill] or [fresh] instead
    proc filler {{svar Session}} {
	Debug.session {filling Session ($svar)} 3

	catch {trace remove variable ::$svar write [list Session wv $svar]}
	catch {trace remove variable ::$svar unset [list Session uv $svar]}
	catch {trace remove variable ::$svar read [list Session rv $svar]}

	catch {array unset ::$svar}
	catch {array set ::$svar {}}

	variable vv
	$vv($svar) all v {
	    dict with v {
		if {!$dead} {
		    set ::${svar}($var) $value
		    Debug.session {filled Session: '$var' with '$value'} 6
		}
	    }
	}
	Debug.session {filled Session: [array get ::$svar]} 4

	# get current generation
	variable sid
	if {![tsv::exists generation $sid($svar)]} {
	    # make sure there's a shared session generation var
	    tsv::set generation $sid($svar) 0
	}
	variable generation; set generation($svar) [tsv::get generation $sid($svar)]

	trace add variable ::$svar write [list Session wv $svar]
	trace add variable ::$svar unset [list Session uv $svar]
	trace add variable ::$svar read [list Session rv $svar]
    }

    # fill: unconditionally fill Session from persistent session (with mutex protection)
    proc fill {{svar Session}} {
	Debug.session {fill Session} 3
	mutex $svar rlock; filler $svar; mutex $svar unlock
    }

    # fresh: ensure the Session array is fresh (fills with updates if necessary)
    # assumes that the mutex is rlock'd
    proc fresh {{svar Session}} {
	variable generation
	variable sid
	if {$generation($svar) != [tsv::get generation $sid($svar)]} {
	    Debug.session {freshen Session} 3
	    filler $svar
	    set generation($svar) [tsv::get generation $sid($svar)]
	}
    }

    # rv: read trace - freshen Session
    proc rv {svar n1 var op} {
	Debug.session {rv $svar $op $var $::Session($var)} 3

	# can't let this fail
	if {[catch {
	    mutex $svar rlock
	    fresh $svar
	    mutex $svar unlock
	} e eo]} {
	    Debug.session {rv failed: $e ($eo)}
	}

	Debug.session {rv completed: [set ::${svar}($var)]} 3
    }

    # fetch session id,key from a cookie called 'session' in request.
    proc fetch {r {var Session}} {
	# get cookies
	if {[dict exists $r cookie]} {
	    set cookies [Cookies parse4server [dict get $r cookie]]
	    
	    # associate cookie with session
	    if {[dict exist $cookies session]} {
		set ::sextra [join [lassign [split [dict get $cookies session] ,] id key]]
		Session attach $id $key $var
		return [list $id $key]
	    } else {
		Session detach $var	;# detach any open session
		return {}
	    }
	} else {
	    return {}
	}
    }

    # store the session id,key in a cookie called 'session' in request.
    proc store {r {maxAge 0} {cookie {}}} {
	Debug.session {store session in cookie} 2
	if {[dict exists $r -cookies]} {
	    set cdict [dict get $r -cookies]
	} else {
	    set cdict [dict create]
	}
	set dom [dict get $r -host]

	if {[info exists ::Session(id)]} {
	    Debug.session {storing session $::Session(id) in cookie $cookie} 2
	    set scookie [2cookie]
	    if {$cookie ne $scookie} {
		# we have a new session to package up
		# include an optional expiry age
		if {![string is integer -strict $maxAge]} {
		    if {[catch {
			expr {[clock scan $maxAge] - [clock seconds]}
		    } maxAge]} {
			set age {}
		    } else {
			set age [list -max-age $maxAge]
		    }
		}

		if {$maxAge} {
		    set age [list -max-age $maxAge]
		} else {
		    set age {}
		}

		set cdict [Cookies add $cdict -path / -domain $dom -name session -value $scookie {*}$age]
		dict set r -cookies $cdict
	    } else {
		# the cookie is ok as is
	    }
	} else {
	    # got to lose the cookie - reset it
	    dict set r -cookies [Cookies clear $cdict -path / -domain $dom -name session]
	}

	return $r
    }

    proc key {id {var Session}} {
	variable sv
	return [$sv($var) get [$sv($var) find id $id dead 0] key]
    }

    # mutex - handle mutex command
    # finds or creates a thread-shared-variable for mutexing
    # this session between several threads
    proc mutex {var args} {
	Debug.smutex {mutex: $var $args}
	variable mutex
	if {![info exists mutex($var)] || $mutex($var) eq ""} {
	    variable sid
	    variable bfl; thread::mutex lock $bfl
	    if {![tsv::exists mutex $sid($var)]} {
		Debug.smutex {new mutex for $var $sid($var)}
		if {[catch {tsv::set mutex $sid($var) [thread::rwmutex create]} r eo]} {
		    puts stderr "mutex $var create error: $r ($eo)"
		} else {
		    #puts "mutex create: $r ($eo)"
		}
	    }
	    thread::mutex unlock $bfl
	    set mutex($var) [tsv::get mutex $sid($var)]
	    Debug.smutex {mutex for $sid($var) -> $mutex($var)}
	}
	Debug.smutex {mutex $args $mutex($var)}
	return [thread::rwmutex {*}$args $mutex($var)]
    }

    # atomic: perform body inside a rlock mutex
    proc atomic {body {var Session}} {
	mutex $var rlock; fresh $var	;# ensure Session is fresh

	catch {trace remove variable ::$var read [list Session rv $var]}
	set code [catch {uplevel $body} r eo]
	Debug.session {atomic body: $code - $r ($eo)}
	trace add variable ::$var read [list Session rv $var]

	mutex $var unlock			;# unlock this session
	return -options $eo $r		;# re-raise any error
    }

    # transaction:
    # perform body inside a wlock mutex, excluding all other read/writes
    # roll back any changes made to Session if an error occurs
    # otherwise commit any changes made to Session
    proc transaction {body {var Session}} {
	variable unset; array set lunset [array get unset $var,*]
	variable set; array set lset [array get set $var,*]
	if {([array size lset] + [array size lunset]) != 0} {
	    error "Transaction half-state [array get set] [array get unset]"
	}

	mutex $var wlock
	fresh $var	;# ensure Session is fresh

	variable in_transaction
	set in_transaction($var) 1

	catch {trace remove variable ::$var read [list Session rv $var]}
	set code [catch {uplevel $body} r eo]
	Debug.session {Transaction body: $code - $r ($eo)}

	if {$code == 1} {
	    # transaction error occurred
	    # restore ::Session to its original value
	    Debug.session {Transaction failed}

	    array unset set $var,*	;# lose session modification state 
	    array unset unset $var,*	;# lose session modification state 
	    filler $var	;# roll back Session
	    set in_transaction($var) 0		;# permit user mods
	    mutex $var unlock	;# unlock this session

	    Debug.session {Transaction Failed: [array get set] [array get unset]}
	    return -options $eo $r		;# re-raise the error
	} else {
	    unset lunset; array set lunset [array get unset $var,*]
	    unset set; array set lset [array get set $var,*]
	    Debug.session {Transaction succeeded: unset:[array names lunset] set:[array names lset]}
	    if {([array size lunset] + [array size lset]) > 0} {
		catch {trace remove variable ::$var write [list Session wv $var]}
		catch {trace remove variable ::$var unset [list Session uv $var]}
		catch {trace remove variable ::$var read [list Session rv $var]}

		# give effect to any unsets
		variable vv
		foreach n [array names unset $var,*] {
		    set n [lindex [split $var ,] 1]
		    if {![catch {$vv($var) find var $n dead 0} row eo]} {
			$vv($var) set $row var "" value "" dead 1
			Debug.session {persist unset: $n}
		    } else {
			# it was never persisted anyway
		    }
		}

		# give effect to any writes
		foreach n [array names set $var,*] {
		    set n [lindex [split $var ,] 1]
		    if {[catch {$vv($var) find var $n dead 0} row eo]} {
			# no such variable - create it
			if {[catch {$vv($var) find dead 1} row eo]} {
			    #Debug.session {not found $n: $r ($eo)} 9
			    set row [$vv($var) insert end var $n dead 0]
			    Debug.session {new row $row for $n} 6
			} else {
			    Debug.session {re-using row $row for $n} 6
			}
		    }
		    Debug.session {persist set: $n to '[array get ::$var $n]' at row:$row}
		    $vv($var) set $row value [set ::${var}($n)]
		}

		variable sid
		set generation($var) [tsv::incr generation $sid($var)] ;# incr session generation

		array unset set $var,*	;# lose session modification state
		array unset unset $var,*	;# lose session modification state 
	    }

	    set in_transaction($var) 0		;# disallow user mods
	    mutex $var unlock			;# unlock this session

	    Debug.session {Transaction Complete: [array get set] [array get unset]}
	    if {$code == 2} {
		# transaction returned - we must return from caller
		set eo [dict merge {-level 1} $eo]
		dict incr eo -level
		return -options $eo $r
	    } else {
		return $r			;# return transaction value
	    }
	}

    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    Debug off db 0
    Debug on session 10
    Debug on smutex 10

    #file delete session_test.db
    puts [Db ::Session::sdb -file session_test.db -shared 1]

    set bfl [thread::mutex create]
    Session init sdb svars $bfl Trip

    if {[catch {Session attach S1 none Trip} r eo]} {
	puts "Error: $r ($eo)"
	Session new Trip id S1
    }

    puts "Initial Value S1: [array get Trip]"
    Session transaction {
	set Trip(name) S1
	puts "Array: [array get Trip]"
	puts "Counter: [incr Trip(counter)]"
	puts "Time: [set Trip(time) [clock microseconds]]"
	set Trip(undo) 1
	puts "Array: [array get Trip]"
	unset Trip(undo)
    } Trip

    Session atomic {
	puts "Atomic Array: [array get Trip]"
    } Trip

    if {[catch {Session attach S2 "" Trip} r eo]} {
	#puts "Error: $r ($eo)"
	Session new Trip id S2
    }

    Session transaction {
	set Trip(name) S2
	puts "key: [Session rekey Trip]"
	puts "Array: [array get Trip]"
	puts "Counter: [incr Trip(counter)]"
	puts "Time: [set Trip(time) [clock microseconds]]"

	set Trip(undo) 1
	puts "Array: [array get Trip]"
	unset Trip(undo)
	puts "Array: [array get Trip]"
    } Trip

    Session delete Trip
}
