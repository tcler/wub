# BasicAuth.tcl - auth wrapper around domains
package require base64
package provide BasicAuth 1.0

oo::class create BasicAuth {
    # to work in with Nub, BasicAuth has to provide a new method
    method new {args} {
	return [self]
    }

    method password {user password} {
	variable passwords
	dict set passwords $user $password
    }

    # search the permission dict for a name and password matching those given
    # the search is rooted at the realm dict entry.
    method perms {userid pass el} {
	variable permission
	upvar 1 looked looked
 
	# avoid redundant searching and empties
	if {![dict exists $permission $el]} {
	    dict set looked $el 1
	    return -1	;# there is no $el - keep searching
	} elseif {[dict exists $looked $el]} {
	    return 0	;# already checked $el - no match so far
	} else {
	    dict set looked $el 1	;# record traversal of $el
	}

	set probe [dict get $permission $el]
	if {[llength $probe]%2} {
	    # this is a singleton - must be user+password - check it
	    return [expr {$pass eq $probe}]
	} else {
	    # $el is a dict.
	    # traverse it looking for a match, or a group to search
	    dict for {n v} $probe {
		if {$n eq $userid && $v eq $pass} {return 1}
		if {$v eq "" && ![dict exists $looked $n]} {
		    if {[my perms $userid $pass $n] > 0} {
			return 1
		    }
		}
	    }
	}
	return 0	;# we have exhausted the search space
    }

    # using HTTP Auth, obtain and check a password,
    # return 1 if authenticated, 0 if none match
    method cred {r prefix} {
	set userid ""; set pass ""
	lassign [Http Credentials $r] userid pass
	Debug.basicauth {perms $prefix ($userid,$pass)}

	# filter out evil chars
	set userid [string map {/ ""} [string trim $userid]]
	set pass [string trim $pass]
	if {$userid eq "" || $pass eq ""} {
	    return 0	;# empty is no good
	}

	variable permission
	variable permissive
	if {![dict exists $permission $prefix]} {
	    # there are no $prefix permissions,
	    # permit it or preclude it depending on permissive setting
	    return $permissive
	}

	variable check	;# lambda to actually check
	
	set prefix [file split $prefix]
	while {[llength $prefix]} {
	    set realm [file join {*}$prefix]
	    lappend realms $realm
	    set looked $realms	;# remember traversal
	    switch -- [{*}$check $userid $pass $realm] {
		-1 {
		    Debug.basicauth {'$realm' has no perms - keep searching}
		}
		0 {
		    Debug.basicauth {perms on '$realm' don't match}
		    return 0
		}
		1 {
		    Debug.basicauth {perms on '$realm' ok}
		    return 1
		}
	    }
	    set prefix [lrange $prefix 0 end-1]
	}

	return 0
    }

    # called as "do $request" checks Basic Auth on request
    method do {r} {
	# calculate the suffix of the URL relative to $mount
	variable mount
	lassign [Url urlsuffix $r $mount] result r suffix path
	if {!$result} {
	    return $r	;# the URL isn't in our domain
	}

	# remove suffix's extension and trim /s
	set fn /[string trim [file rootname $suffix] /]
	if {[my cred $r $prefix]} {
	    # now we can call the wrapped object
	    return [{*}$wrapped do $r]
	} else {
	    # credentials not supplied, or didn't match - fail
	    variable fail
	    return [{*}$fail $r [file rootname $path]]
	}
    }

    constructor {args} {
	variable mount
	variable permissive 0	;# if we don't recognise the realm, no-go
	variable permission {}	;# dict of realm -> names, names->passwords
	# permission dict is of the form:
	# realm {name password name1 {} name2 {}}
	# name1 password
	# name2 {name3 password ...}

	# lambda to check for realm-matching
	variable check [list my perms]

	# lambda to generate failure.
	variable fail [list ::apply [list {r realm} {
	    # no passwords matched
	    # challenge the client to provide user,password
	    set challenge "Please login to $realm"
	    set content "Please login to $realm"
	    Debug.basicauth {perms challenge '$realm'}
	    return [Http Unauthorized $r [Http BasicAuth $challenge] $content x-text/html-fragment]
	}]]

	variable {*}[Site var? BasicAuth]	;# allow .ini file to modify defaults
	if {[llength $args]%1} {
	    variable {*}[lrange $args 0 end-1]
	    variable wrapped [lindex $args end]
	} else {
	    variable {*}$args
	}

	# evaluate the wrapped object
	set wargs [lassign $wrapped cmd]
	if {[llength $cmd] == 1} {
	    lappend cmd new
	} else {
	    set cmd [list [lindex $cmd 0] create [lindex $cmd 1]]
	}
	set wrapped [{*}$cmd {*}$wargs]
    }
}
