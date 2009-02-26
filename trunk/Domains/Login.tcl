# Login is a domain to handle simple login account handling

package require TclOO
namespace import oo::*

set API(Login) {
    {Login is a domain for simple cookie-based login account management}
    account {View for storing accounts (must have at least user and password fields)}
    cookie {cookie for storing tub key (default: tub)}
    age {cookie age}
    domain {domain covered by login cookie}
    cpath {list of paths for login cookie}
    emptypass {boolean - are passwords required to be non-blank (default: 0, of course)}
    realm {Realm used in password challenges for AUTH based login}
    userF {field name for user name/id (default: user)}
    passF {field name for password (default: password)}
    permissive {boolean - completely anonymous accounts?}
}

package require Debug
Debug on tub 10

package require Direct
package require View
package require Cookies

package provide Login 1.0

class create Login {
    variable account realm properties cookie age emptypass userF passF coco forms keys jQ

    # fetch account record of logged-in user
    method user {r {user ""}} {
	if {$user eq ""} {
	    set cdict [Dict get? $r -cookies]

	    # determine the right domain/path for the cookie
	    set cd [list -name $cookie]
	    if {![info exists domain] || $domain == ""} {
		dict set cd -domain $domain
	    }

	    # fetch the cookie
	    if {![catch {Cookies fetch $cdict -name $cookie} cl]} {
		set key [dict get $cl -value]
		set index [dict get $keys($key) ""]
	    } {
		return ""
	    }
	} else {
	    # passed in a user key - search for it
	    if {[catch {$accound find $userF $user} index]} {
		return ""
	    }
	}

	if {[catch {$account get $index} record]} {
	    return ""
	} else {
	    dict set record "" $index
	    return $record
	}
    }

    # set account record of logged-in user
    method set {r args} {
	if {[dict exists $args $userF]} {
	    # specified user
	    set record [user $r [dict get $args $userF]]
	} else {
	    # want logged-in user
	    set record [user $r]
	}

	if {$record ""} return
	set index [dict get $record ""]; dict unset record ""
	catch {dict unset args ""}
	set record [dict merge $record $args]
	$account set $index {*}$record
    }

    # open account view to outside
    method account {args} {
	return [$account {*}$args]
    }

    # return a login form
    method /form {r} {
	set r [Http NoCache $r]
	set key [Cookie fetch [dict get $r -cookies]] 

	if {[info exists keys($key)]} {
	    # already logged in - return a logout link instead
	    return [Http Ok [dict get $forms logout]]
	} else {
	    # not already logged in - return a login form
	    if {$jQ} {
		set r [jQ hint $r]
	    }
	    return [Http Ok [dict get $forms login]]
	}
    }

    # login from a form
    method /login {r args} {
	set r [Http NoCache $r]

	# expect vars user and password, accept url
	set user [Dict get? $args $userF]
	set password [Dict get? $args $passF]
	set url [Dict get? $args url]

	# prelim check on args
	if {$user eq ""} {
	    return [Httpd NotFound $r "Blank username not permitted."]
	}
	if {!$emptypass && $password eq ""} {
	    return [Httpd NotFound $r "Blank password not permitted."]
	}

	# find matching user in account
	if {[catch {$account find $userF $user} index]} {
	    if {$permissive && $user ne "" && $password ne ""} {
		# permissive - create a new user
		set index [$account append $userF $user $passF $password]
	    } else {
		return [Httpd NotFound $r "There is no such user as '$user'"]
	    }
	}

	# match account password
	if {[$account get $index $passF] ne $password
	    && (old_$passF ni $properties
		|| [$account get $index old_$passF] ne $password)	   
	} {
	    return [Httpd NotFound $r "Password doesn't match for '$user'"]
	}
	
	if {[dict exists $r -cookies]} {
	    set cdict [dict get $r -cookies]
	} else {
	    set cdict {}
	}

	# construct a login record keyed by md5
	set key [zlib md5 "[clock microseconds]$user$password"]
	set keys($keys) [list user $user password $password "" $index]

	set cd [list -name $cookie -value $key]

	# include an optional expiry age for the cookie
	if {$age} {
	    dict set cd -expires $age
	}

	# determine the right domain/path for the cookie
	if {![info exists domain] || $domain == ""} {
	    dict set cd -domain $domain
	}

	# add in the cookies
	if {[info exists cpath] && $cpath ne ""} {
	    foreach cp $cpath {
		set cdict [Cookies add $cdict {*}$cd -path $cp]
	    }
	} else {
	    set cdict [Cookies add $cdict {*}$cd]
	}
	dict set r -cookies $cdict

	if {$url eq ""} {
	    set url [Http Referer $r]
	    if {$url eq ""} {
		set url "http://[dict get $r host]/"
	    }
	}

	return [Http NoCache [Http SeeOther $r $url "Logged in as $user"]]
    }

    method /logout {r {url ""}} {
	set r [Http NoCache $r]

	set cdict [Dict get? $r -cookies]

	# determine the right domain/path for the cookie
	set cd [list -name $cookie]
	if {![info exists domain] || $domain == ""} {
	    dict set cd -domain $domain
	}

	# fetch the cookies
	if {![catch {Cookies fetch $cdict -name $cookie} cl]} {
	    catch {unset keys([dict get $cl -value])}	;# forget key
	    # clear cookies
	    if {[info exists cpath] && $cpath ne ""} {
		foreach cp $cpath {
		    set cdict [Cookies clear $cdict {*}$cd -path $cp]
		}
	    } else {
		set cdict [Cookies add $cdict {*}$cd]
	    }

	    # rewrite the cleared cookies
	    dict set r -cookies $cdict
	}

	if {$url eq ""} {
	    set url [Http Referer $r]
	    if {$url eq ""} {
		set url "http://[dict get $r host]/"
	    }
	}

	return [Http NoCache [Http SeeOther $r $url "Logged out"]]
    }

    constructor {args} {
	set userF user		;# user field in $account view
	set passF password	;# password field in $account view
	set cookie login	;# name of the cookie
	set permissive 0	;# allow anonymous creation?
	set emptypass 0	;# permit blank passwords?
	set account {user:S password:S}	;# minimal layout for account
	set domain ""	;# domain for cookies
	set cpath ""	;# list of paths for cookies
	set jQ 1	;# use jQ by default
	set realm "Login [self]"	;# login for Basic AUTH

	dict for {n v} $args {
	    set $n $v
	}

	array set keys {}	;# start remembering our keys

	if {![exists forms]} {
	    set forms {}
	}
	if {![dict exists $forms login]} {
	    # forms for Login
	    dict set forms login [subst {
		[<form> action login {
		    [<submit> submit Login]
		    [<text> $userF size 8 title Username]
		    [<text> $passF size 8 title Password]
		}]
	    }]
	}
	if {![dict exists $forms logout]} {
	    dict set forms logout [<a> href logout Logout]
	}

	# create the data account
	if {[llength $account] > 1} {
	    if {[llength $account]%2} {
		set account [View create {*}$account]
	    } else {
		set account [View new {*}$account]
	    }
	} else {
	    # we presume the caller already has a view
	}

	set properties [$account properties]	;# remember the account properties

	if {![info exists userF] || $userF eq ""} {
	    if {[info exists cookie] && $cookie ne ""} {
		set userF $cookie
	    } else {
		set properties [lassign [lindex $properties 0] userF]
	    }
	} elseif {$userF ni $properties} {
	    error "Field '$userF' must appear in $account's layout ($properties)"
	} elseif {$passF ni $properties} {
	    error "Field '$passF' must appear in $account's layout ($properties)"
	}
    }
}
