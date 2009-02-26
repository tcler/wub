# Login is a domain to handle simple login account handling

package require TclOO
namespace import oo::*

set API(Login) {
    {
	Login is a [Direct] domain for simple cookie-based login account management and a repository for user specific field values.

	Login requires an account [View] which contains at least a user and password field, these allow a user to log.  It can perform in a permissive mode, which allows anonymous account creation, or (the default) can refuse to create a new account from the login form.  In this case, the /new url can be used to construct a new user (/new can paramterised as any URL, and can be called from another [Direct] domain to compose a new user creation form.)

	Using the /set URL you can store any data in the account View of the logged-in user from a simple <form>.  The /get method returns this data in JSON format, for AJAX processing.  You don't need to declare the data you wish to store in the user's account record: any fields which appear in the account db layout can be searched and manipulated by server code, and any others are stashed as a tcl dict in the 'args' field of the account record (if it is specified in the db layout.)  This makes Login a general purpose store for associating data with a logged in user.

	== Methods ==
	;user r {user ""}: fetches account record of the specified user, or the logged-in user if blank.
	;set r args: sets account record of user (specified by the value of the user field.)
	;account args: evaluates args over the account [View] or (if empty args) returns the [View].
	;clear r: clears all login cookies - effectively logging user out
	;login r index {user ""} {password ""}: performs login from code, given at least account index of user to log in.

	== URLs ==
	;/login args: logs in the current user, must specify user and password fields
	;/logout {url ""}: logs out the current user, then redirects to specified url
	;/form: returns a login form or a logout link, depending on current login status
	;/get {fields ""}: returns JSON object containing specified account information fields for logged-in user (default: all fields).
	;/set args: sets account information for logged-in user, suitable for use as the action in a <form>
	;/new: args stores a new user's data - the user must be unique.  By default, non-permissive /login redirects to /new if the user doesn't exist (redirection may be respecified by the configuration variable new.)
    }
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
    autocommit {boolean - commit on each write?}
}

package require Debug
Debug off login 10

package require md5
package require Direct
package require View
package require Cookies

package provide Login 1.0

class create Login {
    variable account realm properties cookie age emptypass userF passF cpath domain forms keys jQ permissive autocommit new

    # fetch account record of logged-in user
    method user {r {user ""}} {
	if {$user eq ""} {
	    set cdict [Dict get? $r -cookies]

	    # determine the right domain/path for the cookie
	    set cd [list -name $cookie]
	    if {[info exists domain] && $domain ne ""} {
		dict set cd -domain $domain
	    }

	    Debug.login {logged in user in $cdict}

	    # fetch the cookie
	    if {![catch {Cookies fetch $cdict -name $cookie} cl]} {
		set key [dict get $cl -value]
		if {[info exists keys($key)]} {
		    set index [dict get $keys($key) ""]
		    Debug.login {logged in user is $index}
		} else {
		    Debug.login {bogus key: $key}
		    set r [my clear $r]
		}
	    } {
		Debug.login {no user logged in under cookie '$cookie'}
		return ""
	    }
	} else {
	    # passed in a user key - search for it
	    Debug.login {user for '$user'}
	    if {[catch {$accound find $userF $user} index]} {
		Debug.login {no such user '$user'}
		return ""
	    }
	}

	if {[catch {$account get $index} record eo]} {
	    Debug.login {can't read user's record $index in $account - $record ($eo)}
	    return ""
	} else {
	    dict set record "" $index
	    Debug.login {got user $index ($record)}
	    return $record
	}
    }

    # return data stored in user record
    method /get {r {fields ""}} {
	set record [my user $r]

	# convert dict to json
	set result {}
	if {[info exists $record args]} {
	    set record [dict merge $args $record]	;# merge args field into record
	}
	dict for {n v} $record {
	    if {$n eq ""} continue
	    if {$fields ne "" && $n ni $fields} continue
	    lappend result "\"$n\": \"$v\""
	}
	set result \{[join $result ,\n]\}
	return [Http NoCache [Http Ok $r $result application/json]]
    }

    # set account record of logged-in user
    method set {r args} {
	if {[dict exists $args $userF]} {
	    # specified user
	    set record [my user $r [dict get $args $userF]]
	} else {
	    # want logged-in user
	    set record [my user $r]
	}

	if {$record ""} {
	    return ""
	}

	set index [dict get $record ""]; dict unset record ""
	catch {dict unset args ""}	;# remove the index

	dict for {n v} $args {
	    if {$n eq "args"} continue
	    if {$n in $properties} {
		set dict $record $v
		dict unset args $n
	    }
	}

	# store surplus variables in args, if it exists
	if {"args" in $properties} {
	    dict set record args [dict merge [Dict get? $record args] $args]
	}

	set record [dict merge $record $args]
	$account set $index {*}$record
	$account db commit
	return $record
    }

    # store some data in the user's record
    method /set {r args} {
	catch {[dict unset args $userF]}	;# want only logged-in user
	set record [my set $r {*}$args]

	if {$record ""} {
	    return [Http NotFound $r [<p> "Not logged in"]]
	} else {
	    return [Http Ok $r [<message> "User $user Logged in."]]
	}
    }

    # open account view to outside
    method account {args} {
	if {[llength $args]} {
	    return [$account {*}$args]
	} else {
	    return $account
	}
    }

    method clear {r} {
	set cdict [Dict get? $r -cookies]
	Debug.login {logout $cookie from $cdict}

	# determine the right domain/path for the cookie
	set cd [list -name $cookie]
	if {[info exists domain] && $domain ne ""} {
	    dict set cd -domain $domain
	}

	# fetch the cookies
	if {![catch {Cookies fetch $cdict -name $cookie} cl]} {
	    catch {unset keys([dict get $cl -value])}	;# forget key
	    # clear cookies
	    foreach cp $cpath {
		set cdict [Cookies clear $cdict {*}$cd -path $cp]
	    }

	    # rewrite the cleared cookies
	    dict set r -cookies $cdict
	}
	return $r
    }

    method /logout {r {url ""}} {
	set r [Http NoCache $r]
	set r [my clear $r]
	if {$url eq ""} {
	    set url [Http Referer $r]
	    if {$url eq ""} {
		set url "http://[dict get $r host]/"
	    }
	}

	return [Http NoCache [Http SeeOther $r $url "Logged out"]]
    }

    # return a login form
    method /form {r} {
	set r [Http NoCache $r]
	set code [catch {Cookies fetch [dict get $r -cookies] -name $cookie} cl]
	if {!$code && [info exists keys([set key [dict get $cl -value]])]} {
	    # already logged in - return a logout link instead
	    if {$jQ} {
		set r [jQ form $r .login target '#message']
		set r [jQ hint $r]	;# style up the form
		if {0} {
		    set r [jQ postscript $r {
			$('input[title!=""]').hint();
			$('.login').ajaxForm({target:'#message'});
		    }]
		}
	    }

	    Debug.login {/form: logged in already as $keys($key)}
	    return [Http Ok $r [dict get $forms logout]]
	} else {
	    if {!$code} {
		# there are cookies, but they're bogus
		set key [dict get $cl -value]
		Debug.login {/form: bogus cookie: $key}
		set r [my clear $r]
	    }

	    if {$jQ} {
		set r [jQ form $r .login target '#message']
		set r [jQ hint $r]	;# style up the form
		if {0} {
		    set r [jQ postscript $r {
			$('input[title!=""]').hint();
			$('.login').ajaxForm({target:'#message'});
		    }]
		}
	    }

	    # not already logged in - return a login form
	    Debug.login {/form: not logged in}
	    return [Http Ok $r [dict get $forms login]]
	}
    }

    # send the client to a page indicating the failure of their login
    method logerr {r {message "Login Failed"} {url ""}} {
	if {$jQ} {
	    if {0} {
		set r [jQ postscript $r {
		    $('input[title!=""]').hint();
		    $('.login').ajaxForm({target:'#message'});
		}]
	    }
	    return [Http Ok $r [subst [dict get $forms logerr]]]
	} else {
	    if {$url eq ""} {
		set url [Http Referer $r]
		if {$url eq ""} {
		    set url "http://[dict get $r host]/"
		}
	    }
	    return [Http Forbidden $r [subst [dict get $forms logerr]]]
	}
    }

    method new {r args} {
	# ensure the new record is minimally compliant
	if {![dict exists $args $userF] || [dict get $args $userF] == 0} {
	    return 0	;# we refuse to allow blank users
	}
	if {$emptypass && ![dict exists $args $passF] || [dict get $args $passF] == 0} {
	    return 0	;# we refuse to allow blank users
	}

	set record [user $args]
	if {$record ne ""} {
	    # the user must be unique
	    return 0
	}

	set index [$account append $userF $user $passF $password]	;# create a new account record
	my set $r {*}$args	;# store the rest of the data
	return 1
    }

    method /new {r {submit 0} args} {
	if {$submit} {
	    if {[$new $r {*}$args]} {
		return [my logerr $r "New user '[dict get $args $argF]' created"]
	    } else {
		return [my logerr $r "There's already a user '[dict get $args $argF]'"]
	    }
	} else {
	    # throw up a new user form.
	    return [dict get $forms new]
	}
    }

    # perform the login of user at $index
    method login {r index {user ""} {password ""}} {
	if {$user eq ""} {
	    # fetch user details
	    lassign [$account get $index $userF $passF] user password
	}
    	if {[dict exists $r -cookies]} {
	    set cdict [dict get $r -cookies]
	} else {
	    set cdict {}
	}

	# construct a login record keyed by md5
	set key [::md5::md5 -hex "[clock microseconds]$user$password"]
	set keys($key) [list user $user password $password "" $index]
	Debug.login {login: created key $key}

	set cd [list -name $cookie -value $key]
	
	# include an optional expiry age for the cookie
	if {[info exists age] && $age} {
	    dict set cd -expires $age
	}
	
	# determine the right domain/path for the cookie
	if {[info exists domain] && $domain ne ""} {
	    dict set cd -domain $domain
	}
	
	# add in the cookies
	foreach cp $cpath {
	    set cdict [Cookies add $cdict {*}$cd -path $cp]
	}
	dict set r -cookies $cdict
	Debug.login {login: added cookie $cookie to $cdict}

	return $r
    }

    # login from a form
    method /login {r args} {
	set r [Http NoCache $r]
	if {$jQ} {
	    set r [jQ form $r .login target '#message']
	    set r [jQ hint $r]	;# style up the form
	}

	# expect vars user and password, accept url
	set user [Dict get? $args $userF]
	set password [Dict get? $args $passF]
	set url [Dict get? $args url]

	Debug.login {/login: user:$user password:$password url:$url}

	# prelim check on args
	if {$user eq ""} {
	    return [my logerr $r "Blank username not permitted." $url]
	}
	if {!$emptypass && $password eq ""} {
	    return [my logerr $r "Blank password not permitted." $url]
	}

	# find matching user in account
	if {[catch {$account find $userF $user} index]} {
	    if {$permissive && $user ne "" && $password ne ""} {
		# permissive - create a new user
		Debug.login {/login: permissively creating user}
		set index [$account append $userF $user $passF $password]
	    } else {
		Debug.login {/login: no such user}
		if {$new eq ""} {
		    return [my logerr $r "There is no such user as '$user'." $url]
		} else {
		    # redirect to a new URL for collecting account information
		    # the URL can decide to grant an account using /new
		    return [Http Redirect $req $new]
		}
	    }
	}

	# match account password
	if {[$account get $index $passF] ne $password
	    && ("old_$passF" ni $properties
		|| [$account get $index old_$passF] ne $password)	   
	} {
	    Debug.login {/login: passwords don't match}
	    return [my logerr $r "Password doesn't match for '$user'." $url]
	}

	set r [login $r $index $user $password]
	
	if {$jQ} {
	    # assume the .form plugin is handling this.
	    return [Http Ok $r [<message> "User $user Logged in."]]
	} else {
	    if {$url eq ""} {
		set url [Http Referer $r]
		if {$url eq ""} {
		    set url "http://[dict get $r host]/"
		}
	    }
	    return [Http NoCache [Http SeeOther $r $url "Logged in as $user"]]
	}
    }

    constructor {args} {
	Debug.login {constructing $args}
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
	set permissive 0	;# no you can't have a login
	set autocommit 1	;# commit on each write
	set new ""		;# url to redirect to on new user creation request

	dict for {n v} $args {
	    set $n $v
	}

	array set keys {}	;# start remembering our keys

	if {![info exists forms]} {
	    set forms {}
	}

	if {![dict exists $forms login]} {
	    # forms for Login
	    dict set forms login [subst {
		[<form> action [file join $mount login] class login {
		    [<submit> submit style {display:none;} Login]
		    [<text> $userF size 8 title Username]
		    [<text> $passF size 8 title Password]
		}]
	    }]
	}
	if {![dict exists $forms logout]} {
	    dict set forms logout [<a> href [file join $mount logout] Logout]
	}
	if {![dict exists $forms logerr]} {
	    dict set forms logerr {[<message> "$message [<a> href $url {Go Back.}]"]}
	}
	if {![dict exists $forms new]} {
	    dict set forms new [<form> newuser [<fieldset> [subst {
		[<legend> "Create User"]
		[<text> $userF title "user id" label "User Id: " ""]
		[<text> $passF title "password" label "Password: " ""]
		[<text> given title "given name" label "Given: " ""]
		[<text> surname title "surname" label "Surname: " ""]
		[<submit> submit value 1]
	    }]]]
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
	
	# the /login path must always get this cookie
	if {[lsearch $cpath $mount] == -1} {
	    lappend cpath $mount
	}
	
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
	Debug.login {constructed [self] $args}
    }
}

if {0} {
    # example of how Login might be used to control the domain /cookie/
    package require Login 
    Debug on login 100
    Nub domain /login/ Direct object {Login ::L account {db accountdb file account.db layout {user:S password:S args:S}} cpath /cookie/ permissive 1 jQ 1} ctype x-text/html-fragment
    
    Nub code /login/test {
	set r [::L /form $r]
	set user [::L user $r]
	set cdict [Dict get? $r -cookies]
	
	set result [dict get $r -content]
	append result [<div> id message {}]
	append result [<p> "User: $user"]
    }
}
