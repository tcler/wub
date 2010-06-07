# Login is a domain to handle simple login account handling

package require TclOO
namespace import oo::*

set ::API(Domains/Login) {
    {
	Simple cookie-based login account management and is simultaneously a repository for field values keyed by user.  It is intended to be constructed under a [Direct] domain, as illustrated in the example below.

	== Operation ==
	Login provides a /login url to authenticate a user by password, and generate a cookie recording this authentication.  /logout removes the login cookies.

	/login can operate in a ''permissive'' mode (which allows anonymous account creation,) but the default response to non-existent user is to redirect to the url /new which will collect account information construct a new user.

	The provided /new will refuse to create duplicate users, and also refuse blank users and (by default) blank passwords.

	The /logout url will remove all the login cookies, logging the user out.

	Login also provides a /form url which either returns a form suitable to /login, or a button to /logout a user.

	=== URLs ===
	The Login domain provides the following URLs:

	;/login args: logs in the current user, must specify user and password fields
	;/logout {url ""}: logs out the current user, then redirects to specified url
	;/form: returns a login form or a logout link, depending on current login status
	;/new: args stores a new user's data - the user must be unique.

	== Repository ==
	Login also functions as a repository for form data associated with logged-in users.  You can easily store and data from <form>s and fetch them for javascript processing.

	The /set URL stores any data it is passed in the account View of the logged-in user.  It's intended to be invoked from a simple <form>.

	The /get method returns all account data in JSON format, suitable for AJAX processing.

	The data stored with /set doesn't have to be declared: any fields which appear explictly in the account db layout can be searched and manipulated by server db code, and any other fields and values are stashed as a tcl dict in the ''args'' field of the account record (if it is specified in the db layout.)  This makes Login a general purpose store for associating data with a logged in user.

	=== URLS ===
	;/get {fields ""}: returns JSON object containing specified account information fields for logged-in user (default: all fields).
	;/set args: sets account information for logged-in user, suitable for use as the action in a <form>

	== Database ==
	Login requires an account [http:../Utility/View View] which contains at least ''user'' and ''password'' fields, these are the minimum to allow a user to log in.  In addition, a field password_old is available as a fallback password, useful in the case of incomplete password changes.  Any other fields in the View are available to be stored and fetched by /set and /get (respectively.)

	== Methods ==
	;user r {user ""}: fetches account record of the specified user, or the logged-in user if no user is specified.  If the data can't be fetched (if, for example, there's no user logged in) then this method returns an empty list.  This can be reliably used to determine the identity of a logged-in user.
	;set r args: sets fields in the account record of user according to the dict in $args.
	;account args: evaluates args over the account [View] or (if empty args) returns the [View].  This can be used to process the account database.
	;clear r: clears any login cookies for this instance of Login - effectively logging user out
	;login r index {user ""} {password ""}: performs login from code, given at least account index of user to log in.

	== Forms ==
	Login requires several predefined forms, and these can be overriden with a ''forms'' configuration variable whose value is some or all of the forms in a dictionary.

	;forms login: is returned by /form to allow login.  It must at least provide the ''user'' and ''password'' variables.
	;forms logout: is returned by /form to allow logout.  It should allow the user to invoke the /logout url.
	;forms new: is used by /new, when a new account is to be created.  It may collect and deliver any variable/values to /new, all of which will be stored with the user.  It must at least provide the ''user'' and ''password'' variables.
	;forms logmsg: is used to indicate errors and successes.

	== Example ==
	This code illustrates how Login can be used to control the domain /cookie/

	   package require Login
	   package require Direct

	   # create a Login object called ::L which uses the account view
	   # to store user account data.
	   # it will commit to the account db immediately upon each modification
	   # it will service the /cookie/ domain, enabling any url handler under /cookie/
	   # access to the account db
	   Login ::L account {
	       db accountdb file account.db layout {user:S password:S args:S}
	   } cpath /cookie/ jQ 1 autocommit 1

	   # construct a Direct Login domain under /login
	   # it uses the account file specified, and generates cookies for /cookie/ domain
	   Nub domain /login/ Direct object ::L ctype x-text/html-fragment
	
	   # this is a test page for Login.  It will permit you to log in with a new account,
	   # (invoking /new for collection of account information) and will display the user
	   # account information recorded in the database for user.
	   Nub code /login/test {
	       set r [::L /form $r]	;# the result of this nub will be a login or logout form
	       set user [::L user $r]	;# get the account record of the user (if any)
	
	       # add some output to the field content
	       set result [dict get $r -content]
	       append result [<div> id message {}]	;# add a message div for feedback
	       append result [<p> "User: $user"]	;# display the account data
	   }

	=== Referenced in Examples ===
	;[http:Nub domain]: a command which construct a nub, mapping a URL-glob onto a domain handler (in this case, Coco.)
	;<*>: commands of this form are defined by the [http:../Utility/Html Html] generator package and the [http:../Utility/Form Form] generator package.
	
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
    jQ {boolean - use jQuery javascript for interactions? (default: yes.)  Depends on [http:JQ jQ module]}
}

package require Debug
Debug define login 10

package require md5
package require Direct
package require View
package require Cookies

package provide Login 1.0

class create ::Login {
    variable account realm properties cookie age emptypass userF passF cpath domain forms keys jQ permissive autocommit new

    # fetch account record of logged-in user
    method user {r {user ""}} {
	if {$user eq ""} {
	    # don't know which user - use the cookie
	    set cdict [dict get? $r -cookies]

	    # determine the right domain/path for the cookie
	    set cd [list -name $cookie]
	    if {[info exists domain] && $domain ne ""} {
		dict set cd -domain $domain
	    }

	    Debug.login {logged in user in $cdict}

	    # fetch the cookie
	    if {![catch {Cookies fetch $cdict -name $cookie} cl]} {
		set key [dict get $cl -value]	;# cookie contains session key
		if {[info exists keys($key)]} {
		    # keys($key) contains the account db index
		    set index [dict get $keys($key) ""]
		    Debug.login {logged in user is $index}
		} else {
		    Debug.login {bogus key: $key}
		    set r [my clear $r]
		    return ""
		}
	    } {
		# there's no logged-in user here
		Debug.login {no user logged in under cookie '$cookie'}
		return ""
	    }
	} else {
	    # passed in a user key - search for it
	    Debug.login {user for '$user'}
	    if {[catch {$account find $userF $user} index]} {
		Debug.login {no such user '$user'}
		return ""
	    }
	}

	# found the index for this user record

	if {[catch {$account get $index} record eo]} {
	    Debug.login {can't read user's record $index in $account - $record ($eo)}
	    return ""
	} else {
	    # found the user's record
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
	if {[dict exists $record args]} {
	    set record [dict merge [dict get $record args] $record]	;# merge args field into record
	    dict unset record args
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

	if {![dict size $record]} {
	    return ""
	}

	set index [dict get $record ""]; dict unset record ""
	catch {dict unset args ""}	;# remove the index

	dict for {n v} $args {
	    if {$n eq "args"} continue
	    if {$n in $properties} {
		dict set record $n $v
		dict unset args $n
	    }
	}

	# store surplus variables in args, if it exists
	if {"args" in $properties} {
	    dict set record args [dict merge [dict get? $record args] $args]
	}

	set record [dict merge $record $args]
	$account set $index {*}$record
	if {$autocommit} {
	    $account db commit
	}

	return $record
    }

    # store some data in the user's record at client request
    method /set {r args} {
	catch {[dict unset args $userF]}	;# want only logged-in user
	# (also, we don't want the user to change its name from a form.)
	set record [my set $r {*}$args]

	if {![dict size $record]} {
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

    # clear the login cookie
    method clear {r} {
	set cdict [dict get? $r -cookies]
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

    # return a login or logout form
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
		set r [my clear $r]	;# clear the bogus cookie
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

	    # user not already logged in - return a login form
	    Debug.login {/form: not logged in}
	    return [Http Ok $r [dict get $forms login]]
	}
    }

    # send the client to a page indicating the failure of their login
    method logmsg {r {message "Login Failed"} {url ""}} {
	if {$jQ} {
	    # we're using jQuery forms
	    if {0} {
		set r [jQ postscript $r {
		    $('input[title!=""]').hint();
		    $('.login').ajaxForm({target:'#message'});
		}]
	    }
	    return [Http Ok $r [subst [dict get $forms logmsg]]]
	} else {
	    if {$url eq ""} {
		set url [Http Referer $r]
		if {$url eq ""} {
		    set url "http://[dict get $r host]/"
		}
	    }
	    # throw up a Forbidden form page.
	    return [Http Forbidden $r [subst [dict get $forms logmsg]]]
	}
    }

    # create new user
    method new {r args} {
	# ensure the new record is minimally compliant
	if {![dict exists $args $userF] || [dict get $args $userF] == 0} {
	    return -1	;# we refuse to allow blank users
	}
	if {$emptypass && ![dict exists $args $passF] || [dict get $args $passF] == 0} {
	    return -1	;# we refuse to allow blank users
	}

	set record [my user $args]
	if {$record ne ""} {
	    # the user must be unique
	    return -1
	}

	# create a new account record
	set user [dict get $args $userF]
	set password [dict get $args $passF]
	set index [$account append $userF $user $passF $password]
	my set $r {*}$args	;# store the rest of the data

	return $index
    }

    # create new user and log them in
    method /new {r {submit 0} args} {
	if {$submit} {
	    # We have a form POST to create a new user
	    set index [my new $r {*}$args]
	    if {$index != -1} {
		my login $r $index	;# log in the new user
		return [my logmsg $r "New user '[dict get $args $userF]' created"]
	    } else {
		return [my logmsg $r "There's already a user '[dict get $args $userF]'"]
	    }
	} else {
	    # We need to throw up a new user form.
	    if {$jQ} {
		set r [jQ form $r .login target '#message']
		set r [jQ hint $r]	;# style up the form
	    }
	    set user [dict get? $args $userF]
	    set password [dict get? $args $passF]
	    return [Http Ok $r [string map [list %USER $user %PASSWORD $password] [dict get $forms new]]]
	}
    }

    # perform the login of user at $index
    method login {r index} {
	# fetch user details for $index'th user
	lassign [$account get $index $userF $passF] user password

	set cdict [dict get? $r -cookies]

	# construct a session record keyed by md5
	set key [::md5::md5 -hex "[clock microseconds]$user$password"]
	if {[info exists keys($key)]} {
	    set key [::md5::md5 -hex "[clock microseconds]$user$password"]
	    # it's got to be a unique key
	}
	set keys($key) [list user $user password $password "" $index]
	Debug.login {login: created key $key}

	set cd [list -name $cookie -value $key]	;# cookie dict
	
	# include an optional expiry age for the cookie
	if {[info exists age] && $age} {
	    dict set cd -expires $age
	}

	# determine the right domain/path for the cookie
	if {[info exists domain] && $domain ne ""} {
	    dict set cd -domain $domain
	}
	
	# add in the cookies for each cookie path
	foreach cp $cpath {
	    set cdict [Cookies add $cdict {*}$cd -path $cp]
	}
	dict set r -cookies $cdict
	Debug.login {login: added cookie $cookie to $cdict}

	return $r
    }

    method / {r args} {
	return [/login $r {*}$args]
    }
    
    # login from a form - construct user record if necessary
    method /login {r args} {
	set r [Http NoCache $r]
	if {$jQ} {
	    set r [jQ form $r .login target '#message']
	    set r [jQ hint $r]	;# style up the form
	}

	# expect vars user and password, accept url
	set user [dict get? $args $userF]
	set password [dict get? $args $passF]
	set url [dict get? $args url]

	Debug.login {/login: user:$user password:$password url:$url}

	# prelim check on args
	if {$user eq ""} {
	    return [my logmsg $r "Blank username not permitted." $url]
	}
	if {!$emptypass && $password eq ""} {
	    return [my logmsg $r "Blank password not permitted." $url]
	}

	# find matching user in account
	if {[catch {$account find $userF $user} index]} {
	    # there's no existing user with this name.
	    if {$permissive && $user ne "" && $password ne ""} {
		# permissive - create a new user with the name and password given
		Debug.login {/login: permissively creating user}
		set index [$account append $userF $user $passF $password]
	    } else {
		Debug.login {/login: no such user}
		if {$new eq ""} {
		    # no $new url has been given for user creation, nothing to be done.
		    return [my logmsg $r "There is no such user as '$user'." $url]
		} else {
		    # redirect to $new URL for collecting account information
		    # the URL can decide to grant an account using /new
		    return [Http Redirect $r $new?$userF=$user&$passF=$password "User '$user' doesn't exist.  Create that user."]
		}
	    }
	}

	# match account password
	if {[$account get $index $passF] ne $password
	    && ("old_$passF" ni $properties
		|| [$account get $index old_$passF] ne $password)	   
	} {
	    Debug.login {/login: passwords don't match}
	    return [my logmsg $r "Password doesn't match for '$user'." $url]
	}

	set r [my login $r $index]
	
	if {$jQ} {
	    # assume the .form plugin is handling this.
	    return [Http Ok $r [<message> "User $user Logged in."]]
	} else {
	    # otherwise we redirect to the page which provoked the login
	    if {$url eq ""} {
		set url [Http Referer $r]
		if {$url eq ""} {
		    set url "http://[dict get $r host]/"
		}
	    }
	    # resume at the appropriate URL
	    return [Http NoCache [Http SeeOther $r $url "Logged in as $user"]]
	}
    }

    constructor {args} {
	Debug.login {constructing $args}
	set userF user		;# user field in $account view
	set passF password	;# password field in $account view
	set cookie login	;# name of the cookie
	set domain ""		;# domain for cookies
	set cpath ""		;# list of paths for cookies
	set permissive 0	;# allow anonymous creation?
	set emptypass 0		;# permit blank passwords?
	set account {user:S password:S}	;# minimal layout for account
	set jQ 1		;# use jQ by default
	set realm "Login [self]"	;# login for Basic AUTH
	set autocommit 1	;# commit on each write
	set new "new"		;# url to redirect to on new user creation request

	variable {*}[Site var? Login]	;# allow .ini file to modify defaults
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
	if {![dict exists $forms logmsg]} {
	    dict set forms logmsg {[<message> "$message [<a> href $url {Go Back.}]"]}
	}
	if {![dict exists $forms new]} {
	    dict set forms new [<form> newuser action new class login [<fieldset> [subst {
		[<legend> "Create User"]
		[<text> $userF title "user id" label "User Id: " "%USER"]
		[<text> $passF title "password" label "Password: " "%PASSWORD"]
		[<br>][<text> given title "given name" label "Given: " ""]
		[<text> surname title "surname" label "Surname: " ""]
		[<br>][<submit> submit value 1]
	    }]]]
	}

	# create the account database
	if {[llength $account] > 1} {
	    if {[llength $account]%2} {
		# we've been handed a named View constructor expression
		set account [View create {*}$account]
	    } else {
		# we've been handed an anonymous View constructor expression
		set account [View new {*}$account]
	    }
	} else {
	    # caller already has a view which it has passed in by name
	}
	
	set properties [$account properties]	;# remember the account properties
	
	# the /login path must always get this cookie
	if {[lsearch $cpath $mount] == -1} {
	    lappend cpath $mount
	}

	# ensure that we can find the user field
	if {![info exists userF] || $userF eq ""} {
	    if {[info exists cookie] && $cookie ne ""} {
		# unspecified a user field, use the cookie name
		set userF $cookie
	    } else {
		# unspecified user field and cookie, use the first property
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
    Nub domain /login/ Direct object {Login ::L account {db accountdb file account.db layout {user:S password:S args:S}} cpath /cookie/ permissive 0 jQ 1 autocommit 1} ctype x-text/html-fragment
    
    Nub code /login/test {
	set r [::L /form $r]	;# the result of this nub will be a login or logout form
	set user [::L user $r]	;# get the account record of the user (if any)
	
	# add some output to the field content
	set result [dict get $r -content]
	append result [<div> id message {}]	;# add a message div for feedback
	append result [<p> "User: $user"]	;# display the account data
    }
}
