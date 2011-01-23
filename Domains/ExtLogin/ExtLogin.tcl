# ExtLogin.tcl - a domain for authentication against external sites

package require TclOO
namespace import oo::*

package require Debug
Debug define ExtLogin 10

package require Cookies
package require HTTP
package provide ExtLogin 1.0

set ::API(Domains/ExtLogin) {
    {External Login domain. More docs to come.
    }
}

class create ::ExtLogin {

    method / {r} {
	return [Http sysPage $r Sorry [<p> {No content here yet.}]]
    }

    method getstatus r {
	set l [Cookies Match $r -name token]
	if {[llength $l]} {
	    set cookie [dict get? [Cookies Fetch $r -name token] -value]
	    if {[dict exists $cookiemap $cookie]} {
		return LOGGEDIN
	    } else {
		return NOTLOGGEDIN
	    }
	} else {
	    return NOTAUTHENTICATED
	}
    }

    method getuserid r {
	set cookie [dict get? [Cookies Fetch $r -name token] -value]
	return [dict get? $cookiemap $cookie]
    }

    method callback {r op args} {
	switch $op {
	    login {
		foreach {k v} $args {
		    set $k $v
		}
		set id [list $username $domain]
		dict set cookiemap $cookie $id
		dict set users $id $args
		return [Http Redirect $r [string trimright $mount /]/login Redirect]
	    }
	}
    }

    method authenticated r {
	return [expr {[my getstatus $r] ne "NOTAUTHENTICATED"}]
    }

    method loggedin r {
	return [expr {[my getstatus $r] eq "LOGGEDIN"}]
    }

    method /logout {r} {
        set l [Cookies Match $r -name token]
        if {[llength $l]} {
            set cookie [dict get? [Cookies Fetch $r -name token] -value]
            dict set r set-cookie "token=$cookie; expires=Tue, 01-Jan-1980 01:01:00 GMT; domain=[dict get $r -host]; path=/"
            if {[dict exists $cookiemap $cookie]} {
                dict unset cookiemap $cookie
                return [Http Ok+ $r "Logged out" text/html]
            } else {
                return [Http Ok+ $r "Not logged in." text/html]
            }
        } else {
            return [Http Ok+ $r "Not logged in." text/html]
        }
    }

    method /login {r} {
	if {[my loggedin $r]} {
	    #
	    set user [dict get $users [my getuserid $r]]
	    return [Http Ok+ $r $style[<div> class login "You have successfully authenticated yourself as [<span> style "padding-left: 20px; background: url([dict get $providersd [dict get $user provider] image-url]) no-repeat;" [dict get $user username]]"]]
	} else {
	    #
	    set urls {}
	    set names {}
	    set images {}
	    set zencode {div.login>label"Login using:"+ul.providers>li.provider*>a{href [lindex $urls $_]}>span{style "background:\ url([lindex $images $_])\ no-repeat\;" $names}}
	    foreach {n p} $providersd {
		lappend names [dict get $p name]
		lappend urls [string trimright $mount /]/providers/[dict get $p name]/authenticate
		lappend images [dict get $p image-url]
	    }
	    dict lappend r -headers $style
	    return [Http Ok+ $r [$zen generate HTML $zencode names $names urls $urls images $images]]
	}
    }

    method /providers {r} {
	lassign [split [dict get $r -suffix] /] - provider action
	if {$action ni {authenticate login getinfo}} {
	    return [Http sysPage $r {Server error} [<p> {Wrong method}]]
	}
	set args {}
	if {[my authenticated $r]} {
	    lappend args cookie [dict get? [Cookies Fetch $r -name token] -value]
	}
	return [[dict get $providersd $provider object] $action $r {*}$args]
    }

    method set-user args {
	dict set users {*}$args
    }

    method get-user args {
	dict get $users {*}$args
    }

    mixin Direct
    variable mount zen users cookiemap records forms providers providersd style

    constructor {args} {
	set mount ""
	set users ""
	set cookiemap ""
	set style [<style> {
	    .login {
		font-family: sans-serif;
		width: 18em;
		border: 1px solid lightgrey;
		padding: 0.7em;
	    }
	    .providers {
		padding-left: 1.2em;
		padding-right: 1.2em;
	    }
	    .provider {
		list-style-type: none;
		border: 1px solid grey;
		text-align: center;
	    }
	    .provider a {
		border: 0;
		display: block;
		text-decoration: none;
	    }
	    .provider a span {
		padding-left: 20px;
	    }
	}]
	set zen [Zen new]
	variable {*}[Site var? ExtLogin]	;# allow .ini file to modify defaults

	foreach {n v} $args {
	    set $n $v
	}

	foreach {name p} $providers {
		package require ExtLogin::$name
		set object [::ExtLogin::$name new mount $mount callback [list [self] callback] {*}$p]
		dict set providersd $name [$object info]
		dict set providersd $name object $object
	}
    }
}


