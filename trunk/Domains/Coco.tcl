# Coco.tcl - a domain built around coroutines from tcl8.6

package require Http
package require Debug
Debug define coco 10

package require md5

package provide Coco 1.0

set API(Domains/Coco) {
    {
	Tcl8.6 Coroutine domain.  Invoking the domain URL creates a coroutine with semantics given by a lambda (evaluated within the Coco namespace,) and associates this running coroutine with an automatically-generated URL.  The domain URL invocation, having generated its coroutine, redirects to it.

	Coco is like a [Direct] domain except the functional element is a coroutine not a namespace or object instance.  The coroutine created by invoking a Coco domain has a unique name which will persist until the coroutine exits or until the server restarts.

	Coroutines maintain their local variable state, so Coco may be used to maintain persistent state for as long as the coroutine exists.  It is possible, therefore, to use Coco as the basis of a session facility.

	The Cocoroutine is called with the request dict.  [[yield]] may return a response to that request, which will subsequently be returned to the client.  If [[yield]] is called without an argument, it returns an HTTP redirect to the coroutine's URL.

	== Coco Forms ==
	Since Coco provides lightweight session persistence, keyed by synthetic URL, it can be used to validate forms.  The [[Coco form]] command is invoked as ''[[Coco form $request $form [[list $fail_message $predicate]] ... ]]''.  Where $request is the current HTTP request dict, $form is an HTML form (with an optional %MESSAGE string embedded), and args are a validation dict.

	Validation dicts associate the name of a field in the form with a list comprising a message to be displayed if the validation fails, and a tcl validation expression or predicate.  The predicate may be anything acceptable to Tcl's [[expr]], and is expected to return a boolean value.  All form fields are available to each predicate as tcl variables of the same name.

	[[Coco form]] will cause the Coco coroutine to re-issue the form until all validation predicates evaluate true.

	=== Example: validating a form ===
	[[Coco form]] provides a form validation facility.  Once called, it will return the supplied form until all validation predicates are true.

	 Nub domain /copf/ Coco lambda {r {
	     set referer [Http Referer $r]	;# remember referer
	     set r [yield]	;# initially just redirect

	     # validate the supplied form against a dict of field/validators
	     set r [form $r [string map [list %REF $referer] {
		 [<h1> "Personal Information"]
		 [<p> "Referer: '%REF'"]
		 %MESSAGE
		 [<form> info {
		     [<fieldset> personal {
			 [<legend> [<submit> submit "Personal Information"]]
			 [<text> forename title "Forename" $forename]
			 [<text> surname title "Surname" $surname]
			 [<br>][<text> phone title "Phone number" $phone]
		     }]
		 }]
	     }] forename {
		 "Forename can't be empty."
		 {$forename ne ""}
	     } surname {
		 "Surname can't be empty."
		 {$surname ne ""}
	     } phone {
		 "Phone number has to look like a phone number."
		 {[regexp {^[-0-9+ ]+$} $phone]}
	     }]
	     # now all the variable/fields mentioned in [form] have valid values
	     
	     # resume where you were
	     return [Http Redirect $r $referer]
	 }}

	== Examples ==

	=== Simple interaction example ===
	This Cocoroutine returns a simple form, collects its response, echoes it to the client, and terminates.

	 Nub domain /said/ Coco lambda {r {
	     set r [yield [Http Ok+ [yield] [<form> said "[<text> stuff][<submit> ok]"]]]
	     Query qvars [Query parse $r] stuff	;# fetch $stuff from the submitted form
	     return [Http Ok+ [yield [Http Ok+ $r [<a> href . "click here"]]] [<p> "You said: $stuff"]]
	     # this coroutine is a one-shot - as it returns, the coroutine will disappear
	 }}

	=== Example: Counting calls ===
	The following just counts calls to the synthetic URL

	 Nub domain /coco/ Coco lambda {r {
	     set r [yield]	;# initially just redirect to this coroutine
	     while {1} {
		 # this coroutine loops around counting calls in $n
		 set content [<h1> "Coco - Coroutining"]
		 append content [<p> "You have called the coroutine [incr n] times."]
		 set r [yield [Http Ok [Http NoCache $r] $content]]
	     }
	 }}

	=== Referenced in Examples ===
	;[http:Nub domain]: a command which construct a nub, mapping a URL-glob onto a domain handler (in this case, Coco.)
	;[http:../Utility/Http Http]: a module to transform request dicts into response dicts suitable for returning to the client.  In summary, [[Http Ok]] generates a normal HTTP Ok response, [[Http Redirect]] generates an HTTP Redirect response, and so on.
	;[http:../Utility/Query Query]: a module to parse and manipulate the GET-query or POST-entity components of a request.
	;<*>: commands of this form are defined by the [http:../Utility/Html Html] generator package and the [http:../Utility/Form Form] generator package.
    }
    lambda {+a lambda or procname (taking a single argument) which is invoked as a coroutine to process client input.  This lambda will be invoked in the context of the Coco-constructed coroutine.}
}

namespace eval Coco {
    proc <message> {message} {
	return [<p> class message [join $message "</p><p class='message'>"]]
    }

    proc form {_r form args} {
	set _vals {}
	foreach {var vv} $args {
	    set _value ""
	    catch {unset _validate}
	    if {[llength $vv] == 1} {
		dict set _vals $var [lindex $vv 0]
	    } else {
		lassign $vv _msg _validate _value
		dict set _vals $var $_value
		if {[info exists _validate]} {
		    set _validates($var) $_validate
		    set _messages($var) $_msg
		}
	    }
	}

	set _message [list {Enter Fields.}]
	while {[llength $_message]} {
	    set _r [jQ hint $_r]	;# add form hinting

	    # issue form
	    set _r [yield [Http Ok [Http NoCache $_r] [dict with _vals {
		subst [string map [list %MESSAGE [<message> $_message]] $form]
	    }] x-text/html-fragment]]

	    # unpack query response
	    set _Q [Query parse $_r]
	    dict set _r -Query $_Q
	    set _Q [Query flatten $_Q]

	    # fetch and validate fields
	    set _message {}
	    foreach _var [dict keys $_vals] {
		dict set _vals $_var [dict get? $_Q $_var]
		uplevel 1 set $_var [list [dict get? $_Q $_var]]
	    }
	    dict with _vals {}

	    Debug.coco {form vals: $_vals}
	    foreach _var [dict keys $_vals] {
		if {[info exists _validates($_var)]} {
		    if $_validates($_var) {
			dict set _r -values $_var [dict get $_vals $_var]
		    } else {
			lappend _message $_messages($_var)
		    }
		}
	    }
	}

	return $_r
    }

    variable uniq "[pid][clock microseconds]"	;# seed for unique coroutine names

    # give a uniq looking name
    proc uniq {} {
	variable uniq
	return [::md5::md5 -hex [incr uniq]]
    }

    # process request helper
    proc process {mount lambda r} {
	dict set r -rest [lassign [split [dict get? $r -suffix] /] suffix]
	Debug.coco {process '$suffix' over $mount}
	
	if {$suffix eq "/" || $suffix eq ""} {
	    # this is a new call - create the coroutine
	    set cmd [uniq]
	    dict set r -cmd $cmd
	    dict set r -csuffix $mount/$cmd

	    set result [coroutine @$cmd ::apply [list {*}$lambda ::Coco] $r]

	    if {$result ne ""} {
		Debug.coco {coroutine initialised - ($r) reply}
		return $result	;# allow coroutine lambda to reply
	    } else {
		# otherwise simply redirect to coroutine lambda
		Debug.coco {coroutine initialised - redirect to $mount/$cmd}
		return [Http Redirect $r $mount/$cmd/]
	    }
	}

	set extra [lassign [split $suffix /] cmd]
	dict set r -extra $extra

	if {[namespace which -command ::Coco::@$cmd] ne ""} {
	    # this is an existing coroutine - call it and return result
	    Debug.coco {calling coroutine '@$cmd' with extra '$extra'}
	    if {[catch {
		@$cmd [list call $r]
	    } result eo]} {
		Debug.error {coroutine error: $result ($eo)}
		return [Http ServerError $r $result $eo]
	    }
	    Debug.coco {coroutine yielded: ($result)}
	    return $result
	} else {
	    Debug.coco {coroutine gone: @$cmd}
	    return [Http NotFound $r [<p> "Coco '$cmd' has terminated."]]
	}
    }

    # process request wrapper
    proc _do {mount lambda r} {
	# calculate the suffix of the URL relative to $mount
	lassign [Url urlsuffix $r $mount] result r
	if {!$result} {
	    return $r	;# the URL isn't in our domain
	}

	set result [process $mount $lambda $r]
	if {[dict size $result]} {
	    return $result
	} else {
	    return [Http NotFound $r]
	}
    }

    # initialize ensemble for Coco
    proc new {args} {
	variable {*}[Site var? Coco]	;# allow .ini file to modify defaults
	return [create CoCo_[uniq] {*}$args]
    }

    proc create {cmd args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}

	variable {*}[Site var? Coco]	;# allow .ini file to modify defaults
	foreach {n v} $args {
	    variable $n $v
	}
	if {![info exists mount]} {
	    set mount /$cmd/
	}
	set cmd [uplevel 1 namespace current]::$cmd
	namespace ensemble create \
	    -command $cmd -subcommands {} \
	    -map [list do [list _do $mount $lambda]]
	return $cmd
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
