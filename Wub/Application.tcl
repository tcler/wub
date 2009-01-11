#! /usr/bin/env tclsh

# Single Threaded Simplistic Site
lappend auto_path [pwd]	;# path to the Site.tcl file
namespace eval Site {
    #variable varnish {go 1 debug 10}
    variable varnish {}
    #variable cache {}
    variable home [file dirname [info script]]
    variable coro 0	;# deselect the coroutine frontend
}

package require Site
if {[catch {package require Session}]} {
    proc Session {args} {
	return [lindex $args end]
    }
}

###### Application Starts Here
set docroot [file normalize [file join $::Site::docroot .. docs]]

#### CGI domain
CGI init cgi root [file normalize [file dirname [info script]]]

#### Rest domain
package require Rest
Rest init mount /_r/

#### Coco domain
if {[info tclversion] >= 8.6} {
    package require Coco

    Coco init said {r {
	set r [yield [Http Ok+ [yield] [<form> said "[<text> stuff][<submit> ok]"]]]
	Query qvars [Query parse $r] stuff
	return [Http Ok+ [yield [Http Ok+ $r [<a> href . "click here"]]] [<p> "You said: $stuff"]]
    }}

    Coco init coco {r {
	set r [yield]	;# initially just redirect
	while {1} {
	    set content [<h1> "Coco - Coroutining"]
	    append content [<p> "You have called the coroutine [incr n] times."]
	    set r [yield [Http Ok [Http NoCache $r] $content]]
	}
    }}

    Coco init copf {r {
	set referer [Http Referer $r]	;# remember referer
	set r [yield]	;# initially just redirect

	set r [form $r {
	    [<h1> "Personal Information"]
	    %MESSAGE
	    [<form> info {
		[<fieldset> personal {
		    [<legend> [<submit> submit "Personal Information"]]
		    [<text> forename title "Forename" $forename]
		    [<text> surname title "Surname" $surname]
		    [<br>][<text> phone title "Phone number" $phone]
		}]
	    }]
	} forename {
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

}

#### Wub documentation directory
# This creates a Mason domain which responds to urls of the form /wub/*
# It will search through the ../docs/ directory for its files,
# calling tcl scripts named .before before processing any file,
# and scripts named .after after processing.
Mason wub -url /wub -root $docroot -auth .before -wrapper .after

#### install directories of static files
foreach {dom expiry} {
    css {tomorrow}
    images {next week}
    scripts {tomorrow}
    img {next week}
    html 0
} {
    # This is a File domain which serves simple files
    # from the eponymous directory.  Each file served has an expiry
    # date as indicated, which allows caching.
    #puts stderr "File: [file join $docroot $dom]"
    File create $dom -root [file join $docroot $dom] -expires $expiry
}
File create bindir -root [file join $docroot bin] -expires 0

#### Repo domain - file repository
#
package require Repo
Repo init repo repo $docroot tar 1 upload 1	;# make a repo from the docroot

#### Commenter is a tcl code comment formatter
# 'code' is a Direct domain over the Commenter package,
# which serves /code/* and presents tcl code comments in a summary form.
package require Commenter
Direct init code namespace ::Commenter prefix /code ctype "x-text/html-fragment"

#### Dub database toy
set dubfailed [catch {
    package require Dub
    Dub init prefix /dub/
    Direct init dub namespace ::Dub prefix /dub/ ctype "x-text/html-fragment"
}]

#### jQ - jQuery framework
package require jQ
jQ init prefix /jquery

#### Honeypot domain captures misbehaving bots
package require Honeypot
set hp [file join /tmp/ captcha]
catch {file mkdir $hp}
Honeypot init dir $hp honeypot /bzzzz captcha /captcha

#### robots: sent as robots.txt
#
variable robots {User-agent: *
    Disallow: /bzzzz
}

#### Icons domain
package require Icons
Icons init mount /icons/

#### initialize Tie 
package require Tie
Tie init

#### ram: RAM domain
#
package require RAM
Debug off RAM 10
# Declare a RAM domain, invoked by [ram], whose URL prefix is /ram/
RAM init ram /ram/

# Declares /ram/test.html which is an literal html-fragment including
# a <head> <style> element which includes /ram/test.css
ram set test.html "[<h1> Test][<p> {This is a test of RAM domain.  You should see it in RED.}]" content-type x-text/html-fragment -headers [list [<style> type text/css {@import url(/ram/test.css);}]]

# declares /ram/test.css is a CSS which colours the <body> red
ram set test.css "body {color: red} ;" content-type text/css

#### Widgets package
# provides a bunch of HTML/JS/CSS widget sets
package require Widget

#### Sinorca package
# provides a page-level conversion
package require Sinorca
Sinorca init
Convert Namespace ::Sinorca

#### Simplicio package
# provides a page-level conversion
package require Simplicio
Simplicio init

#### introspection: example of a direct domain
# Implemented as a Direct domain, used to introspect the server.
#
# Each proc within the ::Introspect namespace which has a name of the
# form /* is made available as a RESTful interface - it is called
# from the client with the arguments as named, and any extra args
# supplied as name/value pairs in args.

Direct init introspect namespace ::Introspect prefix /introspect/ ctype "x-text/html-fragment"

namespace eval ::Introspect {
    # sortable - include javascripts and CSS for sortable table.
    proc sortable {r} {
	foreach js {common css standardista-table-sorting} {
	    dict lappend r -headers [<script> type text/javascript src /$js.js {}]
	}
	dict lappend r -headers [<style> type text/css media all "@import url(/sorttable.css);"]
	return $r
    }

    # clear the cache
    proc /cclear {r args} {
	Cache clear
	return [Http Redir $r /]
    }

    # report on the cache
    proc /cache {r args} {
	set C [Html dict2table [Cache::2dict] {-url -stale -hits -unmod -ifmod -when -size}]
	return [Http NoCache [Http Ok [sortable $r] $C x-text/html-fragment]]
    }

    # report on currentlu blocked client sites
    proc /block {r args} {
	set C [Html dict2table [Block blockdict] {-site -when -why}]
	return [Http NoCache [Http Ok [sortable $r] $C x-text/html-fragment]]
    }

    proc no/state {r} {
	set state [Activity state]
	set result [<table> summary {} class sortable [subst {
	    [<thead> [<tr> [<th> [join {cid socket thread backend ip start end log} </th><th>]]]]
	    [<tbody> [Foreach row $state {
		[<tr> [<td> [join $row </td><td>]]]
	    }]]
	}]]
	
	set r [sortable $r]	;# include the sortable js
	
	return [Http NoCache [Http Ok $r $result]]
    }

    proc no/activity {r {L "current"} {F "html"} args} {
	# generate an activity page
	if {$L eq "log"} {
	    set act [Activity log]
	    set title "Activity Log"
	    set alt [<a> href ".?L=current" "Current Activity"]
	} else {
	    set act [Activity current]
	    set title "Current Activity"
	    set alt [<a> href ".?L=log" "Activity Log"]
	}

	switch -- $F {
	    csv {
		package require csv
		foreach a $act {
		    append result [::csv::joinlist $a] \n
		}
		dict set r content-type text/plain
	    }

	    html -
	    default {
		set table [<table> summary {} class sortable [subst {
		    [<thead> [<tr> [Foreach t [lindex $act 0] {
			[<th> [string totitle $t]]
		    }]]]
		    [<tbody> [Foreach a [lrange $act 1 end] {
			[<tr> class [If {[incr row] % 2} even else odd] \
			     [<td> [join $a </td>\n<td>]]]
		    }]]
		}]]
		set result "[<h1> $title]$table[<p> $alt]"
		
		set r [sortable $r]	;# include the sortable js
		dict set r content-type x-text/html-fragment
	    }
	}
	
	return [Http NoCache [Http Ok $r $result]]
    }

    namespace export -clear {*}
    namespace ensemble create -subcommands {}
}

#### initialize Session
Session init cpath /_s/

#### Responder's result handler - used to post-process the results
# of ::Responder::Incoming
proc ::Responder::post {r} {
    return [Session store [::Convert do $r]]
}

proc Responder::do {req} {
    switch -glob -- [dict get $req -path] {
	/ {
	    # redirect / to /wub
	    Http Redir $req "/wub"
	}

	/_s/* {
	    # handle the Session subdomain
	    #Debug on session 10
	    set rsp [Session do $req]
	    #Debug.session {Session API: [Dict get? $rsp -session]}
	    set rsp
	}

	/repo/* {
	    repo do $req
	}

	/said -
	/said/* {
	    said do $req
	}

	/coco/* {
	    coco do $req
	}

	/copf/* {
	    copf do $req
	}

	/tie/*/ {
	    Http Redir $req [string trimright [dict get $req -path] /]
	}

	/tie/* {
	    return [Tie do $req]
	}

	/suspend {
	    # this is supposed to suspend a request processing for
	    # a given connection.
	    variable suspend
	    set req [Http NoCache [Http Ok $req [<h1> "Resumed"] text/html]]
	    lappend suspend $req
	    puts stderr "Suspending: [dict get $req -transaction] ($req)"
	    return [Http Suspend $req]
	}

	/resume {
	    # this resumes a formerly suspended client's processing
	    variable suspend
	    foreach r $suspend {
		puts stderr "Resuming: [dict get $r -transaction] ($r)"
		Http Resume $r
	    }
	    set count [llength $suspend]
	    set suspend {}
	    return [Http NoCache [Http Ok $req [<h1> "Completed Resume of $count pages"]]]
	}

	/jquery/* -
	/jquery/ {
	    jQ do $req
	}

	/widget/* -
	/widget/ {
	    Widget do $req
	}

	/sinorca/* -
	/sinorca/ {
	    # demonstrates the Sinorca livery
	    Sinorca ram do $req
	}

	/simplicio/* -
	/simplicio/ {
	    # demonstrates the Simplicio icons
	    Simplicio do $req
	}

	/ram/* -
	/ram {
	    # invoke the RAM domain called [ram]
	    ::ram do $req
	}

	/*.php -
	/*.wmv -
	/*.exe -
	/cgi-bin/* {
	    # attempted access to non-existent files indicates a malicious bot
	    # block the originator by IP
	    Block block [dict get $req -ipaddr] "Bogus URL '[dict get $req -path]'"
	    Http Forbidden $req
	}

	/CGI/* {
	    return [cgi do $req]
	}

	/_r/* {
	    if {![Rest exists test]} {
		Rest emit $req {r {
		    Rest again $r	;# ensure this persists
		    set url [Rest emit $r {{r count} {
			# this is the action for the form,
			# it will be constructed anew for,
			# and applied on each form submit
			return [Http Ok $r [<p> "Count: $count"]]
		    }} -count 2]
		    return [Http Ok $r [<form> f action $url {
			[<text> count legend "Count:" value 10]
		    }]]
		}} -key test
	    }
	    Rest do $req
	}

	/wub -
	/wub/* {
	    # Wub documentation - via the ::wub Direct domain
	    ::wub do $req
	}

	/dub {
	    Http Redir $req "/dub/"
	}

	/dub/* {
	    # Dub metakit toy
	    ::dub do $req
	}

	/introspect/* {
	    ::introspect do $req
	}

	/code {
	    # redirect /code to point to 
	    Http Redir $req /code/
	}

	/code/* {
	    # Commenter source code comments
	    ::code do $req
	}

	/icons/ {
	    ::Icons dir $req
	}
	/icons/* {
	    ::Icons do $req
	}

	/*.jpg -
	/*.gif -
	/*.png -
	/favicon.ico {
	    # All image files will be found in the top level
	    # directory of the ::images domain.
	    ::images do [Url flatten $req]
	}

	/css/*.css -
	/*.css {
	    # silently redirect css files
	    dict set req -suffix [file tail [dict get $req -path]]
	    ::css do $req
	}

	/*.gz {
	    # silently redirect gz files
	    dict set req -suffix [file tail [dict get $req -path]]
	    ::bindir do $req
	}

	/robots.txt {
	    variable robots
	    Http Ok $req $robots text/plain
	}

	/*.js {
	    # silently redirect js files
	    dict set req -suffix [file tail [dict get $req -path]]
	    ::scripts do $req
	}

	default {
	    Http NotFound $req	;# by default we return NotFound
	}
    }
}

#### Incoming - indication of incoming request
#
# This proc is called with a fully parsed incoming request.
# It uses Responder to act like tcl's [switch] command,
# but wraps errors in an appropriate HTML response.

proc Incoming {req} {
    # fetch cookies
    #set req [Cookies 4Server $req]
    set req [Session fetch $req]
    #puts stderr $req

    if {[dict exists $req -session]} {
	# do something with existing session
    } else {
	# this will create a new session on request completion
	dict set req -session created [clock seconds]
    }

    # [Responder Incoming] provides a safe wrapper and switch-like
    # dispatcher for incoming requests.
    set rsp [Responder Process $req]
    #return [Session store $rsp]

    return $rsp	;# response generated by one branch of Responder
}

Debug on error 100
Debug on log 10
Debug on block 10

Debug off socket 10
Debug off http 2
Debug off cache 10
Debug off cookies 10
Debug on direct 10
Debug on dispatch 10
Debug off query 10
Debug off direct 10
Debug off convert 10
Debug off cookies 10
Debug off session 10
Debug off mason 10

# Start Site Server(s)
Site start
