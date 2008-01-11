#! /usr/bin/env tclsh

# Single Threaded Simplistic Site
lappend auto_path [pwd]	;# path to the Site.tcl file
package require Site

###### Application Starts Here
set docroot [file normalize [file join $::Site::docroot .. docs]]

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
    bin 0
} {
    # This is a File domain which serves simple files
    # from the eponymous directory.  Each file served has an expiry
    # date as indicated, which allows caching.
    puts stderr "File: [file join $docroot $dom]"
    File $dom -root [file join $docroot $dom] -expires $expiry
}

#### Commenter is a tcl code comment formatter
# 'code' is a Direct domain over the Commenter packeage,
# which serves /code/* and presents tcl code comments in a summary form.
package require Commenter
Direct init code namespace ::Commenter prefix /code ctype "x-text/html-fragment"

#### Dub database toy
if {0} {
    package require Dub
    Dub init prefix /_dub
    Direct init dub namespace ::Dub prefix /_dub ctype "x-text/html-fragment"
}

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

#### ram: RAM domain
#
package require RAM
Debug on RAM 10
# Declare a RAM domain, invoked by [ram], whose URL prefix is /ram/
RAM init ram /ram/

# Declares /ram/test.html which is an html-fragment test.html which includes
# a <head> <style> element which includes /ram/test.css
RAM set test.html "[<h1> Test][<p> {This is a test of RAM domain.  You should see it in RED.}]" content-type x-text/html-fragment -headers [list [<style> type text/css {@import url(/ram/test.css);}]]

# declares /ram/test.css is a CSS which colours the <body> red
RAM set test.css "{body {color: red} \;}" content-type text/css

#### introspection: example of a direct domain
# Implemented as a Direct domain, used to introspect the server.
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

    namespace export -clear {*}
    namespace ensemble create -subcommands {}
}

#### Incoming - indication of incoming request
#
# This proc is called with a fully parsed incoming request.
# It uses Responder to act like tcl's [switch] command,
# but wraps errors in an appropriate HTML response.
#
# Responder's result

proc Incoming {req} {

    # fetch cookies
    set req [Cookies 4Server $req]
    #puts stderr $req

    set rsp [Responder Incoming $req -glob -- [dict get $req -path] {
	/ {
	    Http Redir $req "/wub"
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
	    Send [Http Forbidden $req]
	    continue	;# process next request, this one's dealt with
	}

	/wub -
	/wub/* {
	    # Wub documentation - via the ::wub Direct domain
	    ::wub do $req
	}

	/dub -
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
	    ::bin do $req
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
    }]

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
Debug off dispatch 10
Debug off query 10
Debug off direct 10
Debug off convert 10
Debug off cookies 10

# Start Site Server(s)
Site start
