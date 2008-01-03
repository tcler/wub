#! /usr/bin/env tclsh

# Single Threaded Simplistic Site
lappend auto_path [pwd]	;# path to the Site.tcl file
package require Site

###### Application Starts Here
set docroot $::Site::docroot

#### Wub documentation directory
Mason wub -url /wub -root [file join $docroot .. docs] -auth .before -wrapper .after -dirhead {name size mtime}

#### install directories of static files
foreach {dom expiry} {css {tomorrow} images {next week} scripts {tomorrow} img {next week} html 0 bin 0} {
    File $dom -root [file join $docroot $dom] -expires $expiry
}

#### Commenter is a tcl code comment formatter
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
variable robots {User-agent: *
    Disallow: /bzzzz
}

#### introspection: example of a direct domain
# used to introspect the server
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

Direct init introspect namespace ::Introspect prefix /introspect/ ctype "x-text/html-fragment"

# Incoming - indication of incoming request
proc Incoming {req} {

    # fetch cookies
    set req [Cookies 4Server $req]

    Responder Incoming $req -glob -- [dict get $req -path] {
	/ {
	    Http Redir $req "/wub"
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
	    Http Redir $req "http://[dict get $req host]/code/"
	}

	/code/* {
	    # Commenter source code comments
	    ::code do $req
	}

	/*.jpg -
	/*.gif -
	/*.png -
	/favicon.ico {
	    # silently redirect image files - strip all but tail
	    dict set req -suffix [file tail [dict get $req -path]]
	    ::images do $req
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
	    Http NotFound $req
	}
    }
}

# Start Site Server(s)
Site start
