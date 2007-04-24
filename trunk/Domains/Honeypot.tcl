# Honeypot.tcl - provide a honeypot to catch bad spiders and link harvesters.

# The idea is that there's a single address on the site,
# strongly linked to from all over, but with no-follow and clearly marked
# in robots.txt and by visual cues (which are in any case hidden) which,
# if visited, marks the visitor as a potential problem.
#
# The Httpd server must know the identity of this link, and call [honeypot]
# to service requests for that link.  It must also check each request against
# [bot?] and treat a non-"" return value as the page to return to that client.
#
# Honeypot places a cookie, and marks and remembers the visitor's IP
# as a potential bot.  It generates a captcha, which must be answered
# by the bot before restrictions are lifted.
#
# If a bot persists in ignoring the captcha challenge, and continues to
# generate requests (to any entity on the server) it will eventually be
# blocked by [Httpd block]
#
# In the meantime, as part of the honeypot offering, random emailto:
# links will be generated, just to pollute the db of any link harvesters.

package require passgen
package require captcha
package require fileutil

package require Direct

package provide Honeypot 1.0

namespace eval Honeypot {
    variable dir ./images/captcha	;# directory for captchas
    variable length 5			;# length of captcha

    # initialize Honeypot state
    proc init {args} {
	foreach {n v} $args {
	    variable $n $v
	}

	# make sure there's a place to put the captchas
	catch {variable dir}
	catch {file mkdir $dir}
    }

    variable bots	;# array of potential bot ip addresses
    array set bots {}

    # bot? - determines and records whether a given address is a known bot.
    # adds data about bot to request, for worker handling
    # this is called from the parent thread before and after request processing
    # in order to fetch/store bot related data about the client's ipaddr.
    proc bot? {req} {
	set ipaddr [dict get $req -ipaddr]
	if {$ipaddr eq "127.0.0.1"
	    && [dict exists $req x-forwarded-for]
	} {
	    set ipaddr [lindex [split [dict get $req x-forwarded-for] ,] 0]
	}

	variable bots

	if {[dict exists $req -bot]} {
	    set b [dict get $req -bot]
	    if {$b eq ""} {
		unset bots($ipaddr)
	    } else {
		set bots($ipaddr) $b
	    }
	} elseif {[info exists bots($ipaddr)]} {
	    # this IP address is caught.
	    # We must pass the request to a thread for processing.
	    dict set req -bot $bots($ipaddr)
	}

	return $req	;# if we've signalled this is a bot, worker must handle
    }

    proc link {{where /honeypot}} {
	return "<p style='display:none'><a href='$where' rel='nofollow'>DO NOT CLICK THIS LINK.</a>  It is a trap for badly behaved bots.  If you follow this link, you will be unable to use the site afterwards.</p>"
    }

    # /captcha - an active domain which returns the cached captcha image
    proc /captcha {} {
	upvar 1 response r
	set r [Http NoCache $r]	;# ensure this is dynamic

	# cache the captcha in the client's space, why should we
	# pay for their lameness?
	#set r [Http Cache $r {next month} private]

	lassign [dict get $r -bot] captcha captchaf when
	dict set r content-type "image/jpg"
	return [::fileutil::cat -encoding binary -translation binary -eofchar "" -- $captchaf]
    }

    variable text
    proc poison {} {
	variable text
	if {![info exists text]} {
	    # we haven't primed the text
	}
	return ""
    }

    proc captcha {captchaf} {
	variable length
	set captcha [passgen Generate len $length punctuation,min 0 LETTERS "BCDEFGHJKLMNPTUWYZ" numbers "6789" letters "abcdefhijkmnpuwyz" punctuation "%"]

	# generate captcha image and remember its path
	::captcha $captcha file $captchaf size 360x240
	return $captcha
    }

    # /honeypot - an active domain which processes honeypot requests
    # this is called from within a worker thread and either generates
    # the -bot structure for parent/dispatcher thread to use, or uses
    # a -bot structure previously generated.
    #
    # /honeypot will inspect any release-captcha string handed in,
    # releasing the bot if it matches, otherwise it will return a
    # text explaining the trap, inviting release, and poisoning
    # address harvester dbs.

    proc /honeypot {{c ""} {redo 0}} {
	upvar 1 response r
	set r [Http NoCache $r]	;# ensure this is dynamic

	# cache the honeypot in the client's space, why should we
	# pay for their lameness?
	# TODO: if they ignore this, we should penalise them.
	#set r [Http Cache $r {next month} private]

	set ipaddr [dict get $r -ipaddr]

	variable dir
	set captchaf [file join $dir [string map {. _} $ipaddr].jpg]

	variable bots
	if {[dict exists $r -bot]} {
	    # already a designated bot - see if they've reacted
	    lassign [dict get $r -bot] captcha captchaf when
	    if {$c ne "" && [string toupper $c] eq [string toupper $captcha]} {
		# the user has successfully responded to captcha
		# we now release them from the blockage.
		dict set r -bot ""
		dict set r -bot_change 1

		catch {file delete $captchaf}
		variable released
		return [subst $released]
	    } elseif {$redo > 0 && $redo < 5} {
		set captcha [captcha $captchaf]
		dict set r -bot [list $captcha $captchaf $when]
		dict set r -bot_change 1
	    }
	} else {
	    # newly designated bot - they've supped at the honeypot.

	    # generate captcha image and remember its path
	    set captcha [captcha $captchaf]
	    set when [clock seconds]	;# remember when we caught 'em
	    dict set r -bot [list $captcha $captchaf $when]
	    dict set r -bot_change 1

	    # try to set a cookie, just to see if it's a real bot
	}

	# generate a bunch of plausible-looking emailto: links
	# to poison any address harvesters' dbs, to show we care.
	variable honeypot
	return [subst $honeypot]
    }

    # this is text subst'd and returned upon release
    variable released {<h1>Released from Honeypot</h1>
	<p>You should be free to use the site now.</p>
	<p>Please don't visit this page again.</p>
	<p>You may wish to continue with <a href='/4'>Recent Changes</a></p>
    }

    # this is text subst'd and returned upon entrapment
    variable honeypot {<h1>You're Stuck!</h1>
	<p>This site tries to detect and detain bad robot behavior.
	Unfortunately, if you are reading this, you have been caught.</p>
	<p>You have a limited number of attempts to free yourself from
	this trap, before we decide you are in fact a bot.</p>
	<p>To free yourself, you must type in the characters you see in
	the following image:</p>
	<p><a href='/_honeypot?redo=[incr redo]'><img src='/_captcha'/></a></p>
	<form action='/_honeypot' method='post>
	<fieldset>
	<legend>Captcha</legend>
	<label for='c'>Enter text from above image:</label> <input name='c' type='text'>
	</fieldset>
	</form>
	<p>If the image is unreadable, you can click on it to try another</p>
	<hr>
	<p>Now, for our entertainment, we generate a bunch of bogus
	email address links.  Hopefully harvesters will ingest these, and
	poison their DBs.</p>
	[poison]
	</hr>
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

# create a direct domain to handle the honeypot
Direct honeypot -namespace ::Honeypot -ctype "text/x-system"
