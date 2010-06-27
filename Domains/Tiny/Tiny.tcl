# Tiny.tcl - a tinyurl-like facility
# usage: Nub domain /tiny/ Tiny file tiny.mk

package require Debug
Debug define tiny 10

package require View
package require Direct
package require jQ

package provide Tiny 1.0

set ::API(Plugins/Tiny) {
    {
	A tinyurl-alike for Wub
    }
}

class create ::Tiny {
    method created {r record} {
	set url [dict get $record url]
	set tiny http://[Url host $r][file join $mount [dict get $record tiny]]
	set content [<p> "Created [<a> href $tiny $tiny] -> [<a> href $url $url]"]
	return [Http Ok [Http NoCache $r] $content]
    }

    method /css {r} {
	set css {
	    * {zoom: 1.0;}

	    form.tiny {
		width: 80%;
		text-align: left;
	    }

	    form.tiny > fieldset {
		width:0%;
		background: whitesmoke;
		-moz-border-radius-topleft:7px;
		-moz-border-radius-topright:7px;
		-moz-border-radius-bottomleft:7px;
		-moz-border-radius-bottomright:7px;
	    }

	    form.tiny > fieldset > legend {
		background: darkslategray;
		color: white;
		-moz-border-radius-topleft:5px;
		-moz-border-radius-topright:5px;
		-moz-border-radius-bottomleft:5px;
		-moz-border-radius-bottomright:5px;
	    }

	    input.blur {
		color:lightgray;
	    }
	}
	set r [Http NoCache $r]
	return [Http Ok $r $css text/css]
    }

    method /create {r {possible ""} {url ""} args} {
	Debug.tiny {create: possible $possible url $url $args}
	if {$url eq ""} {
	    # No url to transform - just generate a form and return it.
	    Debug.tiny {No url, generate form.  $possible '$args'}

	    # construct a form
	    set formargs [list class tiny action [file join $mount create] method post]
	    set content [<div> [subst {
		[<form> miniscurl {*}$formargs [subst {
		    [<fieldset> fs {
			[<legend> "MiniscUrl"]
			[<text> url title "URL to make miniscurl" $possible]
			[<div> id tinyresult {}]
		    }]
		}]]
	    }]]

	    set r [jQ hint $r]	;# add auto-hinting to the form element
	    set r [jQ form $r .tiny target '#tinyresult']	;# make the form AJAX

	    dict set r -style [file join $mount css] {}
	    return [Http Ok [Http NoCache $r] $content x-text/html-fragment]
	}
	#[<text> custom legend "Custom alias (optional):" {}]

	set durl [Url parse $url]	;# parse URL
	set url [Url uri $durl]		;# normalize URL
	set old [my view fetch url $url]	;# fetch old record for URL
	if {[dict size $old]} {
	    Debug.tiny {found old $old}
	    return [my created $r $old]	;# we already have a tiny for this URL
	} else {
	    set count [my counter incr 0 id]	;# generate new unique tiny from counter
	    set short [string trimleft [binary encode hex [binary format W $count]] 0]
	    my view append tiny $short url $url	;# record association tiny<->URL

	    Debug.tiny {created new [list url $url tiny $short]}
	    return [my created $r [list url $url tiny $short]] ;# inform the user
	}
    }

    # generate a reference
    method genref {r} {
	set r [jQ jquery $r]	;# load the jQ
	set r [jQ postscript $r [string map [list %TU% [file join $mount ref]] {
	    /* this is javascript which will be run only when jQuery is ready */
	    $('#genref').load('%TU%');
	}]]
	#expects [<div> id genref {}]
	return $r
    }

    # permalink - create permalinks on the fly
    # Nub domain /tiny/ {Tiny ::tiny} file tiny.mk
    # set r [::tiny genref $r]
    # <div> id genref {}
    method /permalink {r {text Permalink} args} {
	set url [Url uri [Url parse [Http Referer $r] 1]]	;# normalized referer
	Debug.tiny {/ref: $url}
	set durl [Url parse $url]

	if {![dict exists $durl -host]} {
	    dict set durl -host [dict get $r -host]
	    dict set durl -port [dict get $r -port]
	}
	Debug.tiny {[Url host $durl] [Url host $r] }

	if {$private && [Url host $durl] ne [Url host $r]} {
	    # don't allow refs to external domains
	    return [<p> "[Url host $r] does not support external Permalinks"]
	}

	set ref [my view fetch url $url]	;# try to load matching record
	if {[dict size $ref]} {
	    dict with ref {}	;# got a matching record
	} else {
	    # no record - create one on the fly
	    set count [my counter incr 0 id]	;# generate new unique tiny from counter
	    set tiny [string trimleft [binary encode hex [binary format W $count]] 0]
	    my view append tiny $tiny url $url	;# record association tiny<->URL
	}
	return [Http Ok [Http Cache $r "next year"] [<a> class permalink href $tiny $text]]
	#return [Http Ok [Http NoCache $r] [<a> href $tiny $text]]
    }

    # default URL process - this will catch /$tiny type URLs
    method / {r args} {
	set extra [string tolower [dict get? $r -extra]]
	Debug.tiny {ref: $extra}
	if {$extra eq ""} {
	    return [my /create $r [Http Referer $r]]
	}

	set ref [my view fetch tiny $extra]	;# try to load matching record
	if {[dict size $ref]} {
	    # got a matching tiny, redirect to URL
	    Debug.tiny {ref: redirecting '$extra' to [dict get $ref url]}
	    return [Http Relocated $r [dict get $ref url]]
	} else {
	    # no match, suggest the creation of a tiny with the referer
	    Debug.tiny {ref: redirecting '$extra' NOT FOUND}
	    return [my /create $r [Http Referer $r]]
	}
    }

    variable viewV counterV mount private
    mixin Direct
    constructor {args} {
	set db tiny
	set private 1

	variable {*}[Site var? Tiny]	;# allow .ini file to modify defaults

	# unpack the args as variables
	foreach {n v} $args {
	    variable $n $v
	}

	if {![info exists file]} {
	    # we have to have a db file
	    error "Must specify a file argument"
	}

	# create or open the tiny.urls view
	[View new file $file db $db name urls commit 1 layout {
	    tiny:S
	    url:S
	}] as viewV
	objdefine [self] forward view $viewV

	# create or open the tiny.counter view
	[View new file $file db $db name counter commit 1 layout {
	    id:I
	}] as counterV
	objdefine [self] forward counter $counterV

	# start the counter at 0 if this is new
	if {![my counter size]} {
	    my counter append id 0
	}
    }
}
