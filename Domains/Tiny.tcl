# Tiny.tcl - a tinyurl-like facility
# usage: Nub domain /tiny/ Tiny file tiny.mk

package require Debug
Debug off tiny 10

package require View
package require Direct
package require jQ

package provide Tiny 1.0

set API(Plugins/Tiny) {
    {
	A tinyurl-alike for Wub
    }
}

class create ::Tiny {
    mixin Direct

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

	    set formargs [list class tiny action [file join $mount create] method post]
	    set content [<div> [subst {
		[<form> miniscurl {*}$formargs [subst {
		    [<fieldset> fs {
			[<legend> "MiniscUrl"]
			[<text> url title "URL to miniscurl" $possible]
			[<div> id tinyresult {}]
		    }]
		}]]
	    }]]

	    set r [jQ hint $r]
	    set r [jQ form $r .tiny target '#tinyresult']
	    set r [jQ postscript $r {
		$('input[title!=""]').hint();
	    }]

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

    # default URL process - this will catch /$tiny type URLs
    method / {r args} {
	set extra [string tolower [dict get? $r -extra]]
	Debug.tiny {ref: $extra}
	if {$extra eq ""} {
	    return [my /create $r [Http Referer $r]]
	}

	set ref [my view fetch url $extra]	;# try to load matching record
	if {[dict size $ref]} {
	    # got a matching tiny, redirect to URL
	    Debug.tiny {ref: redirecting $extra to [dict get $ref tiny]}
	    return [Http Relocated $r [dict get $ref tiny]]
	} else {
	    # no match, suggest the creation of a tiny with the referer
	    Debug.tiny {ref: redirecting $extra NOT FOUND}
	    return [my /create $r [Http Referer $r]]
	}
    }

    variable viewV counterV mount
    constructor {args} {

	# unpack the args as variables
	foreach {n v} $args {
	    variable $n $v
	}

	if {![info exists file]} {
	    # we have to have a db file
	    error "Must specify a file argument"
	}

	# create or open the tiny.urls view
	[View new file $file db tiny name urls commit 1 layout {
	    tiny:S
	    url:S
	}] as viewV
	objdefine [self] forward view $viewV

	# create or open the tiny.counter view
	[View new file $file db tiny name counter commit 1 layout {
	    id:I
	}] as counterV
	objdefine [self] forward counter $counterV

	# start the counter at 0 if this is new
	if {![my counter size]} {
	    my counter append id 0
	}
    }
}
