# Tiny.tcl - a tinyurl-like facility
# usage: Nub domain /tiny/ Tiny file tiny.mk

package require Debug
Debug off tiny 10

package require View
package require Direct

package provide Tiny 1.0

set API(Plugins/Tiny) {
    {
	A tinyurl-alike for Wub
    }
}

class create ::Tiny {
    mixin Direct

    method created {r record} {
	set full http://[Url host $r][file join $mount [dict get $record from]]
	return [Http Ok [Http NoCache $r] [<p> "Created [<a> href $full $full] -> [dict get $record to]"]]
    }

    method /create {r {possible ""} {url ""} args} {
	Debug.tiny {create: possible $possible url $url $args}
	if {$url eq ""} {
	    Debug.tiny {No url, generate form.  $possible '$args'}
	    return [Http Ok [Http NoCache $r] [<form> miniscurl action [file join $mount create] method post [subst {
		[<fieldset> fs {
		    [<legend> "MiniscUrl"]
		    [<text> url legend "URL to miniscurl:" $possible]
		    [<submit> submit "Miniscurl"]
		}]
	    }]] x-text/html-fragment]
	}
	#[<text> custom legend "Custom alias (optional):" {}]

	set durl [Url parse $url]
	set url [Url uri $durl]
	set old [my view fetch to $url]
	if {[dict size $old]} {
	    Debug.tiny {found old $old}
	    return [my created $r $old]
	} else {
	    set count [my counter incr 0 id]	;# get new counter
	    set short [string trimleft [binary encode hex [binary format W $count]] 0]
	    my view append from $short to $url
	    Debug.tiny {created new [list from $url to $short]}
	    return [my created $r [list from $url to $short]]
	}
    }

    method / {r args} {
	set extra [dict get? $r -extra]
	Debug.tiny {ref: $extra}
	if {$extra eq ""} {
	    return [my /create $r [Http Referer $r]]
	}

	set ref [my view fetch from $extra]
	if {[dict size $ref]} {
	    Debug.tiny {ref: redirecting $extra to [dict get $ref to]}
	    return [Http Redirect $r [dict get $ref to]]
	} else {
	    Debug.tiny {ref: redirecting $extra NOT FOUND}
	    return [my /create $r [Http Referer $r]]
	}
    }

    variable viewV counterV mount
    constructor {args} {
	foreach {n v} $args {
	    variable $n $v
	}
	if {![info exists file]} {
	    error "Must specify a file argument"
	}
	[View new file $file db tiny name urls commit 1 layout {
	    from:S
	    to:S
	}] as viewV
	objdefine [self] forward view $viewV

	[View new file $file db tiny name counter commit 1 layout {
	    id:I
	}] as counterV
	objdefine [self] forward counter $counterV
	if {![my counter size]} {
	    my counter append id 0
	}
    }
}
