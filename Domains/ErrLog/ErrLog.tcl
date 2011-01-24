# ErrLog - small repository of the errors Wub has thrown
package require Debug
Debug define errlog 10

package require Http
package require Html
package require Report

package provide ErrLog 1.0

oo::class create ::ErrLog {
    method add {r message eo} {
	variable log
	lappend log [clock clicks] [list [clock format [clock seconds]] $r $message $eo]
    }

    method /test {r} {
	error "This is an intentional error"
    }

    method /css {r} {
	variable css
	return [Http Ok [Http Cache $r] $css text/css]
    }

    method /fetch {r} {
	variable log
	set el [dict get $r -extra]
	set el [dict get? $log $el]
	lassign $el when er message eo
	lassign [Http ErrorPage $er $message $eo] content message title
	set r [Http title $r "$when: [dict get $er -ipaddr] $title"]
	set page [<h2> class pretty $title]
	append page \n [<div> id summary $message]
	append page \n [<div> id errorinfo $content]
	append page \n [tclarmour [Http dump $er]]

	return [Http Ok [Html style $r ../css] $page x-text/system]
    }

    method / {r} {
	variable log
	set r [Html style $r css]
	if {![dict size $log]} {
	    return [Http Ok $r [<h2> class pretty "No errors as at [clock format [clock seconds]]"]]
	}

	set summary {}
	dict for {t v} $log {
	    lassign $v when er message eo
	    set sline [list view [<a> href fetch/$t View]]
	    foreach {var val} [list when $when from [dict get? $er -ipaddr] url [dict get? $er -url] message $message] {
		lappend sline $var $val
	    }
	    dict set summary $t $sline
	}

	Debug.errlog {summary: '$summary'}
	variable params
	return [Http Ok $r [Report html $summary headers {view when from url message} {*}$params]]
    }

    mixin Direct
    constructor {args} {
	variable css {
	    * {zoom: 1.0;}
	    html * { padding:0; margin:0; }
	    body * { padding:10px 20px; }
	    body * * { padding:0; }
	    body { font:small sans-serif; }
	    body>div { border-bottom:1px solid #ddd; }
	    h1 { font-weight:normal; }
	    h2 { margin-bottom:.8em; }
	    h2 span { font-size:80%; color:#666; font-weight:normal; }
	    h3 { margin:1em 0 .5em 0; }
	    table { 
		border:1px solid #ccc; border-collapse: collapse; background:white; }
	    tbody td, tbody th { vertical-align:top; padding:2px 3px; }
	    thead th { 
		padding:1px 6px 1px 3px; background:#fefefe; text-align:left; 
		font-weight:normal; font-size:11px; border:1px solid #ddd; }
	    tbody th { text-align:right; color:#666; padding-right:.5em; }
	    table.errorinfo { margin:5px 0 2px 40px; }
	    table.errorinfo td, table.dict td { font-family:monospace; }
	    #summary { background: #ffc; }
	    #summary h2 { font-weight: normal; color: #666; }
	    #errorinfo { background:#eee; }
	    #details { background:#f6f6f6; padding-left:120px; }
	    #details h2, #details h3 { position:relative; margin-left:-100px; }
	    #details h3 { margin-bottom:-1em; }

	    h1.pretty, h2.pretty, h3.pretty, h4.pretty, h5.pretty, h6.pretty {
		background: darkslategray;
		color: whitesmoke;
		padding: 0.2em 0.5em;
		-moz-border-radius-topleft:7px;
		-moz-border-radius-topright:7px;
		-moz-border-radius-bottomleft:7px;
		-moz-border-radius-bottomright:7px;
	    }

	    table.pretty {
		margin: 1em 1em 1em 2em;
		background: whitesmoke;
		border-collapse: collapse;
	    }

	    table.pretty td {
		border: 1px silver solid;
		padding: 0.2em;
	    }

	    table.pretty th {
		border: 1px silver solid;
		padding: 0.2em;
		background: darkslategray;
		color: white;
		text-align: left;
		-moz-border-radius-topleft:7px;
		-moz-border-radius-topright:7px;
		-moz-border-radius-bottomleft:7px;
		-moz-border-radius-bottomright:7px;
	    }

	    table.pretty tr {
		background: gainsboro;
	    }
	    table.pretty caption {
		margin-left: inherit;
		margin-right: inherit;
		font-size: 150%;
	    }
	}

	variable params {
	    class pretty
	    sortable 1
	    evenodd 1
	}

	variable {*}[Site var? File]	;# allow .ini file to modify defaults
	variable {*}$args
	variable log {}
	next? {*}$args

	if {[llength [info class instances ::ErrLog]] > 1} {
	    error "Can't create two ErrLogs ([info class instances ::ErrLog])"
	}
	proc ::errLog {args} [string map [list %S% [self]] {
	    %S% {*}$args
	}]
    }
}
