# Sql --
#
# A Wub domain to return contents of a sqlite db

# TODO: make the package return only a CSV-like format (with 1st line as header?)
# and provide CSV->HTML and CSV->Sylk as Convert domain conversions.
# - force this by interpreting .sylk and .csv suffixes as indicating client's willingness
# to accept these formats, and jam the relevant mime types at the front of the accept
# request field.
# one reason this isn't already done is that I didn't want to force dependence on Convert,
# also there's a lot of wriggle room in CSV->HTML conversion, and it's not clear
# that Convert has enough user customisability.

package require OO
package require Query
package require Mime
package require Debug
Debug off sql 10

package provide Sql 1.0

set API(Domains/Direct) {
    {
	A domain to return contents of a db
    }
}

namespace eval SqlConvert {
    # parameters handed to Report for html table generation
    variable params {
	sepchar ,
	delchar \"
	sortable 1
	evenodd 1
	class table
	tparam {title table}
	hclass header
	hparam {title column}
	thparam {class thead}
	thrparam {class thead}
	fclass footer
	tfparam {class tfoot}
	tfrparam {class tfoot}
	rclass row
	rparam {}
	eclass el
	eparam {}
	footer {}
    }
    proc params {args} {
	variable params
	set params [dict merge $params $args]
    }

    proc .text/x-tdbc.application/x-sylk {r} {
	package require Sylk

	variable params
	set p [dict merge $params [dict get? $r -params]]
	set sepchar [dict get $p $sepchar]
	set r [.text/x-tdbc.text/csv $r]
	set content [csv2sylk [dict get $r -content] $sepchar]

	return [dict merge $r [list -content $content content-type application/x-sylk]]
    }

    proc .text/x-tdbc.text/csv {r} {
	package require csv

	variable params
	set p [dict merge $params [dict get? $r -params]]
	set sepchar [dict get $p $sepchar]
	set delchar [dict get $p $delchar]

	set content ""
	foreach record [dict get $r -content] {
	    append content [::csv::joinlist [dict values $record] $sepchar $delchar] \n
	}
	set header #[::csv::joinlist [dict keys $record] $sepchar $delchar]\n

	return [dict merge $r [list -content $header$content content-type text/csv]]
    }

    proc .text/x-tdbc.text/html {r} {
	variable params
	set p [dict merge $params [dict get? $r -params]]

	set content {}
	set n 0
	foreach el [dict get $r -content] {
	    dict set content $n $el
	    incr n
	}

	return [dict merge $r [list -content [Report html $content {*}$p] content-type text/x-html-fragment]]
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

class create Sql {
    method selector {r vs} {
	# use query to determine fieldset
	set display_f {}
	set order_f {}
	set select_f {}
	set flags {}

	# get relevant field descriptors from the query
	set q [Query parse $r]
	foreach {n v m} [Query nvmlist $q] {
	    catch {unset meta}
	    array set meta $m
	    if {[info exists meta(unassigned)]} {
		if {[string match -* $n]} {
		    # flag from &-field&
		    dict set flags $n 1
		} elseif {[string match ^* $n]} {
		    # order from &^field&
		    lappend order_f $meta(-count) $n
		} else {
		    # display field indicated by &field&
		    dict set display_f $meta(-count) $n
		}
	    } elseif {[string match -* $n]} {
		# flag from &-field&
		dict set flags $n $v
	    } elseif {[string match ^* $n]} {
		# order from &^field&
		lappend order_f $meta(-count) $n

		# select clause element indicatd by &field=expr
		lappend select_f [string trim $n ^] $v
	    } else {
		# select clause element indicatd by &field=expr
		lappend select_f $n $v
	    }
	}

	# get display fields in query order
	set display {}
	foreach n [lsort -integer [dict keys display_f]] {
	    lappend display [dict get $display_f $n]
	}

	# get order fields in query order
	set order {}
	foreach n [lsort -integer [dict keys order_f]] {
	    lappend order [dict get $order_f $n]
	}

	Debug.sql {Sql query: display:($display) order:($order) flags:($flags) select:($select)}
	if {![llength $display]} {
	    set display *
	}

	set select "SELECT [join $display ,] FROM [join $vs ,]"

	# calculate the selector from select clause elements
	set selector {}
	foreach {n v} $select_f {
	    set plain [string trim $n "%@=<>!^"]
	    switch -glob -- $n {
		*% {
		    # like
		    lappend selector [list $plain LIKE '$v']
		}

		*@ {
		    # regexp
		    lappend selector [list $plain REGEXP '$v']
		}

		*[*] {
		    # glob
		    lappend selector [list $plain GLOB '$v']
		}

		*> {
		    lappend selector [list $plain > '$v']
		}

		*>= {
		    lappend selector [list $plain >= '$v']
		}

		*< {
		    lappend selector [list $plain < '$v']
		}

		*<= {
		    lappend selector [list $plain < '$v']
		}

		*! -
		*!= {
		    lappend selector [list $plain != '$v']
		}

		default {
		    lappend selector [list $n = '$v']
		}
	    }
	}

	set selector [join $selector " AND "]

	set selector [string trim $selector]
	if {$selector ne ""} {
	    append select " WHERE $selector"
	}

	if {$order ne {}} {
	    append select " ORDER BY [join $order ,]"
	}
	
	Debug.sql {select: $select}
	return $select
    }

    method /_tables {r {table {}}} {
	if {$table ne {}} {
	    foreach table [$db tables] {
		dict for {n v} [db columns $table] {
		    dict lappend result $table [<a> href _columns?table=$table&column=$n $n]
		}
	    }
	} else {
	    dict for {n v} [db columns $table] {
		dict lappend result [<a> href _columns?table=$table&column=$n $n] $v
	    }
	}
	return [Http Ok $r $result text/x-tdbc]
    }

    method / {r} {
	# calculate the suffix of the URL relative to $mount
	lassign [Url urlsuffix $r $mount] result r suffix path
	if {!$result} {
	    return $r	;# the URL isn't in our domain
	}

	# use suffix to determine which view
	lassign [split $suffix .] view ext
	Debug.sql {$view - $suffix}

	# determine which views must be joined
	set view [split $view /]
	if {$views ne {}} {
	    foreach v $view {
		if {$v ni $views} {
		    do respond NotFound $r
		}
	    }
	}

	# generate a select from the query
	set select [my selector $r $view]

	# run the select and generate list-of-dicts content
	if {[dict exists stmts($select)]} {
	    set stmt $stmts($select)
	} else {
	    set stmts($select) [set stmt [db prepare $select]]
	}
	set content [$stmt allrows -as dicts]

	# calculate the desired content-type
	set mime [Mime MimeOf [string tolower $ext]]
	if {$mime eq ""} {
	    set mime text/x-html-fragment
	}
	Debug.sql {desired content type: $mime}

	# generate and pre-convert the response
	set r [Http Ok $r $content text/x-tdbc]

	dict set r -params $params	;# send parameters to conversion
	return [Convert Convert! $r $mime]
    }

    variable db views csv tdbc local params stmts

    destructor {
	if {$local} {
	    $db close
	}
    }

    mixin Direct	;# use Direct to map urls to /methods

    constructor {args} {
	set db ""
	set file ""		;# db file
	set views {}		;# views which can be accessed by the domain
	set tdbc sqlite3	;# TDBC backend
	set params {}	;# parameters for Report in html table generation
	set stmts {}

	foreach {n v} $args {
	    set [string trimleft $n -] $v
	    Debug.direct {variable: $n $v}
	}

	# load the tdbc drivers
	package require $tdbc
	package require tdbc::$tdbc

	if {$db eq ""} {
	    # create a local db
	    set local 1
	    if {$file eq ""} {
		error "Sql must specify an open db or a file argument"
	    }
	    set db [self]_db
	    tdbc::${tdbc}::connection create $db $file 
	} else {
	    # use a supplied db
	    set local 0
	}
    }
}
