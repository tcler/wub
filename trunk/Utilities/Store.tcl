# Store - a simple wrapper around TDBC
package require tdbc
package require Debug
Debug define store 10

package provide Store 1.0

oo::class create Store {
    # stmt - evaluate tdbc statement
    # caches prepared statements
    method stmt {stmt args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}

	variable stmts	;# here are some statements we prepared earlier
	if {[dict exists $stmts $stmt]} {
	    set s [dict get $stmts $stmt]
	} else {
	    set s [db prepare $stmt]
	    dict set stmts $stmt $s
	}

	Debug.store {stmt '$stmt'}
	set result [$s allrows -as dicts $args]
	Debug.store {stmt result: '$stmt' -> ($result)}
	return $result
    }

    # stmtL - evaluate tdbc statement
    # caches prepared statements
    method stmtL {stmt args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}

	variable stmts	;# here are some statements we prepared earlier
	if {[dict exists $stmts $stmt]} {
	    set s [dict get $stmts $stmt]
	} else {
	    set s [db prepare $stmt]
	    dict set stmts $stmt $s
	}

	set result [$s allrows -as lists $args]
	Debug.store {stmtL result: '$stmt' -> ($result)}
	return $result
    }

    constructor {args} {
	Debug.store {Creating Store [self] $args}
	variable tdbc sqlite3	;# TDBC backend
	variable db ""		;# already open db
	variable file ""	;# or db file
	variable opts {}
	variable schema {}
	variable {*}$args
	variable stmts {}

	# load the tdbc drivers
	package require $tdbc
	package require tdbc::$tdbc
	
	if {$db eq ""} {
	    if {$file eq ""} {
		error "Must provide a db file"
	    } else {
		set db [tdbc::${tdbc}::connection create db $file {*}$opts]
	    }
	}
	oo::objdefine [self] forward db $db

	if {![llength [db tables tuples]]} {
	    # we don't have any tables - apply schema
	    if {$schema eq ""} {
		error "Must provide a schema or an initialized db"
	    } else {
		db allrows $schema
	    }
	}
    }
}
