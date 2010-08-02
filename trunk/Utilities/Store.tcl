# Store - a simple wrapper around TDBC providing a store

package require tdbc
if {[catch {package require Debug}]} {
    #proc Debug.store {msg} {puts stderr "STORE: $msg"}
    proc Debug.store {msg} {}
} else {
    Debug define store 10
}

package provide Store 1.0

oo::class create Store {
    # stmt - evaluate tdbc statement
    # caches prepared statements
    # returns resultset as dict
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
    # returns resultset as list
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

    # change a given tuple by id
    method change {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	if {[llength $args]%2} {
	    set args [lassign $args table]
	} else {
	    variable primary; set table $primary
	}
	if {![dict exists $args id]} {
	    error "changeset must contain id"
	}

	set updates {}
	foreach n [dict keys $args] {
	    if {$n ni {id}} {
		lappend updates "$n=:$n"
	    }
	}
	return [my stmt "UPDATE $table SET [join $updates ,] WHERE id=:id" {*}$args]
    }

    # insert a tuple, return its id
    method insert {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	if {[llength $args]%2} {
	    set args [lassign $args table]
	} else {
	    variable primary; set table $primary
	}
	set vs {}
	dict for {n v} $args {
	    if {$n ne "id"} {
		lappend vs :$n
	    } else {
		dict unset dict $n
	    }
	}

	return [my stmt "INSERT INTO $table ([join [dict keys $args] ,]) VALUES ([join $vs,])" $args]
    }

    # matching tuples
    method match {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	if {[llength $args]%2} {
	    set args [lassign $args table]
	} else {
	    variable primary; set table $primary
	}

	set sel {}
	foreach n [dict keys $args] {
	    lappend sel $n=:$n
	}
	return [my stmt "SELECT * FROM $table WHERE ([join $sel ,])" $args]
    }

    # return tuples by a single field match
    method by {name value {table ""}} {
	if {$table eq ""} {
	    variable primary; set table $primary
	}
	return [my stmt "SELECT * FROM $table WHERE ($name=:value)" value $value]
    }

    # return only one tuple by a single field match
    method oneby {name value {table ""}} {
	if {$table eq ""} {
	    variable primary; set table $primary
	}
	set result [my by $table $name $value]
	if {[llength $result] > 1} {
	    error "one $name '$value' returned [llength $result] results: ($result)"
	}
	return [lindex $result 0]
    }

    # maxid of given table/primary
    method maxid {{table ""}} {
	if {$table eq ""} {
	    variable primary; set table $primary
	}
	set result [my stmtL "SELECT MAX(id) FROM $table"]
	if {$result eq "{{}}"} {
	    return 0
	} else {
	    return $result
	}
    }

    constructor {args} {
	Debug.store {Creating Store [self] $args}
	variable tdbc sqlite3	;# TDBC backend
	variable db ""		;# already open db
	variable file ""	;# or db file
	variable opts {}
	variable schema {}	;# schema for empty dbs
	variable primary ""	;# primary table of interest
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

	if {$primary ne ""
	    && $primary ni [db tables]
	} {
	    # we don't have any tables - apply schema
	    if {$schema eq ""} {
		error "Must provide a schema or an initialized db"
	    } else {
		db allrows $schema
	    }
	}
    }
}
