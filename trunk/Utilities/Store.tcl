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
    method prep {stmt} {
	variable stmts	;# here are some statements we prepared earlier
	if {[dict exists $stmts $stmt]} {
	    set s [dict get $stmts $stmt]
	} else {
	    set s [my db prepare $stmt]
	    dict set stmts $stmt $s
	}
	return $s
    }

    # stmt - evaluate tdbc statement
    # caches prepared statements
    # returns resultset as dict
    method stmt {stmt args} {
	Debug.store {stmt '$stmt' over ($args)}
	set result [[my prep $stmt] allrows -as dicts $args]
	Debug.store {stmt result: '$stmt' -> ($result)}
	return $result
    }

    # stmtL - evaluate tdbc statement
    # caches prepared statements
    # returns resultset as list
    method stmtL {stmt args} {
	set result [[my prep $stmt] allrows -as lists $args]
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
	return [my stmt "UPDATE $table SET [join $updates ,] WHERE id=:id;" {*}$args]
    }

    # append a tuple, return its id
    method append {args} {
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
	    lappend vs :$n
	}
	Debug.store {append $table ($args)}
	return [my stmtL "INSERT INTO $table ([join [dict keys $args] ,]) VALUES ([join $vs ,]);SELECT last_insert_rowid();" {*}$args]
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

	Debug.store {match in '$table' $args}

	set sel {}
	foreach n [dict keys $args] {
	    lappend sel $n=:$n
	}
	set sel [join $sel " AND "]
	return [my stmt "SELECT * FROM $table WHERE $sel;" {*}$args]
    }

    # find rowid of first match
    method find {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	if {[llength $args]%2} {
	    set args [lassign $args table]
	} else {
	    variable primary; set table $primary
	}

	Debug.store {find in '$table' $args}

	set sel {}
	foreach n [dict keys $args] {
	    lappend sel $n=:$n
	}
	set sel [join $sel " AND "]
	return [lindex [my stmtL "SELECT OID FROM $table WHERE $sel;" {*}$args] 0]
    }

    # delete matching tuples
    method delete {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	if {[llength $args]%2} {
	    set args [lassign $args table]
	} else {
	    variable primary; set table $primary
	}

	Debug.store {delete from '$table' $args}

	set sel {}
	foreach n [dict keys $args] {
	    lappend sel $n=:$n
	}
	set sel [join $sel " AND "]
	return [my stmt "DELETE FROM $table WHERE $sel;" {*}$args]
    }

    # return only one tuple by field match
    method fetch {args} {
	Debug.store {fetch $args}
	set result [my match {*}$args]
	if {[llength $result] > 1} {
	    error "one $name '$value' returned [llength $result] results: ($result)"
	}
	return [lindex $result 0]
    }

    method row {table row args} {
	if {![llength $args]} {
	    Debug.store {row $table $row}
	    return [lindex [my stmt "SELECT * FROM $table where OID=:oid" oid $row] 0]
	} else {
	    set cols [join $args ,]
	    Debug.store {row $table $row of $cols}
	    return [dict values [lindex [my stmt "SELECT $cols FROM $table where OID=:oid" oid $row] 0]]
	}
    }

    # return tuples by a single field match
    method by {name value {table ""}} {
	if {$table eq ""} {
	    variable primary; set table $primary
	}
	Debug.store {by name:$name value:$value table:$table}
	return [my stmt "SELECT * FROM $table WHERE $name=:value;" value $value]
    }

    method get {index args} {
	lassign [split $index .] table id
	if {$id eq ""} {
	    set id $table
	    variable primary; set table $primary
	}
	Debug.store {get id:$id from table:$table}
	set result [lindex [my stmt "SELECT * FROM $table WHERE id=:id;" id $id] 0]
	Debug.store {get id:$id from table:$table -> ($result) after $args}
	return [dict get $result {*}$args]
    }

    # set a row's values
    method set {index args} {
	lassign [split $index .] table id
	if {$id eq ""} {
	    set id $table
	    variable primary; set table $primary
	}
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}

	Debug.store {[self] set $index keys: '[dict keys $args]'}
	if {[dict size $args]} {
	    set updates {}
	    foreach n [dict keys $args] {
		if {$n ni {id}} {
		    lappend updates "$n=:$n"
		}
	    }
	    return [my stmt "UPDATE $table SET [join $updates ,] WHERE id=:id;" id $id {*}$args]
	}
    }

    # incr a field as an integer
    method incr {index field {qty 1}} {
	Debug.store {incr $index $field $qty}
	set val [expr {[my get $index $field] + $qty}]
	my set $index $field $val
	Debug.store {incr $index $field $qty -> $val}
	return $val
    }

    # maxid of given table/primary
    method maxid {{table ""}} {
	if {$table eq ""} {
	    variable primary; set table $primary
	}
	set result [my stmtL "SELECT MAX(id) FROM $table;"]
	if {$result eq "{{}}"} {
	    return 0
	} else {
	    return $result
	}
    }

    # associate [self] with the lifetime of a given object
    method as {tracer} {
	upvar 1 $tracer tracevar
	set tracevar [self]
	trace add variable tracevar unset [list [self] destroy]
	return [self]
    }

    destructor {
	catch {my db close}
	catch {my db destroy}
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
		Debug.store {creating db: tdbc::${tdbc}::connection create [namespace current]::dbI $file $opts}
		file mkdir [file dirname $file]
		tdbc::${tdbc}::connection create [namespace current]::dbI $file {*}$opts
		oo::objdefine [self] forward db [namespace current]::dbI
	    }
	} else {
	    Debug.store {provided db: '$db'}
	    oo::objdefine [self] forward db {*}$db
	}

	if {[my db tables] eq ""
	    || ($primary ne "" && $primary ni [my db tables])
	} {
	    # we don't have any tables - apply schema
	    if {$schema eq ""} {
		error "Must provide a schema or an initialized db"
	    } else {
		my db allrows $schema
	    }
	}
    }
}
