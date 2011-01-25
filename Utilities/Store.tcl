# Store - a simple wrapper around TDBC providing a store
#
# Store cached prepared statements for all DB interactions
# It presumes we're mainly interested in one table of a db, and that
# all tables can meaningfully be accessed by row number / oid.
#
# Store provides get/set for individual fields in numbered rows
#
# Store provides for matching/fetching of record sets using
# dicts passed in args.

package require tdbc
if {[catch {package require Debug}]} {
    #proc Debug.store {msg} {puts stderr "STORE: $msg"}
    proc Debug.store {msg} {}
} else {
    Debug define store 10
}

package provide Store 1.0

oo::class create Store {
    # prep - prepare a stmt or reused an already cached stmt
    method prep {stmt} {
	variable stmts	;# here are some statements we prepared earlier
	variable maxcache
	if {[dict exists $stmts $stmt]} {
	    set s [dict get $stmts $stmt]
	    if {$maxcache > 0} {
		# move matched element to end of cache (for LRU)
		dict unset stmts $stmt
		dict set stmts $stmt $s
	    }
	} else {
	    set s [my db prepare $stmt]
	    dict set stmts $stmt $s
	    if {$maxcache > 0 && [dict size $stmts] > $maxcache} {
		Debug.store {removing LRU cached statement}
		set stmts [lrange $stmts 2 end]
	    }
	}
	return $s
    }

    # stmt - evaluate (possibly cached) tdbc statement
    # returns resultset as dict
    method stmt {stmt args} {
	Debug.store {stmt '$stmt' over ($args)}
	set result [[my prep $stmt] allrows -as dicts $args]
	Debug.store {stmt result: '$stmt' -> ($result)}
	return $result
    }

    # stmtL - evaluate (possibly cached) tdbc statement
    # returns resultset as list
    method stmtL {stmt args} {
	set result [[my prep $stmt] allrows -as lists $args]
	Debug.store {stmtL result: '$stmt' -> ($result)}
	return $result
    }

    # change a given tuple (by id) to the given dict
    method change {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	if {[llength $args]%2} {
	    set args [lassign $args table]
	} else {
	    variable primary; set table $primary
	}
	if {$table eq ""} {
	    error "must specify primary table on creation, or explicitly mention table in call"
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

    # append a tuple, returning its id
    method append {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	if {[llength $args]%2} {
	    set args [lassign $args table]
	    set orderby [join [lassign [split $table ,] table] ,]
	    if {$table eq ""} {
		variable primary; set table $primary
	    }
	    if {$orderby ne ""} {
		set orderby "ORDER BY $orderby"
	    }
	} else {
	    variable primary; set table $primary
	    set orderby ""
	}
	if {$table eq ""} {
	    error "must specify primary table on creation, or explicitly mention table in call"
	}

	set vs {}
	dict for {n v} $args {
	    lappend vs :$n
	}
	Debug.store {append $table ($args)}
	return [my stmtL "INSERT INTO $table ([join [dict keys $args] ,]) VALUES ([join $vs ,]);SELECT last_insert_rowid();" {*}$args]
    }

    # match - return a list of all tuples matching the args dict
    method match {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	if {[llength $args]%2} {
	    set args [lassign $args table]
	    set orderby [join [lassign [split $table ,] table] ,]
	    if {$table eq ""} {
		variable primary; set table $primary
	    }
	    if {$orderby ne ""} {
		set orderby " ORDER BY $orderby"
	    }
	} else {
	    variable primary; set table $primary
	    set orderby ""
	}
	if {$table eq ""} {
	    error "must specify primary table on creation, or explicitly mention table in call"
	}

	Debug.store {match in '$table$orderby' $args}

	set sel {}
	foreach n [dict keys $args] {
	    lappend sel $n=:$n
	}
	set sel [join $sel " AND "]
	return [my stmt "SELECT * FROM $table WHERE $sel$orderby;" {*}$args]
    }

    # find rowid of first match
    method find {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	if {[llength $args]%2} {
	    set args [lassign $args table]
	    set orderby [join [lassign [split $table ,] table] ,]
	    if {$table eq ""} {
		variable primary; set table $primary
	    }
	    if {$orderby ne ""} {
		set orderby "ORDER BY $orderby"
	    }
	} else {
	    variable primary; set table $primary
	    set orderby ""
	}
	if {$table eq ""} {
	    error "must specify primary table on creation, or explicitly mention table in call"
	}

	Debug.store {find in '$table$orderby' $args}

	set sel {}
	foreach n [dict keys $args] {
	    lappend sel $n=:$n
	}
	set sel [join $sel " AND "]
	return [lindex [my stmtL "SELECT OID FROM $table WHERE $sel$orderby;" {*}$args] 0]
    }

    # delete tuples matching the args dict
    method delete {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	if {[llength $args]%2} {
	    set args [lassign $args table]
	} else {
	    variable primary; set table $primary
	}
	if {$table eq ""} {
	    error "must specify primary table on creation, or explicitly mention table in call"
	}

	Debug.store {delete from '$table' $args}

	set sel {}
	foreach n [dict keys $args] {
	    lappend sel $n=:$n
	}
	set sel [join $sel " AND "]
	return [my stmt "DELETE FROM $table WHERE $sel;" {*}$args]
    }

    # fetch first tuple matching the dict passed in $args
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

    # by - return all tuples matching the single field given by name and value
    method by {name value {table ""}} {
	if {$table eq ""} {
	    variable primary; set table $primary
	    set orderby ""
	} else {
	    set orderby [join [lassign [split $table ,] table] ,]
	    if {$table eq ""} {
		variable primary; set table $primary
	    }
	    if {$orderby ne ""} {
		set orderby "ORDER BY $orderby"
	    }
	}
	if {$table eq ""} {
	    error "must specify primary table on creation, or explicitly mention table in call"
	}
	Debug.store {by name:$name value:$value table:$table$orderby}
	return [my stmt "SELECT * FROM $table WHERE $name=:value$orderby;" value $value]
    }

    # get the named field (or its subcomponent as dict) given by args
    method get {index args} {
	lassign [split $index .] table id
	if {$table eq ""} {
	    variable primary; set table $primary
	} elseif {$id eq ""} {
	    set id $table
	    variable primary; set table $primary
	}
	if {$table eq ""} {
	    error "must specify primary table on creation, or explicitly mention table in call"
	}
	Debug.store {get id:$id from table:$table}
	set result [lindex [my stmt "SELECT * FROM $table WHERE id=:id;" id $id] 0]
	Debug.store {get id:$id from table:$table -> ($result) after $args}
	return [dict get $result {*}$args]
    }

    # set a row's values to those given in args dict
    # table may be specified as $table.$index for the 'index' arg
    method set {index args} {
	lassign [split $index .] table id
	if {$table eq ""} {
	    variable primary; set table $primary
	} elseif {$id eq ""} {
	    set id $table
	    variable primary; set table $primary
	}
	if {$table eq ""} {
	    error "must specify primary table on creation, or explicitly mention table in call"
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
	if {$table eq ""} {
	    error "must specify primary table on creation, or explicitly mention table in call"
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

    # constructor
    #
    # tdbc - which tdbc driver? (default: sqlite3)
    # db - an already-opened db
    # file - db file to create or use
    # schema - schema to create a db in $file
    # opts - to pass to tdbc for creation
    #
    # primary - primary table of interest in store
    # maxcache - maximum size of prepared statement cache (default unlimited)
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
	variable maxcache 0	;# limit size of cached stmts
	# load the tdbc drivers
	package require $tdbc
	package require tdbc::$tdbc
	
	if {$db eq ""} {
	    if {$file eq ""} {
		error "Must provide a db file"
	    } else {
		set ons [info object namespace [self]]
		Debug.store {creating db: tdbc::${tdbc}::connection create ${ons}::dbI $file $opts}
		file mkdir [file dirname $file]
		tdbc::${tdbc}::connection create ${ons}::dbI $file {*}$opts
		oo::objdefine [self] forward db ${ons}::dbI
	    }
	} else {
	    Debug.store {provided db: '$db'}
	    oo::objdefine [self] forward db {*}$db
	}

	if {[my db tables] eq ""
	    || ($primary ne "" && [string tolower $primary] ni [my db tables])
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


if {[info exists argv0] && ($argv0 eq [info script])} {
    package require tcltest
    namespace import ::tcltest::*
    #proc Debug.store {msg} {puts stderr "STORE: [uplevel [list subst $msg]]"}

    tcltest::skip unsupported-*
    set dbfile /tmp/storetest.db
    if {[file exists $dbfile]} {
	file delete $dbfile
    }

    test store-init {Create a Store} -body {
	Store create store file $dbfile primary phonebook schema {
	    PRAGMA foreign_keys = on;
	    CREATE TABLE phonebook (
				 id INTEGER PRIMARY KEY AUTOINCREMENT,
				 name TEXT UNIQUE NOT NULL COLLATE NOCASE,
				 phone TEXT
				 );
	    CREATE UNIQUE INDEX name ON phonebook(name);
	}
    } -result ::store

    set records {{name Colin phone 0296590404}
	{name Santa phone 1234567890}
	{phone 1234567890 name Santa2}
    }

    # create some records with append
    set count 0
    foreach el $records {
	incr count
	test store-append$count {Append record to Store} -body [list store append {*}$el] -result $count
    }

    # fetch records by row number
    set count 0
    foreach el $records {
	incr count
	test store-get$count {Append record to Store} -body {
	    # compare record to what we stored
	    dict for {n v} [store get $count] {
		if {$n eq "id"} {
		    if {$v != $count} {
			error "id doesn't match"
		    }
		}  elseif {[dict get $el $n] ne $v} {
		    error "field $n doesn't match"
		}
	    }
	    return 1
	} -result 1
    }

    # fetch records by name match
    set count 0
    foreach el $records {
	incr count
	test store-match$count {match records by one field} -body {
	    # compare record to what we stored
	    dict for {n v} [store fetch name [dict get $el name]] {
		if {$n eq "id"} {
		    if {$v != $count} {
			error "id doesn't match"
		    }
		}  elseif {[dict get $el $n] ne $v} {
		    error "field $n doesn't match"
		}
	    }
	    return $count
	} -result $count
    }

    # these are facilities we don't support
    set count 0
    foreach {from to} {
    } {
	incr count
	test store-$count {} -body [list zen parse $from]-result $to
    }

    store destroy
}
