# Store - a simple wrapper around TDBC providing a store
#
# A 'store' is considered to be a database with fairly simple
# access/update requirements:
#
# Store presumes we're primarily interested in one table of a db, and that
# all tables can meaningfully be accessed by row number / oid.
#
# Store LRU-caches tdbc-prepared statements for all DB interactions
# this saves time re-preparing statements which might not change
#
# Store provides get/set/incr for individual fields by record oid
#
# Store provides for record matching/fetching/updating/deleting
# using a match alist, passed in args, whose terms are ANDed together.
#
# Match alists comprise pairs of words, similar to a dict, but with repetition
# permitted.  The name part of each pair may be a simple field name, in
# which case it represents a match for equality with the named field.
#
# If the name part of a match alist is suffixed with **, * or %, it will be
# treated as a REGEXP, GLOB or LIKE field comparison, respectively.
#
# If the name part of a match alist is suffixed with ?, then it will be
# interpreted as "IS NOT NULL" or "IS NULL" depending on the truth value
# of its associated value, so fred? 0 will succceed if the field fred is null.
#
# The name part may also be suffixed with >=, <=, >, <, != or = to indicate
# the respective SQL comparison operator.
#
# Despite its focus on simple matches and single table access,
# Store exports [db], [stmt] and [stmtL], providing unfettered access
# to the underlying database.  The stmt commands provide caching.

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
	Debug.store {stmtL '$stmt' over ($args)}
	set result [[my prep $stmt] allrows -as lists $args]
	Debug.store {stmtL result: '$stmt' over ($args) -> ($result)}
	return $result
    }

    method selector {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	set sel {}; set selargs {}; set count 0
	foreach {n v} $args {
	    set op ""
	    if {$n eq ""} {
		error "empty selector"
	    }
	    if {[string is alnum -fail fail $n]} {
		set op =	;# pure alpha selector
	    } else {
		set op [string range $n $fail end]
		set n [string range $n 0 $fail-1]

		switch -- $op {
		    ** {
			set op REGEXP
		    }

		    * {
			set op GLOB
		    }

		    % {
			set op LIKE
		    }

		    ? {
			set op IS
		    }
		    >= - <= - > - < -
		    == - != - = {}

		    default {
			set op =
		    }
		}
	    }

	    if {$op eq "IS"} {
		if {$v} {
		    lappend sel "$n IS NOT NULL"
		} else {
		    lappend sel "$n IS NULL"
		}
	    } else {
		lappend sel "$n $op :${n}_$count"
		dict set selargs ${n}_$count $v
		incr count
	    }
	}

	set sel [join $sel " AND "]
	return [list $sel $selargs]
    }

    # update matching tuples with the given dict
    method update {selector args} {
	Debug.store {update: $selector ($args)}
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

	# parse selector into SQL and selector args
	lassign [my selector $selector] sel selargs

	set updates {}
	foreach n [dict keys $args] {
	    if {$n ni {id}} {
		lappend updates "$n=:$n"
	    }
	}

	return [my stmt "UPDATE $table SET [join $updates ,] WHERE $sel;" {*}$args {*}$selargs]
    }

    # replace matching tuples with the given dict
    method replace {selector args} {
	Debug.store {update: $selector ($args)}
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

	# parse selector into SQL and selector args
	lassign [my selector $selector] sel selargs

	set updates {}
	foreach n [dict keys $args] {
	    if {$n ni {id}} {
		lappend updates "$n=:$n"
	    }
	}

	return [my stmt "REPLACE INTO $table ([join $updates ,]) WHERE $sel;" {*}$args {*}$selargs]
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

	# parse selector into SQL and selector args
	lassign [my selector $args] sel selargs

	return [my stmt "SELECT * FROM $table WHERE $sel$orderby;" {*}$selargs]
    }

    method all {{table ""}} {
	if {$table eq ""} {
	    variable primary; set table $primary
	}
	set orderby [join [lassign [split $table ,] table] ,]
	if {$table eq ""} {
	    variable primary; set table $primary
	}
	if {$orderby ne ""} {
	    set orderby " ORDER BY $orderby"
	}
	return [my stmt "SELECT * FROM $table $orderby;"]
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

	# parse selector into SQL and selector args
	lassign [my selector $args] sel selargs

	return [lindex [my stmtL "SELECT OID FROM $table WHERE $sel$orderby;" {*}$selargs] 0]
    }

    method count {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	if {[llength $args]%2} {
	    set args [lassign $args table]
	    set orderby [join [lassign [split $table ,] table] ,]
	    if {$table eq ""} {
		variable primary; set table $primary
	    }
	} else {
	    variable primary; set table $primary
	}
	if {$table eq ""} {
	    error "must specify primary table on creation, or explicitly mention table in call"
	}

	Debug.store {exists '$table' $args}

	# parse selector into SQL and selector args
	lassign [my selector $args] sel selargs

	return [llength [my stmtL "SELECT OID FROM $table WHERE $sel;" {*}$selargs]]
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

	# parse selector into SQL and selector args
	lassign [my selector $args] sel selargs

	return [my stmt "DELETE FROM $table WHERE $sel;" {*}$selargs]
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
    method getf {selector args} {
	return [dict get [my fetch $selector] {*}$args]
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
        if {[llength $args]} {
            return [dict get $result {*}$args]
        } else {
            return $result
        }
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
	variable schemafile ""	;# file containing schema
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
	    if {$schema eq "" && $schemafile ne ""} {
		set fd [open $schemafile r]
		set schema [read $fd]
		close $fd
	    }
	    if {$schema eq ""} {
		error "Must provide a schema,schemafile or an initialized db"
	    } else {
		Debug.store {schema: $schema}
		my db allrows -- $schema
	    }
	}
	catch {next? {*}$args}
    }
}


if {[info exists argv0] && ($argv0 eq [info script])} {
    package require tcltest
    namespace import ::tcltest::*
    proc Debug.store {msg} {puts stderr "STORE: [uplevel [list subst $msg]]"}

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
				 phone TEXT,
				 busy INTEGER DEFAULT 0,
				 nada TEXT
				 );
	    CREATE UNIQUE INDEX name ON phonebook(name);
	}
    } -result ::store

    set records {{name Colin phone 0296590404}
	{name Santa phone 0123456789}
	{phone 0123456789 name Santa2}
    }

    # create some records with append
    set count 0
    foreach el $records {
	incr count
	test store-append$count {Append record to Store} -body {
	    store append {*}$el
	} -result $count
    }

    # compare record sought with record fetched
    proc cmprec {id r1 r2} {
	dict for {n v} $r2 {
	    if {$n eq "id"} {
		if {$id ne "" && $v != $id} {
		    error "id doesn't match"
		}
	    }  elseif {[dict exists $r1 $n] && [dict get $r1 $n] ne $v} {
		error "field $n doesn't match"
	    }
	}
	return $id
    }

    # fetch records by row number
    set count 0
    foreach el $records {
	incr count
	test store-get$count {Append record to Store} -body {
	    # compare record to what we stored
	    cmprec $count $el [store get $count]
	} -result $count
    }

    # fetch records by name match
    set count 0
    foreach el $records {
	incr count
	test store-match$count {match records by one field} -body {
	    # fetch record matching name, compare it
	    cmprec $count $el [store fetch name [dict get $el name]]
	} -result $count
    }

    # fetch matching records
    test store-match$count {match records by phone field} -body {
	dict unset el name
	set count 0
	# count and records whose phone matches 0123456789
	foreach rec [store match phone 0123456789] {
	    cmprec "" [list phone 0123456789] $rec
	    incr count
	}
	return $count
    } -result 2

    # test NULL predicate
    test store-null {change records} -body {
	return [llength [store match nada? 0]]
    } -result [llength $records]

    # test record changing
    test store-change {change records} -body {
	set count 0
	store update {phone 0123456789} busy 1
	foreach rec [store match busy 1] {
	    cmprec "" [list phone 0123456789] $rec
	    incr count
	}
	return $count
    } -result 2

    # test record deletion
    test store-delete {change records} -body {
	set count 0
	store delete busy 1
	foreach rec [store match busy 1] {
	    cmprec "" [list phone 0123456789] $rec
	    incr count
	}
	return $count
    } -result 0

    # these are facilities we don't support
    set count 0
    foreach {from to} {
    } {
	incr count
	test store-$count {} -body {} -result $to
    }

    store destroy
}
