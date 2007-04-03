# Db.tcl -- a snit Metakit wrapper

package provide Db 1.0

package require snit
package require Mk4tcl
if {[info commands Debug] eq {}} {
    proc Debug.db {args} {}
    proc Debug.dblayout {args} {}
}

snit::type Db {
    option -db
    component mk
    delegate method * to mk using "mk::file %m %w"

    # Periodic commit
    option -flush 5000	;# Auto-Commit frequency
    variable timer ""		;# periodic commit timer

    # options to pass to metakit on creation
    option -readonly 0
    option -commit 1
    option -extend 0
    option -shared 0

    option -file ""

    method self {} {
	return $self
    }

    method view? {view} {
	return [expr {[View info instances ${self}.$view] ne {}}]
    }

    # layout/create a view in this db.
    method layout {view args} {
	if {[View info instances ${self}.$view] eq {}} {
	    View ${self}.$view
	}

	${self}.$view layout {*}$args
	return ${self}.$view
    }

    # flush changes to the metakit
    method periodicCommit {} {
	if {$timer eq ""} {
	    if {($options(-flush) <= 0)
		|| $options(-readonly)
		|| !$options(-commit)} {
		return	;# don't want commit
	    }

	    $self autocommit
	}
	$self commit

	set timer [after $options(-flush) [mymethod periodicCommit]]
    }

    constructor {args} {
	Debug.db {construct $self '$args'}
	set mk "mk::file"

	$self configurelist $args
	if {$options(-db) ne ""} {
	    set db $options(-db)
	} else {
	    set oargs {}
	    foreach v {-readonly -extend -shared} {
		if {$options($v)} {
		    lappend oargs $v
		}
	    }

	    if {!$options(-commit)} {
		if {!$options(-readonly)} {
		    lappend oargs -nocommit	;# we're doing our own commits
		}
	    } elseif {$options(-readonly)} {
		error "Can't have a readonly db with commit"
	    }
	    
	    if {$options(-file) eq ""} {
		# find an already opened db
		Debug.db {Opening: mk::file open $self $oargs}
		mk::file open $self {*}$oargs ;# in-core or shared metakit
	    } else {
		Debug.db {Opening: mk::file open $self $options(-file) $oargs}
		mk::file open $self $options(-file) {*}$oargs
		if {!$options(-readonly) && ($options(-flush) != 0)} {
		    $self periodicCommit
		}
	    }
	}
    }

    destructor {
	# close database itself
	if {$options(-commit)} {
	    catch {$self commit}	;# flush as requested
	}
	catch {$self close $self}
    }
}

::snit::type View {
    option -db -default ""	;# Db object containing this view
    option -type -default "raw"	;# type of view
    variable select_count 0	;# used to generate unique select views

    ;# parent views for derived/synthetic views
    option -parents -default {} -configuremethod setonce

    # metakit's view object
    option -view -default "" -configuremethod setonce
    method setonce {option value} {
	if {$options($option) ne ""} {
	    error "-view is a set-once option"
	}
	set options($option) $value
    }

    component view -public view	;# metakit's idea of our view
    delegate method * to view

    # mk::channel access
    method channel {index property mode} {
	return [mk::channel ${self}!$index $property $mode]
    }

    # construct a hash view over $self
    method hash {args} {
	set h [$options(-db) layout map_[$self name] {_H:I _R:I}]
	set hash [View hash<[$self name]> -parents $self \
		      -view [$view view hash $h {*}$args] \
		      -type hash \
		      -db $options(-db)]
	return $hash
    }

    # give this object a human readable name
    method name {} {
	if {$options(-type) eq "raw"} {
	    return [lindex [split $self .] end]
	} elseif {[llength $options(-parents)] == 1} {
	    return "$options(-type)<[$options(-parents) name]>"
	} else {
	    set result {}
	    foreach p $options(-parents) {
		lappend result [$p name]
	    }
	    return "$options(-type)<[join $result ,]>"
	}
    }

    # generate some methods for some dyadic view operators
    foreach m {concat different intersect
	join map minus pair
	product union} {
	eval [string map [list @M $m] {
	    method @M {v args} {
		return [View @M<[$self name],[$v name]> \
			    -view [$view view @M [$v cget -view] \
				       {*}$args] \
			    -parents [list $self $v] \
			    -type @M \
			    -db $options(-db)]
	    }
	}]
    }

    method where {select args} {
	set x [$self find {*}$select]
	$self set $x {*}$args
	return $x
    }

    # open a subview on this view
    method open {cursor prop} {
	Debug.db {OPEN: $self $cursor $prop - $view - [$view properties]}
	return [View subview<[$self name]/$cursor/$prop> \
		    -view [$view open $cursor $prop] \
		    -parents $self \
		    -type subview \
		    -db $options(-db)]
    }

    # convenience append method
    method append {args} {
	Debug.db {$self append $args}
	return [$self insert end {*}$args]
    }

    method incref {} {}
    method decref {} {}

    # tie this view to a single variable instance's lifetime.
    method as {var} {
	upvar $var v
	catch {unset v}
	set v $self
	trace add variable v unset [list $self destroy]
	return $self
    }

    method mselect {args} {
	set sv [$self select {*}$args]
	return [$self map $sv]
    }

    method select {args} {
	return [View select<[$self name]/[incr select_count]> \
		    -view [$view select {*}$args] \
		    -parents $self \
		    -type select \
		    -db $options(-db)]
    }

    # select over view, yielding a cursor list
    method rselect {args} {
	set s [$view select {*}$args]
	set result {}
	$s loop c {
	    set record [$s get $c]
	    if {[llength $record] == 2} {
		set record [$view get $record(index)]
	    }
	}
	Debug.db {lselect generated from $s over $args: $result}
	return $result
    }

    # select over view, yielding a cursor list
    method lselect {args} {
	set s [$view select {*}$args]
	set result {}
	$s loop c {
	    lappend result [$s get $c index]
	}
	Debug.db {lselect generated from $s over $args: $result}
	$s close
	return $result

	# construct a method-local select view
	set select ""
	[View %AUTO% -view [$view select {*}$args] \
	     -parents $self -type select -db $options(-db)] as select

	Debug.db {lselect generated over $select: [$select properties]}
	set result {}
	$select all x {
	    lappend result [dict get $x index]
	}

	Debug.db {lselect: $result}
	return $result
    }

    # for each element of the view,
    # set var to a dict containing the record and evaluate the body
    method all {var body} {
	upvar 1 $var v
	$self loop cursor {
	    Debug.db {ALL: $cursor [$self get $cursor]} 3
	    set v [dict create {*}[$self get $cursor] "" $cursor]
	    uplevel 1 $body
	}
    }

    method foreach {var clause body} {
	# construct a method-local select view
	set select ""
	[View %AUTO% -view [$view select {*}$clause] \
	     -parents $self -type select -db $options(-db)] as select

	upvar $var v
	$select loop cursor {
	    set cursor [$select get $cursor index]
	    set v [dict create {*}[$self get $cursor] "" $cursor]
	    uplevel $body
	}
    }

    # generate some monadic view methods
    foreach m {clone copy dup blocked
	readonly unique flatten indexed
	ordered project range rename
	restrict groupby
    } {
	eval [string map [list @M $m] {
	    method @M {args} {
		return [View @M<[$self name]> \
			    -view [$view view @M {*}$args] \
			    -parents $self \
			    -type @M \
			    -db $options(-db)]
	    }
	}]
    }

    method names {} {
	set result {}
	foreach f [split [$view properties]] {
	    lappend result [split $f :]
	}
	return $result
    }

    variable typesof

    method typeof {prop} {
	if {![info exists typesof]} {
	    Debug.db {$self properties: [$self properties]} 20
	    foreach n [$self properties] {
		lassign [split $n :] n v
		Debug.db {$self typeof: $n $v}
		set typesof($n) $v
	    }
	}
	return $typesof($prop)
    }

    # layout this view
    method layout {args} {
	if {$args eq ""} {
	    return [$view properties]
	}

	if {[llength $args] == 1} {
	    set args {*}$args
	}

	Debug.dblayout {LAYOUT: $args}

	set r ""
	foreach l [split $args \n] {
	    set l [string trim [lindex [regexp -inline {[^\#]*} $l] 0]]
	    if {$l ne {}} {
		Debug.dblayout {layout l-ing '$l'} 3
		append r $l " "
	    }
	}

	set pretty ""
	set accum ""
	#set r [string map [list \{\  \{ \ \} \}] $r]
	Debug.dblayout {layout r: $r} 2

	foreach el $r {
	    set el [string trim $el]
	    if {$el eq ""} continue
	    set el [string trim $el "\""]
	    lappend accum $el
	    Debug.dblayout {layout accumulating: '$accum'} 3
	    if {![info complete $accum]} {
		continue
	    }

	    if {[llength [lindex $accum 0]] == 1} {
		Debug.dblayout {layout name: '$el' / $accum} 3

		lassign [split $accum :] name t
		set accum ""
		
		if {$t eq ""} {
		    set t ""
		} else {
		    set t ":$t"
		}

		lappend pretty ${name}${t}
	    } else {
		Debug.dblayout {layout subview: '$el' / $accum} 3

		# this is a subview declaration
		set name $accum
		lappend pretty [lindex $accum 0]
		set accum ""
	    }
	}

	Debug.dblayout {LAYOUT: '$pretty'}
	::mk::view layout $self $pretty
	Debug.dblayout {LAYOUT $self: [::mk::view layout $self]}
    }

    variable recset
    method recset {} {
	array set recset {}
	foreach el [$view properties] {
	    if {[llength $el] == 1} {
		lassign [split $el :] name t
		set recset($name) $t
	    }
	}
    }

    method merge_idx {idx record} {
	if {![info exists recset]} {
	    $self recset
	}

	set setter {}
	foreach {n v} [array get recset] {
	    if {[dict exists $record $n]} {
		lappend setter $n [dict get $record $n]
	    }
	}
	$self set $idx {*}$setter
    }

    method create {record} {
	Debug.db {$self create $record}
	if {![info exists recset]} {
	    $self recset
	}

	set setter {}
	foreach {n v} [array get recset] {
	    if {[dict exists $record $n]} {
		lappend setter $n [dict get $record $n]
	    }
	}

	return [$self insert end {*}$setter]
    }

    method with {clause cmd args} {
	foreach idx [$self lselect {*}$clause] {
	    $self $cmd $idx {*}$args
	}
    }

    method merge {key record} {
	Debug.db {$self merge $key - ($record)}
	foreach p $options(-parents) {
	    Debug.db {$self Merging $p / $key - ($record)} 2
	    $p merge $key $val $record
	}

	# find key value in record
	if {[catch {
	    dict get $record $key
	} val]} {
	    return
	}

	if {![catch {
	    # locate the record with the key value
	    $self find $key $val
	} idx eo]} {
	    # found match
	    Debug.db {$self merging $key-$val-$idx - ($record)} 2
	    $self merge_idx $idx $record
	} else {
	    # no match - new record.
	    $self create $record 
	}
    }

    constructor {args} {
	$self configurelist $args
	Debug.db {construct $args}

	catch {unset recset}

	if {$options(-type) eq "raw"} {
	    # we have an objectname of form db.view
	    lassign [split $self .] options(-db) name
	    if {[Db info instances *$options(-db)] == ""} {
		error "Db $options(-db) doesn't exist in [Db info instances]"
	    }
	    
	    Debug.db {name: $name}
	    #if {$name in [$options(-db) views]}
	    set view [mk::view open $self]
	    set options(-view) $view
	} elseif {$options(-view) ne ""} {
	    set view $options(-view)
	}
	Debug.db {view: $view}
    }

    destructor {
	catch {$view close}
    }
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    # some test stuff

    proc Debug.db {args} {
	catch {puts stderr [uplevel $args]}
    }

    file delete test.db
    puts [Db db -file test.db]

    db layout phone {
	id:I
	name
	phone
	{phones {category phone}}
    }

    db layout address {
	name
	address
    }

    db.phone insert end name "fred" phone "9659-2990"
    db.phone insert end name wilma phone 96592990
    puts "PHONE: [db.phone get [db.phone find name fred]]"

    db.address insert end name "fred" address "nowhere"
    db.address insert end name wilma address there
    puts "ADDRESS: [db.address get [db.address find name fred]]"

    set union [db.phone join db.address name]
    puts "$union: [$union get [$union find name fred]]"
    puts [$union properties]
    puts [$union size]
    puts [$union name]
    puts "[$union get [$union find name wilma]]"
    puts [db.phone layout]
    puts "Views: [db views]"

    $union foreach v {} {
	puts "MOOP: $v"
    }
    $union foreach v {name wilma} {
	puts "MOOP1: $v"
    }
}
