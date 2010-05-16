package require OO

package provide Tuple 1.0

# AXIOMS

# TUPLES
#* T1: tuples indexed immutable ordinal id number
#* T2: tuples are uniquely indexed by name
#* T3: tuples have content of type (which may be a mime type)

# NAMING of tuples
#* N1: each tuple is uniquely named (from T1)
#* N2: a tuple's name may only comprise characters which are alnums, space, * or +
#* N3: tuple names may be composed by +, such a name is called a 'composite name'
#* N4: name composition is left-associative a+b+c is (a+b)+c

# REFERENCES to tuples
#* R1: a reference with the prefix form /#id is a reference to the tuple whose ordinal is id (which might be used as an HTTP object reference)
#* R2: reference /n is a reference to the tuple whose name is n, and known as a 'simple reference'
#* R3: reference /n/c is equivalent to /n+c.  Both forms are known as 'compound references'
#* R4: a reference /n.ext is a request to convert the tuple n to the mime type of the extension, and is otherwise equivalent to /n (see P1)
#* R7: a reference /+n is resolved as ${referer}+n

# TRANSCLUSION
#* Tc1: a reference /+n is a transclusion in the context of ${referer}
#* Tc2: a reference /n or /+n/ is a top level fetch

# COMPOSITION - a reference /M+n (or /m/+n) is resolved in the following order:
#* C1: as a tuple named M+n (HTTP Moved?)
#* C2: as a tuple named type(M)+n (HTTP See?)
#* C3 (QUESTIONABLE): if n has a leading *-character, then
#** C3.1 as an element n of the tuple M
#** C3.2: as an operator n applied to M.

# TYPE 
#* Ty1: a tuple will be transformed for presentation according to its type, its reference's .ext and client's HTTP Accept and the manner in which it is referenced (either for transclusion or for top-level presentation)
#* Ty2: the transformation of a tuple n will be performed by the operator n+*type(n).  If n+*type(n) is not of type text/tcl or text/js, then n+*type(n)+*type(type(n)) (and so on) will be sought.

# OWNERSHIP and permissions
#* O1: tuples are owned by a user and a group, which have distinct permissions
#* O2: users and groups are themselves tuples named *user+n and *group+n

if {[catch {package require Debug}]} {
    #proc Debug.tuple {args} {}
    proc Debug.tuple {args} {puts stderr "tuple @ [uplevel subst $args]"}
} else {
    Debug on tuple 10
}

oo::class create Tuple {
    # rightish - find the longest existing name-prefix
    method rightish {args} {
	set name [join $args +]

	variable name2id
	set rest {}
	while {[llength $args]} {
	    set left [join $args +]
	    if {[dict exists $name2id $left]} {
		set rest [join $rest +]
		set id [dict get $name2id $left]
		Debug.tuple {rightish found a prefix '$left' at #$id [expr {($rest eq "")?"":"with a remainder '$rest'"}]}
		return [list $id $left $rest]	;# axiom N1
	    } else {
		set rest [list [lindex $args end] {*}$rest]
		set args [lrange $args 0 end-1]
		Debug.tuple {rightish didn't find '$left' - try again with '$args' remainder '[join $rest +]'}
	    }
	}

	# we couldn't find any existing prefix
	Debug.tuple {rightish couldn't find any existing prefix for '$name'}
	return -code error -kind name -notfound $name "find: '$name' doesn't exist"
    }

    # find - turn a name into a tuple id.
    method find {name {referer ""}} {
	variable tuples
	variable name2id

	# do trivial case
	if {[string match #* $name]
	    && [string is integer [string range $name 1 end]]
	} {
	    set id [string trimleft $name #]
	    if {[info exists tuples($id)]} {
		Debug.tuple {found $name trivially}
		return $id
	    } else {
		return -code error -kind simple -notfound $a "find $name: tuple($id) does not exist"
	    }
	}

	Debug.tuple {find '$name'}
	# convert name to a list of elements axioms R7 and R3
	if {[string match +* $name]} {
	    set name $referer$name	;# axiom R7
	}

	set name [string tolower $name]	;# names are case-insensitive

	# reduce all path elements to names
	set rest {}
	foreach a [split $name /+] {
	    if {[string match #* $a]} {
		set aid [string trimleft $a #]
		if {[info exists tuples($aid)]} {
		    lappend rest [string tolower [dict get $tuples($aid) name]]
		} else {
		    return -code error -kind simple -notfound $a "find $name: tuple($a) does not exist"
		}
	    } elseif {[dict exists $name2id $a]} {
		lappend rest $a
	    } else {
		# individual component doesn't exist, but that's ok
		lappend rest $a
	    }
	}

	# determine longest matching prefix (left) its id and the right parth
	lassign [my rightish {*}$rest] id left right

	# prefix is $left and its id is $id
	# any remaining unmatched elements are in $right
	if {$right eq ""} {
	    Debug.tuple {find found a '$name' at #$id}
	    return [list $id $left ""]	;# we have a complete match
	}

	Debug.tuple {find failed to match '$name', longest prefix '$left' at #$id}

	# partial match with a single remaining right suffix
	# we know that $left exists and $left+$right doesn't exist
	if {[catch {
	    set type [join [split [dict get $tuples($id) type] /] +]
	    set essay $type+$right
	    if {$essay eq $name} error
	    Debug.tuple {find composite '$essay'}
	    my find $essay	;# find the type equivalent
	} found] && [catch {
	    set essay *rform+$right
	    if {$essay eq $name} error
	    Debug.tuple {find composite '$essay'}
	    my find $essay	;# find the *rform equivalent
	} found]} {
	    # axiom C3 (field as pseudo tuple)
	    Debug.tuple {find didn't find composite formd '$type+$right' or '*rform+$right'}
	    if {[string match {[*]*} $right]} {
		# construct a synthetic tuple whose content is the tuple's field contents
		# and whose types etc are either derived from the tuple itself or
		# are constants provided by tuple metadata.  axiom C3
		variable metadata
		lassign $id id left
		set rest [join [lassign [split $right +] right] +]
		set tuple $tuples($id)
		set right [string trimleft $right *]
		if {[dict exists $tuple $right]} {
		    set sid #$id#$right	;# synthetic tuples's id
		    if {![info exists tuple($sid)]} {
			if {![dict exists $metadata $right]} {
			    return -code error -kind field -notfound $right "find: field $right doesn't exist"
			}

			# copy metadata for field into synthetic tuple
			dict for {n v} [dict get $metadata $right] {
			    lappend synthetic $n [dict get $metadata $right $n]
			}

			# copy uninitialized synthetic from tuple
			dict for {n v} $tuple {
			    if {![dict exists $synthetic $n]} {
				dict set synthetic $n $v
			    }
			}

			# get synthetic content as tuple's field content
			dict set synthetic content [dict get $tuple $right]
			set sname $left+*$right
			dict set synthetic name $sname
			dict set synthetic id $sid

			# create the synthetic tuple with a crazy name
			set $tuples($sid) $synthetic
		    }
		    if {[llength $rest]} {
			tailcall my find $sid+$rest
		    } else {
			return [list $sid $sname $rest]
		    }
		}
	    }

	    # give up - there's no such tuple
	    return -code error -kind compound -notfound [list $left $right] "find: $name - found '$left' at #$id, but can't find '$type+$right' or '*rform+$right'"
	} else {
	    # found the named tuple
	    lassign $find found l r
	    Debug.tuple {find found $essay at #[dict get $found id] for $name at #$id as ($left)+($right)}
	    return [list $found $left $right]
	}
    }

    # return a list of indices whose name matches the regexp
    method regexpByName {regexp} {
	variable name2id
	set result [dict values [dict filter $name2id script {name id} {
	    regexp ^$regexp\$ $name
	}]]
	Debug.tuple {regexpByName $glob ($result)}

	set names {}
	foreach n $result {
	    lappend names #$n
	}
	return $names
    }

    # return a list of indices whose name matches the glob
    method globByName {glob} {
	variable name2id
	set result [dict values [dict filter $name2id script {name id} {
	    #Debug.tuple {globByName pername: '$glob $name'}
	    string match $glob $name
	}]]
	Debug.tuple {globByName $glob ($result)}

	set names {}
	foreach n $result {
	    lappend names #$n
	}
	return $names
    }

    # return a tuple given an id
    method id2tuple {id} {
	variable tuples
	return $tuples($id)
    }

    # fetch a tuple
    method fetch {name} {
	variable tuples
	if {[string match +* $name]} {
	    error "fetch: $name must be fully qualified"
	}
	lassign [my find $name] id left right

	# TODO - check permissions

	# fetch the identified tuple
	set tuple $tuples($id)
	if {[string match *#* $id]} {
	    # this is a synthetic tuple, we could destroy it here
	    # or could leave it for a gc sweep
	}

	# record the actual name we're fetching
	dict set tuple _left $left
	dict set tuple _right $right

	return $tuple
    }

    # fixups for linkage to code, etc.
    method fixup {tuple} {
	variable metadata
	dict with tuple {
	    if {0 && [string match {[*]*} $_right]} {
		# must be a tuple field described by $metadata
		if {![dict exists $metadata [string trimleft $_right *]]} {
		    error -kind field -notfound $_right "Field $_right of $name must be a Field"
		}
	    }
	}

	# remove immutable fields
	dict for {n v} $tuple {
	    if {[string match _* $n]} {
		dict unset tuple $n
	    }
	}

	# lowercase some fields
	foreach n {type mime} {
	    if {[dict exists $tuple $n]} {
		set v [dict get $tuple $n]
		set tlv [string tolower $v]
		if {$tlv ne $v} {
		    dict set tuple $n $tlv
		    dict unset tuple $n
		}
	    }
	}
	Debug.tuple {Tuple fixed up ($tuple)}
	return $tuple
    }

    method set {id args} {
	variable tuples
	dict set tuples($id) {*}$args
    }

    # store values in a tuple or a field
    method store {name args} {
	variable tuples
	if {[string match +* $name]} {
	    error "store: $name must be fully qualified"
	}

	# resolve name as id, leftmost match and rightmost non-match
	lassign [my find $name] id left right

	# TODO - check permissions

	# fetch the identified tuple
	set tuple $tuples($id)

	# ensure all field names are lowercase
	# remove immutable and synthesised fields before storage
	dict for {n v} $args {
	    if {$n eq "id" || [string match _* $name]} {
		dict unset args $n	;# name is immutable
		continue
	    }

	    set lcn [string tolower $n]
	    if {$n ne $lcn} {
		dict set args $lcn $v
		dict unset args $n
	    }
	}

	if {[dict exists $args name]} {
	    dict unset args name
	    # rename tuple - NOT IMPLEMENTED
	}

	if {[string match *#* $id]} {
	    # special form of assignment to a synthetic tuple
	    # representing a field within a tuple
	    # this ensures coherence between synthetic and actual
	    Debug.tuple {store to field tuple $id}
	    if {[dict exists $args content]} {
		set content [dict get $args content]
		if {[info exists tuples($id)]} {
		    # only write the tuple if it already exists
		    dict set tuples($id) content $content
		}

		# reflect synthetic tuple writing to actual tuple's field
		lassign [split $id #] pid field
		set tuples($pid) [my fixup [dict merge $tuple [list $field $content _left $left _right $right]]]
	    } else {
		error "only content is settable in synthetic tuples"
	    }

	    # this is a synthetic tuple, we could destroy it here
	    # or await a later gc sweep
	} else {
	    set $tuples($id) [my fixup [dict merge $tuple $args [list _left $left _right $right]]]
	    Debug.tuple {store tuple $id ($tuples($id))}
	}

	return $id
    }

    # generate a unique id - axiom T1
    # (this may be overridden by a different storage mechanism)
    method newid {} {
	variable nextid
	return [incr nextid]
    }

    method New {args} {
	Debug.tuple {New Tuple ($args)}

	# index lowercase name
	variable name2id
	set nname [string tolower [dict get $args name]]

	# ensure name isn't reused
	if {[dict exists $name2id $nname]} {
	    dict set args id [set id [dict get $name2id $nname]]
	} else {
	    # generate unique id - axiom T1
	    dict set args id [set id [my newid]]
	}

	# remove immutable and synthesised fields before storage
	foreach k [dict keys $args _*] {
	    if {[dict exists $args $k]} {
		dict unset args $k	;# $k is immutable
	    }
	}

	# ensure meaningful default field values
	foreach {n d} {type Basic content ""} {
	    if {![dict exists $args $n]} {
		dict set args $n $d
	    }
	}

	variable tuples
	if {[info exists tuples($id)]} {
	    # update tuple
	    set tuple [dict merge $tuples($id) $args]
	} else {
	    set tuple $args	;# create tuple
	}
	
	set tuples($id) [my fixup $tuple]
	
	dict set name2id $nname $id	;# this is the only place we set name

	Debug.tuple {New Tuple: ($tuples($id)) with name '$nname' and id $id}

	return $id	;# return tuple's id
    }

    # create a new tuple
    method new {args} {
	Debug.tuple {new tuple $args}

	# ensure all field names are lowercase
	dict for {n v} $args {
	    set lcn [string tolower $n]
	    if {$n ne $lcn} {
		dict set args $lcn $v
		dict unset args $n
	    }
	}

	# names must be unique - axiom T2
	if {![dict exists $args name]} {
	    error "create failed: name not given"
	}
	set name [string tolower [dict get $args name]]

	variable name2id
	if {[dict exists $name2id $name]} {
	    error "create failed: name $name already exists"
	}

	# ensure type consistency
	variable tuples
	if {[dict exists $args type]} {
	    # types must exist, and must be of type Type
	    set t [string tolower [dict get $args type]]
	    if {![dict exists $name2id $t]} {
		return -code error -kind type "type [dict get $args type] does not exist"
	    } else {
		set tid [dict get $name2id $t]
		if {[string tolower [dict get $tuples($tid) name]] ne "basic"
		    && [string tolower [dict get $tuples($tid) type]] ne "type"
		} {
		    return -code error -kind type "type '[dict get $args type]' is not of type Type"
		}
	    }
	}

	dict set args id [set id [my newid]]

	tailcall my New {*}$args
    }

    method create {name args} {
	tailcall my new $args name $name
    }

    # ids of matching tuples
    method ids {args} {
	variable tuples
	return [array names tuples {*}$args]
    }

    # all matching tuples
    method all {args} {
	variable tuples
	return [array get tuples {*}$args]
    }

    # full resolution of all matching tuples
    # this differs from [all] because its result
    # contains synthetic fields
    method full {args} {
	variable tuples
	set result {}
	set names [array names tuples {*}$args]
	Debug.tuple {Full ([lsort -dictionary $names]) from '$args'}
	foreach n $names {
	    lappend result $n [my fetch [dict get $tuples($n) name]]
	}
	return $result
    }

    method prime {content} {
	variable tuples
	variable name2id
	foreach {n v} $content {
	    # ensure all field names are lowercase
	    dict for {tn tv} $v {
		set lcn [string tolower $tn]
		if {$tn ne $lcn} {
		    dict set v $lcn $tv
		    dict unset v $tn
		}
	    }

	    # decode the dict key as an id or a name
	    if {[string match #* $n]} {
		error "Prime can't specify id"
	    }
	    dict set v name $n

	    if {[dict exists $v name]} {
		set name [dict get $v name]
		if {[dict exists $name2id $name]} {
		    dict set v id [dict get $name2id $name]
		} else {
		    # no existing tuple with this name - New will create it
		    #dict set v id [my newid]
		}
	    } else {
		# no id, no name - this has to be an error
		error "prime: must supply name or existing id ($v)"
	    }
	    Debug.tuple {Prime $v}

	    # fixup and store the new tuple
	    my New {*}$v
	}
    }

    method metadata {args} {
	variable metadata
	return $metadata
    }

    method traceN {var id op args} {
	variable name2id
	variable old
	#set from  "from '[info frame [expr {[info frame] -1}]]'"
	#set from  "from '[info frame -1]'"
	set from  "from '[info level -1]'"
	if {[info exists old]} {
	    dict for {n v} $name2id {
		if {$n ne [string tolower $n]} {
		    error "CASE '$n' $from"
		}
		if {[dict exists $old $n]} {
		    if {[dict get $old $n] eq $v} {
			puts stderr "NAME CHANGED $op '$n': [dict get $old $n] -> $v $from"
		    } else {
			error "RENAMED $op '$n': [dict get $old $n] -> $v $from"
		    }
		}
	    }
	}
	set old $name2id
    }

    method traceT {var id op args} {
	variable tuples
	variable name2id
	switch -- $op {
	    write {
		set tuple $tuples($id)
		#set detail "id:([dict merge $tuple {content ...}]) from '[info frame [expr {[info frame] -1}]]'"
		#set detail "id:([dict merge $tuple {content ...}]) from '[info frame -1]'"
		set detail "id:([dict merge $tuple {content ...}]) from '[info level -1]'"
		if {[catch {
		    dict size $tuple
		}]} {
		    error "NOT A DICT $detail"
		}

		dict with tuple {
		    set nname [string tolower $name]
		    if {[dict exists $name2id $nname]
			&& $id != [dict get $name2id $nname]
		    } {
			error "DUPLICATE: [dict get $name2id $nname] $detail"
		    }
		}

		#puts stderr "WRITE: $detail"
	    }
	}
    }

    constructor {args} {
	variable tuples
	array set tuples {}	;# tuples array permits traces
	variable name2id {}
	variable trace 0
	#variable nextid -1

	if {[llength $args]%2} {
	    set content [lindex $args end]
	    set args [lrange $args 0 end-1]
	}

	variable {*}$args
	next? {*}$args

	if {$trace} {
	    trace add variable tuples {array write unset} [list [self] traceT]
	    trace add variable name2id {write} [list [self] traceN]
	}

	# metadata for each tuple field as if it were itself a tuple
	variable metadata {
	    id {type numeral}
	    name {type text}
	    content {type blob}
	    type {type text}
	    mime {type text}
	}

	if {[info exists prime]} {
	    my prime $prime
	}
    }
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    set ts [Tuple new]
    $ts prime {
	0 {
	    name Root
	}
	text {
	    content "This is a test"
	    type Text
	}
    }

    puts "test 'all' method:"
    foreach {n v} [$ts all] {
	if {[lindex [$ts find #$n] 0] != $n} {
	    error "Couldn't find $n"
	}
	puts "$n: $v"
    }
}
