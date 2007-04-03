# tie an array of dicts to an mk database
package provide mkrecord 1.0

package require Tcl 8.5
package require snit
package require record
package require Mk4tcl

snit::type mkrecord {
    option -layout
    option -key
    option -db
    option -view

    option -check ""
    option -variable ""
    option -delcheck ""

    variable record
    variable max_key -1
    component view -inherit yes
    variable check -array {}

    method var {} {
	return [myvar record]
    }

    method max {} {
	return $max_key
    }

    method get {field value} {
	return [$view get [$view find $field $value]]
    }

    method write {arr id op} {
	if {[string match {@*} $id]} {
	    return
	}
	puts stderr "$options(-view) write $id - $record($id)"

	set to $record($id)
	if {![info exists record(@$id)]} {
	    # brand new record
	    set cursor [$view append $options(-key) $id]
	    set record(@$id) [$view get $cursor]
	} else {
	    set cursor [$view find $options(-key) $id] 
	}

	set from $record(@$id)
	set delta [record delta $from $to]

	# consistency check the record
	if {$options(-check) ne ""} {
	    puts stderr "$options(-view) write $id checking $from -> $delta"
	    set delta [{*}$options(-check) $id $from $to]
	    puts stderr "$options(-view) write $id changed $delta"
	}
	$view set $cursor {*}$delta

	set record($id) [$view get $cursor]
	set record(@$id) $record($id)

	puts stderr "$options(-view) write $id done $record($id)"
    }

    method unset {arr id op} {
	if {{$options(-delcheck) ne ""}
	    && [{*}$options(-delcheck) $record($id)]} {
	    unset record(@$field)
	    $view delete [$view find $options(-key) $id]
	} else {
	    # reinstate value
	    set record($field) $record(@$field)
	}
    }

    constructor {args} {
	$self configurelist $args
	$options(-db) layout $options(-view) $options(-layout)
	set view $options(-db).$options(-view)

	if {$options(-variable) ne ""} {
	    # make 'record' an alias for some other variable.
	    namespace eval \
		[namespace qualifiers [myvar record]] \
		"upvar #0 $options(-variable) record"
	} else {
	    array set record {}
	}

	$view all rec {
	    dict unset rec ""
	    set id [dict get $rec $options(-key)]
	    if {$id > $max_key} {
		set max_key $id
	    }
	    set record($id) $rec	;# main record
	    set record(@$id) $rec	;# backup
	}

	trace add variable record write [list $self write]
	trace add variable record unset [list $self unset]
    }
}
