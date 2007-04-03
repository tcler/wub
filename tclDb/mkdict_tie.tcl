# mkdict_tie.tcl --
#
#	Tie arrays of dicts to Mk 
#
# Copyright (c) Colin McCormack 15Nov2005
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# 
# RCS: @(#) $Id: mkdict_tie.tcl,v 1.3 2005/09/28 04:51:24 andreas_kupries Exp $

# ### ### ### ######### ######### #########
## Requisites

package provide mkdict_tie 1.0

package require snit
package require tie

if {[info exists argv0] && ($argv0 eq [info script])} {
    lappend auto_path [pwd]
}
package require Db

# ### ### ### ######### ######### #########
## Implementation

snit::type mkdict_tie {

    option -index "name"	    ;# field by which array is accessed
    variable ov	;# index by which array is matched
    option -layout "tie {name:S value:S}"	;# default layout of view 'tie'
    # Nb: the layout need only be specified for fields whose values aren't
    # the default type (S)

    option -file ""	;# file to open
    option -db "%AUTO%"	;# already open db or "%AUTO%"
    variable db	;# our db
    variable max_id	;# maximum id

    option -view ""	;# already open view
    component view	;# view used in processing
    option -options	;# passthrough metakit options

    pragma -hastypemethods no
    pragma -hasinfo        no
    pragma -simpledispatch yes

    # ### ### ### ######### ######### #########
    ## API : Construction & Destruction

    constructor {args} {
	$self configurelist $args
	
	if {$options(-view) eq ""} {
	    # no view supplied
	    
	    if {$options(-file) eq ""} {
		# already open db
		set db $options(-db)
	    } else {
		# create/open db from file
		install db using Db $options(-db) \
		    -file $options(-file) \
		    {*}$options(-options)
	    }

	    # construct the user view
	    set layout [lassign $options(-layout) viewname]
	    $db layout $viewname {*}$layout
	    set view $db.$viewname
	    #puts stderr "LAYOUT: [mk::view layout $db.viewname]"
	} else {
	    set view $options(-view)
	}
	set ov $options(-index)
    }

    destructor {
	$db commit
	if {$options(-view) eq ""} {
	    close $view
	} elseif {$options(-db) eq "%AUTO%"} {
	    $db close
	}
    }

    # ### ### ### ######### ######### #########
    ## API : Data source methods

    # equiv: array get
    method get {} {
	#puts stderr "$self get"

	set result {}
	$view all rec {
	    #puts stderr "REC: $rec"
	    catch {
		set key [dict get $rec $ov]
		dict unset rec ""	;# don't want cursor
		lappend result $key $rec
	    }
	}

	return $result
    }
    
    # equiv array set
    method set {dict} {
	#puts stderr "$self set $dict"
	for dict {n v} $dict {
	    catch {dict unset v $ov}
	    if {[catch {$view find $ov $n} cursor]} {
		$view append $ov $n {*}$v
	    } else {
		$view set $cursor $ov $n {*}$v
	    }
	}
	$db commit
    }

    # equiv array unset
    method unset {{pattern *}} {
	#puts stderr "$self unset $pattern"
	$view foreach rec [list -glob "" $pattern] {
	    $view delete [dict get $rec ""]
	}
    }

    # equiv array names
    method names {} {
	set names {}
	$view foreach v {} {
	    lappend names [dict get $v $ov]
	}
	return $names
    }

    # equiv array size
    method size {} {
	return [$view size]
    }

    # equiv $a($index)
    method getv {index} {
	#puts stderr "$self getv $index -> [$self get [$self find $ov $index]]"
	if {$index eq ""} {
	    return "max_id $max_id"
	}
	return [$view get [$view find $ov $index]]
    }

    # equiv [set a($index) $value]
    method setv {index value} {
	Debug.db {$self setv '$index' '$value'}
	if {$index eq ""} {
	    set {*}$value
	    return
	}
	dict set value $ov $index

	if {[catch {$view find $ov $index} cursor]} {
	    #puts stderr "$self setv $index $value APPEND"
	    $view append $ov $index {*}$value
	} else {
	    #puts stderr "$self setv $index $value SET"
	    $view set $cursor $ov $index {*}$value
	}

	$db commit
    }

    # equiv unset a($index)
    method unsetv {index} {
	#puts stderr "$self unsetv $index"
	$view delete [$view find $ov $index]
	$db commit
    }
}

::tie::register mkdict_tie as mkdict_tie

if {[info exists argv0] && ($argv0 eq [info script])} {
    array set test {}
    ::tie::tie test -open -merge mkdict_tie \
	-file mkdict_tie.db \
	-layout {tie "uid:I
	    user:S
	    email:S
	    count:I"} \
	-index uid
    puts "[::tie::info ties test] - [array size test]"

    if {[info exists test(0)]} {
	set uid [expr {int(rand() * 1000)}]
	set test($uid) [dict create uid $uid user user$uid email user$uid@fred]
    } else {
	set uid 0
	set test(0) [dict create uid 0 user root email root@fred]
	set test(1) [dict create uid 1 user colin email colin@fred]
    }

    puts "Array: [array get test]"
    puts $test(0)
    puts $test($uid)
    puts [dict get $test(0) uid]
    puts [dict incr test(0) count]
}
