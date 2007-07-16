package provide Html 1.0

interp alias {} armour {} string map {& &amp; < &lt; > &gt; \" &quot; ' &#39;}

namespace eval Html {
    proc attr {T args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	set result $T
	foreach {n v} $args {
	    if {$n in {checked disabled selected}} {
		if {$v} {
		    lappend result $n
		}
	    } else {
		lappend result "[string trim $n]='[armour [string trim $v]]'"
	    }
	}
	return [join $result]
    }
    
    proc menulist {menu} {
	return [<ul> [Foreach {text url} $menu {
	    [<li> [<a> href $url $text]]
	}]]
    }

    proc optset {selector args} {
	set optset ""
	for {val text} $args {
	    if {$val eq $selector} {
		append optset \n "<option selected value='$val'>$text</option>"
	    } else {
		append optset \n "<option value='$val'>$text</option>"
	    }
	}
	return $optset
    }

    proc table {name args} {
	array set arg $args
	append c "<table border='1' width='80%'>" \n
	append c <tr> <th> $name </th> </tr> \n
	foreach n [lsort [array names arg]] {
	    append c <tr> <td> $n </td> <td> $arg($n) </td> </tr> \n
	}
	append c </table> \n
	return $c
    }

    # dict2table - convert dict into sortable HTML table
    proc dict2table {dict header {footer {}}} {
	set row 0
	return [<table> class sortable [subst {
	    [<thead> [<tr> [Foreach t $header {
		[<th> [string totitle $t]]
	    }]]]
	    [If {$footer ne {}} {
		[<tfoot> [<tr> [Foreach t $footer {[<th> $t]}]]]
	    }]
	    [<tbody> [Foreach {k v} $dict {
		[<tr> class [If {[incr row] % 2} even else odd] \
		     [Foreach th $header {
			 [If {[dict exists $v $th]} {
			     [<td> [dict get $v $th]]
			 } else {
			     [<td> {}]
			 }]
		     }]]
	    }]]
	}]]
    }

    # dir2table - convert directory into sortable table
    proc dir2table {dir header {footer {}}} {
	if {$header eq {}} {
	    set header {name size mtime ctime atime}
	}
	return [dict2table [dir2dict [Dict dir $dir $header] $header $footer]]
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

proc <img> {args} {
    return "<[Html::attr img {*}args]>"
}

# HTML <> commands per http://wiki.tcl.tk/2776
package require know
know {[string match <*> [lindex $args 0]]} {
    set tag [string trim [lindex $args 0] "<>"]
    ::proc ::<$tag> {args} [string map [list @T $tag] {
	set result {@T}
	foreach {n v} [lrange $args 0 end-1] {
	    lappend result "[string trim $n]='[armour [string trim $v]]'"
	}
	set val [string trim [lindex $args end]]
	return "<[join ${result}]>$val</@T>"
    }]
    return [eval $args]
}

# Some command equivalents which use subst instead of eval

# If: Subst-if
# Note that this version does not support the then keyword in if,
# and requires the else keyword.
proc If {args} {
    #puts stderr "IF cond: [lindex $args 0]"
    while {[llength $args] && ![uplevel 1 expr [list [lindex $args 0]]]} {
	set args [lrange $args 2 end]	;# lose the cond and positive-cond
	#puts stderr "IF cond: [lindex $args 0]"
	
	if {[lindex $args 0] eq "else"} {
            break
        }
	
	set args [lrange $args 1 end] ;# assumed to be 'elseif'
    }
    #puts stderr "IF consequence: [lindex $args 0]"
    return [uplevel 1 subst [list [lindex $args 1]]] ;# return with neg-consequence
}

# Subst-while
proc While {cond body} {
    set result {}
    while {[uplevel 1 expr [list $cond]]} {
	lappend result [uplevel 1 subst [list $body]]
    }
    return [join $result]
}

# Subst-foreach
proc Foreach {args} {
    set body [lindex $args end]
    set vars [lrange $args 0 end-1]
    set script [string map [list %A $vars %B $body %V $vars] {
	set {%A} {}
	foreach %V {
	    lappend {%A} [subst {%B}]
	}
	return [join [set {%A}]]
    }]
    #puts stderr "FOREACH: $script"
    return [uplevel 1 $script]
}

# Subst-switch
proc Switch {args} {
    set switch {}
    foreach {key body} [lindex $args end] {
	if {$body eq "-"} {
	    lappend switch $key -
	} else {
	    lappend switch $key [list subst $body]
	}
    }
    return [uplevel 1 [list switch {*}[lrange $args 0 end-1] $switch]]
}
