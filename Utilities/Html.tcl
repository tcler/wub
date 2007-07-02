package provide Html 1.0

interp alias {} armour {} string map {& &amp; < &lt; > &gt; \" &quot; ' &#39;}

namespace eval Html {
    proc menulist {menu} {
	set result "<ul>\n"
	foreach {text url} $menu {
	    append result <li> "<a href='$url'>" $text </a> </li>
	}
	append result </ul> \n
	return $result
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

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

# HTML <> commands per http://wiki.tcl.tk/2776
package require know
know {[string match <*> [lindex $args 0]]} {
    set tag [string trim [lindex $args 0] "<>"]
    ::proc ::<$tag> {args} [string map [list @T $tag] {
	set result {}
	foreach {n v} [lrange $args 0 end-1] {
	    lappend result "[string trim $n]='[armour [string trim $v]]'"
	}
	set val [string trim [lindex $args end]]
	if {$val eq ""} {
	    return "<@T [join ${result}] />"
	} else {
	    return "<@T [join ${result}]>$val</@T>"
	}
    }]
    return [eval $args]
}

# Some command equivalents which use subst instead of eval

# If: Subst-if
# Note that this version does not support the then keyword in if,
# and requires the else keyword.
proc If {args} {
    while {[llength $args] && ![uplevel expr [lindex $args 0]]} {
	set args [lrange $args 2 end]	;# lose the cond and positive-cond
	
	if {[lindex $args 0] eq "else"} {
            break
        }
	
	set args [lrange $args 1 end] ;# assumed to be 'elseif'
    }
    return [uplevel subst [list [lindex $args 1]]] ;# return with neg-consequence
}

# Subst-while
proc While {cond body} {
    set result {}
    while {[uplevel $cond]} {
	lappend result [uplevel subst [list $body]]
    }
    return $result
}

# Subst-foreach
proc Foreach {args} {
    set body [lindex $args end]
    set vars [lrange $args 0 end-1]
    set script [string map [list %A $args %B $body %V $vars] {
	foreach %V {
	    lappend {%A} [subst {%B}]
	}
	return [set {%A}]
    }]
    return [uplevel $script]
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
    return [uplevel [list switch {*}[lrange $args 0 end-1] $switch]]
}
