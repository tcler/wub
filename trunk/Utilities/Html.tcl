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
	    lappend result "$n='[armour $v]'"
	}
	return "<@T [join ${result}]>[lindex $args end]</@T>"
    }]
    return [eval $args]
}
