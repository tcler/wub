package provide functional 1.0

# eval+ always tries to compile its argument
interp alias {} eval+ {} if 1

proc lambda {arglist body {ns {}}} {
    list ::apply [list $arglist $body $ns]
}

proc curry {lam args} {
    lappend lam {*}$args
}

# Maps a function to each element of a list,
# and returns a list of the results.
proc map {func list} {
    set ret [list]
    foreach item $list {
	lappend ret [uplevel 1 $func [list $item]]
    }
    return $ret
}

proc mapargs {func args} {
    return [map $func $args]
}

# Filters a list, returning only those items which pass the filter.
proc filter {func list} {
    set ret [list]
    foreach item $list {
	if {[uplevel 1 $func [list $item]]} {
	    lappend ret $item
	}
    }
    return $ret
}

proc filterargs {func args} {
    return [filter $func $args]
}

# Useful higher-order functions which replace common uses of recursion
# foldl (fold left)
# foldl - 0 {1 2 3} -> ((0-1)-2)-3
proc foldl {func default list} {
    set res $default
    foreach item $list {
	set res [{*}$func [list $res $item]]
    }
    return $res
}

# foldr (fold right)
# foldr + 0 {1 2 3} -> 1+(2+(3+0))
proc foldr {func default list} {
    set tot $default
    # Go in reverse
    for {set i [llength $list]} {$i > 0} {incr i -1} {
	set tot [{*}$func [list [lindex $list [expr {$i-1}]] $tot]]
    }
    return $tot
}

# compose - compose two functions together
# [compose f g] $args -> f [g $args]
proc compose {f g} {
    return [lambda {args} "$f \[$g {*}\$args\]"]
}

# The K combinator - obscure, but very useful.
proc K {a b} { set a }
