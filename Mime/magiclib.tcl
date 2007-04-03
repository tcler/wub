# usage:

# 1) magic::open to open the file and prime buffers, etc.
# 2) run the appropriate generated magic over it, viz magic::/magic.mime
# 3) the result call in the generated magic will return the result of scanning
#     to the caller.
# 4) magic::close to release the stuff.

# TODO:

# Required Functionality:

# implement full offset language
# implement pstring (pascal string, blerk)
# implement regex form (blerk!)
# implement string qualifiers

# Optimisations:

# reorder tests according to expected or observed frequency
# this conflicts with reduction in strength optimisations.

# Rewriting within a  level will require pulling apart the
# list of tests at that level and reordering them.
# There is an inconsistency between handling at 0-level and
# deeper level - this has to be removed or justified.

# Hypothetically, every test at the same level should be
# mutually exclusive, but this is not given, and should be
# detected.  If true, this allows reduction in strength to switch
# on Numeric tests

# reduce Numeric tests at the same level to switches
#
# - first pass through clauses at same level to categorise as
#   variant values over same test (type and offset).

# work out some way to cache String comparisons

# Reduce seek/reads for String comparisons at same level:
#
# - first pass through clauses at same level to determine string ranges.
#
# - String tests at same level over overlapping ranges can be
#   written as sub-string comparisons over the maximum range
#   this saves re-reading the same string from file.
#
# - common prefix strings will have to be guarded against, by
#   sorting string values, then sorting the tests in reverse length order.
package provide magiclib 1.0

namespace eval magic {}

set magic::debug 0
set magic::optimise 1

# open the file to be scanned
proc magic::open {file} {
    variable fd
    if {[info exists fd]} {
	::magic::close
    }

    set fd [::open $file]
    fconfigure $fd -translation binary

    # fill the string cache
    # the vast majority of magic strings are in the first 4k of the file.
    variable strbuf
    set strbuf [read $fd 4096]

    # clear the fetch cache
    variable cache
    catch {unset cache}

    variable result
    set result ""

    variable string
    set string ""

    variable numeric
    set numeric -9999

    return $fd
}

proc magic::close {args} {
    variable fd
    ::close $fd
    unset fd
}

# mark the start of a magic file in debugging
proc magic::file_start {name} {
    variable debug
    if {$debug} {
	puts stderr "File: $name"
    }
}

# return the emitted result
proc magic::result {{msg ""}} {
    variable result
    if {$msg != ""} {
	emit $msg
    }
    return -code return $result
}

# emit a message
proc magic::emit {msg} {
    variable string
    variable numeric
    set msg [::string map [list \\b "" %s $string %ld $numeric %d $numeric] $msg]

    variable result
    append result " " $msg
    set result [string trim $result " "]
}

# handle complex offsets - TODO
proc magic::offset {where} {
    variable debug
    #if {$debug} {
	puts stderr "OFFSET: $where"
    #}
    return 0
}

# fetch and cache a value from the file
proc magic::fetch {where what scan} {
    variable cache
    variable numeric

    if {![info exists cache($where,$what,$scan)]} {
	variable fd
	seek $fd $where
	binary scan [read $fd $what] $scan numeric
	set cache($where,$what,$scan) $numeric
    } else {
	set numeric $cache($where,$what,$scan)
    }
    return $numeric
}

# maps magic typenames to field characteristics: size, binary scan format
array set magic::typemap {
    byte {1 c}
    ubyte {1 c}
    short {2 S}
    ushort {2 S}
    beshort {2 S}
    leshort {2 s}
    ubeshort {2 S}
    uleshort {2 s}
    long {4 I}
    belong {4 I}
    lelong {4 i}
    ubelong {4 I}
    ulelong {4 i}
    date {2 S}
    bedate {2 S}
    ledate {2 s}
    ldate {4 I}
    beldate {4 I}
    leldate {4 i}
}

# generate short form names
foreach {n v} [array get magic::typemap] {
    foreach {len scan} $v {
	#puts stderr "Adding $scan - [list $len $scan]"
	set magic::typemap($scan) [list $len $scan]
	break
    }
}

proc magic::Nv {type offset {qual ""}} {
    variable typemap
    variable numeric

    # unpack the type characteristics
    lassign $typemap($type) size scan

    # fetch the numeric field
    set numeric [fetch $offset $size $scan]

    if {$qual != ""} {
	# there's a mask to be applied
	set numeric [expr $numeric $qual]
    }

    variable debug
    if {$debug} {
	puts stderr "NV $type $offset $qual: $numeric"
    }

    return $numeric
}

# Numeric - get bytes of $type at $offset and $compare to $val
# qual might be a mask
proc magic::N {type offset comp val {qual ""}} {
    variable typemap
    variable numeric

    # unpack the type characteristics
    lassign $typemap($type) size scan

    # fetch the numeric field
    set numeric [fetch $offset $size $scan]

    if {$comp == "x"} {
	# anything matches - don't care
	return 1
    }

    # get value in binary form, then back to numeric
    # this avoids problems with sign, as both values are
    # [binary scan]-converted identically
    binary scan [binary format $scan $val] $scan val

    if {$qual != ""} {
	# there's a mask to be applied
	set numeric [expr $numeric $qual]
    }

    set c [expr $val $comp $numeric]	;# perform comparison

    variable debug
    if {$debug} {
	puts stderr "numeric $type: $val $comp $numeric / $qual - $c"
    }

    return $c
}

proc magic::getString {offset len} {
    # cache the first 1k of the file
    variable string
    set end [expr {$offset + $len - 1}]
    if {$end < 4096} {
	# in the string cache
	variable strbuf
	set string [string range $strbuf $offset $end]
    } else {
	# an unusual one
	variable fd
	seek $fd $offset	;# move to the offset
	set string [read $fd $len]
    }
    return $string
}

proc magic::S {offset comp val {qual ""}} {
    variable fd
    variable string

    # convert any backslashes
    set val [subst -nocommands -novariables $val]

    if {$comp eq "x"} {
	# match anything - don't care, just get the value
	set string ""

	seek $fd $offset	;# move to the offset
	while {([::string length $string] < 100)
	       && [::string is print [set c [read $fd 1]]]} {
	    if {[string is space $c]} {
		break
	    }
	    append string $c
	}

	return 1
    }

    # get the string and compare it
    set string [getString $offset [::string length $val]]
    set cmp [::string compare $val $string]
    set c  [expr $cmp $comp 0]

    variable debug
    if {$debug} {
	puts "String '$val' $comp '$string' - $c"
	if {$c} {
	    puts "offset $offset - $string"
	}
    }

    return $c
}
