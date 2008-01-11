package require struct::tree
package require Debug
Debug off STX

package provide stx 1.1

namespace eval stx {
    variable path	;# current path in tree
    variable cursor root	;# current node in tree
    variable tree ""	;# tree of list structure
    variable tagnum 0	;# tag counter - a real number to provide disjoint sets
    variable refs	;# set of all refs in text

    # mapping from formatting prefix character to implementation
    variable funct
    array set funct {
	" " pre
	* li
	# li
	; dlist
	= header
	> indent
	- hr
	. special
	\x82 li
	\x83 li
    }
    variable state {}
}

proc stx::undent {text} {
    package require textutil
    return [::textutil::undent $text]
}

# count the number of characters in the set 'leadin'
proc stx::count {para leadin} {
    set content [string trimleft $para $leadin]
    return [expr {[string length $para] - [string length $content]}]
}

# tree debugging - generate the node path to root
proc stx::path {{node ""}} {
    variable tree
    if {$node eq ""} {
	variable cursor
	set node $cursor
    }
    if {$node eq "root"} {
	return root
    } else {
	return "[path [$tree parent $node]] $node/[$tree get $node type]"
    }
}

# tree debugging - dump node
proc stx::node {{node root}} {
    variable tree
    set result ""
    if {$node eq ""} {
	set node root
    }

    $tree walk $node n {
	append result "[string repeat > [$tree depth $n]] [$tree get $n type]: "
	foreach k [$tree keys $n] {
	    if {$k ne "type"} {
		append result " $k \"[$tree get $n $k]\""
	    }
	}
	append result \n
    }
    return $result
}

# unwind the path stack to given new level of list nesting
proc stx::unlist {{new {}}} {
    variable path
    if {$path ne $new} {
	variable tree
	variable cursor

	# calculate the size of matching prefix
	set match 0
	foreach curr $path n $new {
	    if {$curr ne $n} break
	    incr match
	}

	# get the changes: undo set, then add set
	set undo [lrange $path $match end]
	set add [lrange $new $match end]
	Debug.STX {unlist Path: $path New: $new / $match / Undo: $undo Add: $add} 10

	# unwind the cursor, moving up the tree
	while {[llength $undo]} {
	    set cursor [$tree parent $cursor]
	    set undo [lrange $undo 0 end-1]
	}

	# construct list elements downward
	foreach l $add {
	    set cursor [$tree insert $cursor end]
	    $tree set $cursor type $l
	}

	set path $new
    }
}

# balance paired character-level markup, e.g. '' pairs
proc stx::balance {text sep fn} {
    set result ""
    foreach {pre seg} [split [string map [list $sep \x81] $text] \x81] {
	if {$seg ne ""} {
	    append result "${pre}\[$fn ${seg}\]"
	} else {
	    append result $pre
	}
    }
    return $result
}

# preprocess character-level markup
proc stx::char {text} {
    set text [string map {
	\[\[ \x84
	\]\] \x85
	\; \x87
    } $text]
    # handle naked http references
    regsub -all {([^[]|^)(http:[^ ]+)} $text {\1[http:\2]} text
	Debug.STX {Char: '$text'} 30
    variable refs
	#puts stderr "ENCODING REFS: $text"
    while {[regexp -- {\[[^]]+\]} $text ref]} {
	set index [array size refs]
	Debug.STX {ENCODE REF: $index $ref} 10
	set refs($index) [string trim $ref {[]}]
	regsub -- {\[[^]]+\]} $text "(\x86$index)" text
    }

    set text [balance $text ''' italic]
    set text [balance $text '' strong]
    set text [balance $text __ underline]
    set text [balance $text %% smallcaps]
    set text [balance $text -- strike]
    set text [balance $text ^^^ subscript]
    set text [balance $text ^^ superscript]
    set text [balance $text !! big]

    return $text	;# the list acts as a quote
}

# create a new node of given type with value at the given path
proc stx::newnode {type {p {}}} {
    variable tree
    variable cursor
    unlist $p
    set node [$tree insert $cursor end]
    $tree set $node type $type
    #variable path
    #$tree set $node path $path
    return $node
}

proc stx::nodevalue {node val} {
    variable tree
    $tree set $node val [list $val]
}

proc stx::nodecdata {node val} {
    variable tree
    set cdata [$tree insert $node end]
    $tree set $cdata type cdata
    $tree set $cdata val [list [list $val]]
}

# identity preprocess for normal
proc stx::normal {para} {
    nodecdata [newnode normal] [char $para]
}

proc stx::special {para} {
    nodecdata [newnode special] [string range $para 1 end]
}

# identity preprocess for hr
proc stx::hr {args} {
    newnode hr
}

# identity preprocess for pre
proc stx::pre {para} {
    nodecdata [newnode pre] [string range [string map {"\n " "\n"} $para] 1 end]
}

# preprocess for indent elements
proc stx::indent {para} {
    variable path
    nodecdata [newnode indent $path] [char $para]
}

# preprocess for header elements
proc stx::header {para} {
    Debug.STX {HEADER: $para}
    set count [count $para =]	;# depth of header nesting
    set para [string trimleft $para =] ;# strip leading ='s

    # we have the depth, and the start of the header
    # now we need to find the end of header.
    set p [split $para =]
    set para [lindex $p 0]
    set rest [string trimleft [join [lrange $p 1 end] =] "= "] ;# this may be a para

    set para [split $para "\#"]
    set tag [string trim [lindex $para end]]
    set para [string trim [lindex $para 0]]

    if {$tag eq ""} {
	variable tagnum
	set tagnum [expr $tagnum + 1.0] ;# allow non-int tagnums
	set tag "h$tagnum"
    }
    set hnode [newnode header]
    nodecdata $hnode $count
    nodecdata $hnode $para
    nodecdata $hnode $tag

    # now process any pendant para
    if {$rest ne ""} {
	do_para $rest
    }
}

# preprocess for list elements
proc stx::li {para} {
    set count [count $para "\#*"]	;# how many list levels deep?
    Debug.STX {li $count '$para'}
    set li [string range $para 0 [expr {$count - 1}]]	;# list prefix
    set li [split [string trim [string map {\# "ol " * "ul "} $li]]]
    set para [string trim [string range $para $count end]]	;# content

    nodecdata [newnode li $li] [char $para]
    #nodecdata [newnode li $li] [do_para $para]
}

# preprocess for dlist elements
proc stx::dlist {para {cpath ""}} {
    set pp [split [char [string trimleft $para ";"]] :]
    set term [lindex $pp 0]
    set def [string trim [join [lrange $pp 1 end] ": "]]
    set dlnode [newnode dl dlist]
    Debug.STX {DLIST: '$para' - '[char $para]' - [char $term]}
    nodecdata $dlnode $term	;#OR: nodecdata $dlnode $term ?
    nodecdata $dlnode $def
}

proc stx::do_para {para} {
    if {$para eq {}} return

    set first [string index $para 0]
    variable funct
    if {[info exists funct($first)]} {
	set f $funct($first)
    } else {
	set f normal
    }
    
    Debug.STX {do_para $f '$para'}
    $f $para
}

# translate structured text into
# tcl function calls over paragraphs
proc stx::translate {text {tagstart 0}} {
    # construct an empty tree
    variable tree
    if {$tree ne ""} {
	$tree destroy
    }
    set tree [::struct::tree stx]
    $tree set root type root

    # our original path is empty
    variable path {}
    variable cursor root
    variable tagnum $tagstart

    set result ""
    foreach para [split [string map {
	\n# \x81\#
	\n* \x81*
	\n; \x81;
	\n- \x81-
	\n= \x81=
	\n> \x81>

	\n\n\n\n\n \x81
	\n\n\n\n \x81
	\n\n\n \x81
	\n\n \x81

	"\n " "\n "
	\n " "
	
	\{ "\x89"
	\} "\x8A"
	$ "\x8B"
	< "&lt\x87"
	> "&gt\x87"
    } $text] \x81] {
	do_para $para
    }

    set result ""
    variable tree
    $tree walk root -order both {action node} {
	if {$node eq "root"} continue
	if {$action eq "enter"} {
	    if {[$tree depth $node] > 1} {
		append result " "
	    }
	    append result "\[[$tree get $node type]"
	    if {[$tree keyexists $node val]} {
		append result " [join [$tree get $node val]]"
	    }
	} else {
	    append result "] "
	    if {[$tree depth $node] == 1} {
		append result "\n"
	    }
	}
    }

    # substitute refs back in
    #puts stderr "STX REFSUBBING"
    variable refs
    while {[regexp -- {[(]\x86([^)]+)[)]} $result index]} {
	Debug.STX {REFSUB $index -> $refs([string trim $index \x86()])}
	regsub -- {[(]\x86[^)]+[)]} $result "\[ref \{$refs([string trim $index \x86()])\}\]" result
    }

    catch {unset refs}
    set refs() ""
    unset refs()
    Debug.STX {RESULT: ${result}}
    return ${result}
}
