# zen - parse and manipulate zencode
# http://code.google.com/p/zen-coding/

package require Tcl 8.5
package provide Zen 1.0

if {[catch {package require Debug}]} {
    proc Debug.zenparse {args} {puts stderr zen@[uplevel subst $args]}
    proc Debug.zenparse {args} {}
    proc Debug.zengen {args} {puts stderr zengen@[uplevel subst $args]}
    proc Debug.zengen {args} {}
} else {
    Debug define zenparse 10
    Debug define zengen 10
}

# ZenGen is a parent class providing support for generators
oo::class create ::ZenGen {
    # find free -variables in children of an element
    method freevars {args} {
	Debug.zengen {freevars $args}
	set vars {}
	foreach arg $args {
	    switch -- [lindex $arg 0] {
		term {
		    if {[dict exists [lrange $arg 2 end] -variable]} {
			lappend vars [dict get [lrange $arg 2 end] -variable] 
		    }
		}
		mult {
		    lappend vars {*}[my freevars {*}[lrange $arg 2 end]]
		}
		child {
		    lappend vars {*}[my freevars [lindex $arg 1]]
		    lappend vars {*}[my freevars {*}[lrange $arg 2 end]]
		}
		default {}
	    }
	}
	Debug.zengen {freevars $args -> $vars}
	return $vars
    }

    method commands {args} {
	Debug.zengen {commands $args}
	set cmds {}
	foreach arg $args {
	    switch -- [lindex $arg 0] {
		term {
		    if {[dict exists [lrange $arg 2 end] -command]} {
			lappend cmds [dict get [lrange $arg 2 end] -command] 
		    }
		}
		mult {
		    lappend cmds {*}[my commands {*}[lrange $arg 2 end]]
		}
		child {
		    lappend cmds {*}[my commands [lindex $arg 1]]
		    lappend cmds {*}[my commands {*}[lrange $arg 2 end]]
		}
		default {}
	    }
	}
	Debug.zengen {commands $args -> $cmds}
	return $cmds
    }

    method mult {mult args} {
	variable context
	set result ""
	variable multiplying 1
	if {$mult eq "*"} {
	    Debug.zengen {mult '$mult' args:$args}

	    # build map of vars per element
	    set el2var {}	;# map from el to referenced vars
	    set allvars {}	;# all bound vars
	    set bvars {}	;# bound vars per element
	    set fvars {}	;# free vars per element
	    foreach arg $args {
		set freevars [my freevars $arg]
		dict set el2var $arg $freevars
		foreach v $freevars {
		    if {[dict exists $context $v]} {
			# assumed to be a list
			lappend bvars $arg $v
			dict set allvars $v 1
		    } else {
			lappend fvars $arg $v
		    }
		}
	    }

	    set cmds [my commands {*}$args]

	    Debug.zengen {allvars: ($allvars) fvars: ($fvars) bvars: ($bvars)}
	    # evaluate the contained args repeatedly until they error
	    variable context
	    set mcont $context
	    set index 0
	    set processing [expr {[dict size $allvars] + [llength $cmds]}]
	    while {$processing} {
		foreach v [dict keys $allvars] {
		    set newval [lindex [dict get $mcont $v] $index]
		    dict set context $v $newval
		    if {$newval eq ""} {
			dict unset allvars $v
		    }
		}

		set processing [expr {[dict size $allvars] + [llength $cmds]}]
		set line {}
		if {$processing} {
		    dict set context _ $index
		    foreach arg $args {
			if {[catch {
			    lappend line [my {*}$arg]
			}]} {
			    set processing 0
			    set line {}
			}
		    }
		    incr index
		}
		lappend result {*}$line
	    }
	    set context $mcont
	} else {
	    set vars [my freevars {*}$args]
	    Debug.zengen {mult '$mult' vars:($vars) args:$args}

	    variable context
	    set result {}
	    for {set index 0} {$index < $mult} {incr index} {
		foreach var $vars {dict set context $var $index}
		foreach arg $args {
		    lappend result [my {*}$arg]
		}
	    }
	}
	set multiplying 0
	Debug.zengen {mult result: $result}
	return [join $result \n]
    }

    method attr {args} {
	variable context
	set result ""
	foreach {n v} $args {
	    if {![string match -* $n]} {
		if {$v eq ""} continue
		lappend result $n [::apply [list [dict keys $context] [string map [list %V% $v] {subst %V%}]] {*}[dict values $context]]
	    }
	}
	return $result
    }

    method value {args} {
	set result {}
	if {[dict exists $args -literal]} {
	    # term has a literal value
	    lappend result [dict get $args -literal]
	} elseif {[dict exists $args -command]} {
	    # term's value is given by the generator
	    Debug.zengen {value from command '[dict get $args -command]'}
	    variable context
	    set cmd [::apply [list [dict keys $context] [dict get $args -command]] {*}[dict values $context]]
	    Debug.zengen {value from command '[dict get $args -command]' -> $cmd}
	    lappend result $cmd
	} elseif {[dict exists $args -variable]} {
	    # term's value is in $variable
	    set variable [dict get $args -variable]
	    if {[string match ::* $variable]} {
		lappend result [set $variable]
	    } else {
		variable context
		Debug.zengen {gen term using variable '$variable' from ($context)}
		lappend result [dict get $context $variable]
	    }
	}
	return $result
    }

    method generate {args} {
	Debug.zengen {generate: $args}
	set result {}
	foreach arg $args {
	    lappend result [my {*}$arg]
	}
	return [join $result \n]

    }

    constructor {args} {
	variable multiplying 0
	variable context $args
    }
}

# ZenCSS is a class to generate CSS from zencode
oo::class create ::ZenCSS {
    method attr {args} {
	set result {}
	foreach {n v} [next {*}$args] {
	    lappend result "$n: $v;"
	}
	return [join $result " "]
    }

    method tagAttrs {tag var} {
	upvar $var attrs
	if {[dict exists $attrs class]} {
	    append tag .[dict get $attrs class]
	    dict unset attrs class
	}
	if {[dict exists $attrs id]} {
	    append tag #[dict get $attrs id]
	    dict unset attrs id
	}
	return $tag
    }

    method term {tag args} {
	set tag [my tagAttrs $tag args]
	return "$tag \{[my attr {*}$args]\}"
    }

    method naked {nattr args} {
	Debug.zengen {naked: $nattr $args}
	set sel {}
	foreach arg $args {
	    Debug.zengen {selector: '$arg'}
	    set attr [lassign $arg -> tag]
	    set s [my tagAttrs $tag attr]
	    if {[llength $attr]} {
		append s \[
		foreach {n v} $attr {
		    if {[string index $v 0] eq "~"} {
			append s n~=\"[string trimleft $v ~]\"
		    } else {
			append s n=\"$v\"
		    }
		}
		append s \]
	    }
	    lappend sel $s
	}

	return "[join $sel +] \{[my attr {*}$nattr]\}"
    }

    method child {parent args} {
	set attrs [lassign $parent -> tag]
	set tag [my tagAttrs $tag attrs]
	foreach arg $args {
	    lappend tag [my {*}$arg]
	}
	if {[llength $attrs]} {
	    return "[join $tag >] \{[my attr {*}$attrs]\}"
	} else {
	    return [join $tag >]
	}
    }

    superclass ::ZenGen
    constructor {args} {
	next {*}$args
    }
}

# ZenHTML is a class to generate HTML from zencode
oo::class create ::ZenHTML {
    method attr {args} {
	set result {}
	foreach {n v} [next {*}$args] {
	    lappend result $n='$v'
	}
	return $result
    }

    method term {tag args} {
	lappend result "<[join [list $tag {*}[my attr {*}$args]]]>"
	lappend result {*}[my value {*}$args]
	lappend result </$tag>
	return [join $result \n]
    }

    method child {parent args} {
	set attrs [lassign $parent -> tag]
	lappend result "<[join [list $tag {*}[my attr {*}$attrs]]]>"

	foreach arg $args {
	    lappend result [my {*}$arg]
	}

	lappend result </$tag>
	return [join $result \n]
    }

    superclass ::ZenGen
    constructor {args} {
	next {*}$args
    }
}

# Zen is a parser/compiler for zencode.
# [$zen parse $zencode] produces a tcl list
# [$zen generate $lang $zencode args...] generates $lang output
#  using the ::Zen$lang class and with args as an association list
#  for variable resolution within the $zencode
oo::class create Zen {
    method token {string {type wordchar}} {
	set failed 0
	set match [string is $type -strict -failindex failed $string]
	Debug.zenparse {token: string is $type until: $failed over '$string'}
	if {$match} {
	    return [list $string ""]	;# whole string matches
	} elseif {$failed} {
	    return [list [string range $string 0 $failed-1] [string range $string $failed end]]
	} else {
	    return [list "" $string]
	}
    }

    method literal {match_var rest_var} {
	upvar 1 $match_var match; set match ""
	upvar 1 $rest_var rest
	while {$rest ne ""} {
	    set next [string index $rest 0]
	    set rest [string range $rest 1 end]
	    if {$next eq "\\"} {
		append match $next
		append match [string index $rest 0]
		set rest [string range $rest 1 end]
	    } elseif {$next eq "\""} {
		break
	    } else {
		append match $next
	    }
	}
	return [expr {$match ne ""}]
    }

    method tokenize {match_var rest_var {type wordchar}} {
	upvar 1 $match_var match
	upvar 1 $rest_var rest
	set rest [string trim $rest]
	if {$rest eq ""} {
	    return 0
	}

	if {[string index $rest 0] eq "\""} {
	    Debug.zenparse {tokenize $type: looking for literal id}
	    set rest [string range $rest 1 end]
	    my literal match rest
	} else {
	    lassign [my token $rest $type] match rest
	    set match [string trim $match]
	    set rest [string trim $rest]
	    if {[string range $rest 0 1] eq "\\\$"} {
		set rest [string range $rest 2 end]
		my tokenize submatch rest
		append match \$ $submatch
	    } elseif {[string index $rest 0] eq ":"} {
		set rest [string range $rest 1 end]
		my tokenize submatch rest
		append match : $submatch
	    } elseif {[string index $rest 0] eq "-"} {
		set rest [string range $rest 1 end]
		my tokenize submatch rest
		append match - $submatch
	    }
	}

	Debug.zenparse {tokenize $type: '$match' '[string range $rest 0 10]'}
	return [expr {$match ne ""}]
    }

    method punct {rest_var} {
	upvar 1 $rest_var rest
	set rest [string trim $rest]
	set punct [string index $rest 0]
	Debug.zenparse {is '$punct' punct?}
	if {$punct in {. # * \{ \[ | > + ( ) \$ \"}} {
	    set rest [string range $rest 1 end]
	    Debug.zenparse {punct '$punct' '[string range $rest 0 10]'}
	    return $punct
	} else {
	    Debug.zenparse {punct failed over '$rest'}
	    return ""
	}
    }

    method get_attr {match_var rest_var {brace "\{"}} {
	upvar 1 $match_var match; set match $brace
	upvar 1 $rest_var rest
	while {![info complete $match]
	       && $rest ne ""
	   } {
	    append match [string index $rest 0]
	    set rest [string range $rest 1 end]
	}

	if {$match eq $brace} {
	    # no match
	    Debug.zenparse {get_attr failed}
	    return 0
	} else {
	    Debug.zenparse {get_attr match:($match) rest:'$rest'}
	    set match [join $match]
	    return 1
	}
    }

    method split_attr {attr} {
	set attr [string trim $attr]

	Debug.zenparse {split_attr:($attr)}
	set el ""; set result {}
	while {[set attr [string trimleft $attr]] ne ""} {
	    if {[string index $attr 0] eq "\""} {
		if {$el ne ""} {
		    error "improperly formed literal $attr"
		}
		set attr [string range $attr 1 end]
		if {[my literal literal attr]} {
		    Debug.zenparse {split_attr literal:($literal) ($attr)}
		    lappend result $literal
		} else {
		    error "unterminated literal in $attr"
		}
		continue
	    } elseif {[set space [string first " " $attr]] == -1} {
		Debug.zenparse {split_attr spaces $space:($attr)/$el/$result}
		append el $attr
		lappend result $el
		set el ""
		break	;# no spaces left
	    } else {
		append el [string range $attr 0 $space-1] " "
		set attr [string range $attr $space+1 end]
		Debug.zenparse {split_attr el: ($el) rest:($attr)}
	    }

	    if {[info complete $el]} {
		lappend result [string trim $el]
		set el ""
	    }
	}
	if {$el ne ""} {
	    error "improperly formed attributes - must be a valid tcl list"
	}

	if {[llength $result]%2} {
	    set extra [lindex $result end]
	    set result [lrange $result 0 end-1]
	    switch -- [string index $extra 0] {
		\[ {
		    lappend result -command [string trim $extra \[\]]
		}
		\" {
		    lappend result -literal [string trim $extra \"]
		}
		\$ {
		    lappend result -variable [string range $extra 1 end]
		}
		default {
		    lappend result -attr [lindex $extra 0]
		}
	    }
	}
	
	Debug.zenparse {split_attr result: ($result)}
	return $result
    }

    method compound {rest_var {nesting 0}} {
	upvar 1 $rest_var rest
	upvar 1 done_attrs done_attrs
	set $rest [string trim $rest]
	if {$rest eq ""} {
	    return ""
	}

	Debug.zenparse {compound '$rest'}

	# look for leading punctuation
	set punct [my punct rest]
	Debug.zenparse {compound punct: '$punct'}

	switch -- $punct {
	    "" {
		error "Can't parse '$rest' - no copula or compound punctuation"
	    }

	    . {
		# class
		if {![my tokenize class rest]} {
		    error "trailing '.' with no identifier"
		}
		return [list class $class]
	    }

	    \# {
		# id
		if {![my tokenize id rest]} {
		    error "trailing '#' with no identifier"
		}
		return [list id $id]
	    }

	    * {
		# multiplier
		if {![my tokenize mult rest integer]} {
		    set mult *
		}
		return [list mult $mult]
	    }

	    \" {
		if {![my literal lit rest]} {
		    error "failed string literal"
		}
		return [list -literal $lit]
	    }

	    \$ {
		# var
		if {![my tokenize id rest]} {
		    error "trailing '#' with no identifier"
		}
		return [list -variable $id]
	    }

	    \[ -
	    \{ {
		# attribute
		if {$punct eq "\{" && $done_attrs} {
		    set rest "\{$rest"
		    return ""
		}
		if {[my get_attr match rest $punct]} {
		    Debug.zenparse {compound attribute match:($match) '$rest'}
		    if {$punct eq "\["} {
			return [list -command [string trim $match \[\]]]
		    } else {
			set attrs [my split_attr $match]
			incr done_attrs
			return $attrs
		    }
		} else {
		    error "attribute: no close to match $punct parsing '$rest'"
		}
	    }
	    
	    | -
	    > -
	    + {
		# connector - not compound
		set rest ${punct}$rest
		return ""
	    }
	    
	    \( {
		error "misplaced '(' in $rest"
	    }

	    \) {
		# closed subexpr
		if {!$nesting} {
		    error "misplaced ')' in $rest"
		} else {
		    # how to close subexpr?
		    set rest \)$rest	;# push back the close subexpr
		    return ""
		}
	    }
	}
    }

    method compounding {rest_var {nesting 0}} {
	upvar 1 $rest_var rest
	set rest [string trim $rest]
	if {$rest eq ""} {
	    return ""
	}

	Debug.zenparse {compounding '$rest'}

	set result {}
	set done_attrs 0
	while {1} {
	    set compound [my compound rest $nesting]
	    if {$compound eq ""} {
		Debug.zenparse {complete compound '$compound' with '$rest' remaining}
		break
	    } else {
		Debug.zenparse {compounded '$compound' with '$rest' remaining}
	    }
	    lappend result {*}$compound
	}

	Debug.zenparse {compounded '$result' remaining '$rest'}
	return $result
    }

    method term {rest_var {nesting 0}} {
	upvar 1 $rest_var rest
	set rest [string trim $rest]
	if {$rest eq ""} {
	    return ""	;# return empty list for EOS
	}

	# look for leading term
	Debug.zenparse {looking for id in: '$rest'}
	my tokenize term rest
	if {$term ne ""} {
	    Debug.zenparse {leading term: '$term'}
	    set result [list term $term {*}[my compounding rest $nesting]]
	    Debug.zenparse {term is: '$result' remaining: $rest}
	    return $result	;# return term+compound dict
	} else {
	    # look for leading term punctuation - treat it as defaults
	    set punct [my punct rest]
	    Debug.zenparse {term punct: '$punct'}

	    switch -- $punct {
		. {
		    return [list default . {*}[my compounding rest $nesting]]
		}
		\# {
		    return [list default \# {*}[my compounding rest $nesting]]
		}

		* {
		    return [list term * {*}[my compounding rest $nesting]]
		}

		\( {
		    # start a new clause
		    Debug.zenparse {new subexpr: '$rest'}
		    #return [list subexpr [my parse_expr rest [incr nesting]]]
		    return [list subexpr [my parser rest [incr nesting]]]
		}

		> - + - \{ - \[ - \$ -
		\| {
		    error "naked '$punct' in '$rest'.  Expecting an identifier"
		}

		default {
		    error "unknown punctuation '$punct' in '$rest'"
		}
	    }
	}
    }

    # copula - having found an term/subexpr on the left,
    # find a copula (+,>) and an term on the right
    method copula {rest_var} {
	upvar 1 $rest_var rest
	set rest [string trim $rest]
	Debug.zenparse {looking for copula in '$rest'}
	if {$rest eq ""} {
	    return ""
	}

	# look for leading punctuation
	set punct [my punct rest]
	Debug.zenparse {copula punct: '$punct'}
	switch -- $punct {
	    > {
		return child
	    }

	    + {
		return sib
	    }

	    "\{" {
		return braced
	    }

	    "" -
	    \) {
		return $punct
	    }

	    default {
		error "unknown copula '$punct' in '$rest'"
	    }
	}
    }

    method parser {rest_var {nesting 0}} {
	upvar 1 $rest_var rest
	set rest [string trim $rest]
	if {$rest eq ""} {
	    return ""
	}

	Debug.zenparse {parser $nesting over: '$rest'}
	
	# get lhs term
	set result [list [my term rest $nesting]]
	Debug.zenparse {parse lhs: '$result', rest: '$rest'}

	while {$rest ne ""} {
	    Debug.zenparse {parse looking for copula in '$rest'}
	    set copula [my copula rest]
	    Debug.zenparse {parse copula: '$copula', rest: '$rest'}

	    switch -- $copula {
		child -
		sib {
		    # get rhs term/phrase
		    set rhs [my term rest $nesting]
		    Debug.zenparse {parsed $copula rhs: '$rhs', rest: '$rest'}

		    lappend result $copula $rhs
		}

		braced {
		    # naked attributes
		    Debug.zenparse {naked attributes: '$rest'}
		    my get_attr match rest
		    lappend result naked $match
		}

		\) {
		    if {$nesting == 0} {
			error "Extraneous trailing \) in $rest"
		    }
		    return $result
		}

		"" {
		    Debug.zenparse {parsed: $result rest:'$rest'}
		    if {$nesting != 0} {
			error "No closing \) in $rest"
		    }
		    return $result
		}

		default {
		    error "unknown copula '$copyla'"
		}
	    }
	}

	Debug.zenparse {completed: $result}
	return $result
    }

    method compile {result} {
	Debug.zenparse {compile: '$result'}
	set cmd {}

	foreach {el op} $result {
	    set result [lrange $result 2 end]
	    Debug.zenparse {compiling el:($el) op:'$op'}
	    if {[dict exists $el mult]} {
		# handle mult subexpr
		set mult [dict get $el mult]
		dict unset el mult
		Debug.zenparse {compiling a mult '$mult' ($el $op $result)}
		lappend cmd [list [list mult $mult {*}[my compile [list $el $op {*}$result]]]]
		break
	    }

	    if {$op eq "child"} {
		# 'child' operation
		Debug.zenparse {compile child '$el' rest:'$result'}
		if {[lindex $el 0] eq "subexpr"} {
		    Debug.zenparse {child subexpr ([lrange $el 1 end])}
		    set se {}
		    foreach sub [lrange $el 1 end] {
			lappend se {*}[my compile $sub]
		    }
		    set el [join $se]
		}
		lappend cmd [list [list child $el {*}[my compile $result]]]
		break
	    } elseif {$op eq "naked"} {
		Debug.zenparse {compile naked '$el' rest:'$result'}
		if {[lindex $el 0] eq "subexpr"} {
		    Debug.zenparse {naked subexpr ([lrange $el 1 end])}
		    set se {}
		    foreach sub [lrange $el 1 end] {
			lappend se [my compile $sub]
		    }
		    set el [join $se]
		    lappend cmd [list [list naked {*}[my compile $result] {*}$el]]
		} else {
		    lappend cmd [list [list naked {*}[my compile $result] $el]]
		}
		break
	    } else {
		# 'sibling' operation
		if {[lindex $el 0] eq "subexpr"} {
		    Debug.zenparse {compiling a subexpr ([lrange $el 1 end])}
		    set se {}
		    foreach sub [lrange $el 1 end] {
			lappend cmd [my compile $sub]
		    }
		} else {
		    lappend cmd [list $el]
		}
	    }
	}

	Debug.zenparse {compile joining: '$cmd'}
	set cmd [join $cmd]
	Debug.zenparse {compiled: '$cmd'}
	return $cmd
    }

    method parse {rest} {
	set rest [string trim $rest]
	Debug.zenparse {parser over: '$rest'}
	if {$rest eq ""} {
	    return ""
	}
	set result [my parser rest]
	return [my compile $result]
    }

    method generate {language rest args} {
	set generator [::Zen$language new {*}$args]
	set generated [$generator generate {*}[my parse $rest]]
	$generator destroy
	return $generated
    }

    destructor {}
    constructor {args} {
	variable {*}$args
    }
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    package require tcltest
    namespace import ::tcltest::*

    variable SETUP {Zen create zen}
    variable CLEANUP {zen destroy}

    tcltest::skip unsupported-*

    proc upto {top index} {
	Debug.zengen {upto $top $index}
	if {$index > $top} {error done}
	return [expr {$top - $index}]
    }

    # these are facilities we don't support
    set count 0
    foreach {from to} {
	p.title|e "No Filters"
	p.name-$*3 "No anonymous index vars"
	select>option#item-$*3 "No anonymous index vars"
	ul+ "no default expansion"
	table+ "no default expansion"
    } {
	incr count
	test unsupported-$count {} -setup $SETUP -body [list zen parse $from] -cleanup $CLEANUP -result $to
    }

    # test parsing of simple Zencode
    set count 0
    foreach {from to} {
	div#name {{term div id name}}
	div.class {{term div class class}}
	div.one.two {{term div class one class two}}
	div#name.one.two {{term div id name class one class two}}
	head>link {{child {term head} {term link}}}
	table>tr>td {{child {term table} {child {term tr} {term td}}}}
	ul#name>li.item {{child {term ul id name} {term li class item}}}
	p+p {{term p} {term p}}
	{div#page>div.logo+ul#navigation>li*5>a} {{child {term div id page} {term div class logo} {child {term ul id navigation} {mult 5 {child {term li} {term a}}}}}}
	{div#page>div.logo+ul#navigation>li*>a{$list}} {{child {term div id page} {term div class logo} {child {term ul id navigation} {mult * {child {term li} {term a -variable list}}}}}}
	div#name>p.one+p.two {{child {term div id name} {term p class one} {term p class two}}}
	p*3 {{mult 3 {term p}}}
	ul#name>li.item*3 {{child {term ul id name} {mult 3 {term li class item}}}}
	ul#name>li.item*3$var {{child {term ul id name} {mult 3 {term li class item -variable var}}}}
    } {
	incr count
	test simple-$count {} -setup $SETUP -body [list zen parse $from] -cleanup $CLEANUP -result $to
    }

    # test parsing of Tcl Zencode extensions
    set count 0
    foreach {from to} {
	{(div)>p} {{child {term div} {term p}}}
	{div>(ul>li)+p} {{child {term div} {child {term ul} {term li}} {term p}}}
	{div>(title+ul>li)+p} {{child {term div} {term title} {child {term ul} {term li}} {term p}}}
	p{title} {{term p -attr title}}
	td{colspan=2} {{term td -attr colspan=2}}
	{td{colspan 2}} {{term td colspan 2}}
	{td{colspan 2 $content_var}} {{term td colspan 2 -variable content_var}}
	{td{colspan 2 [content script]}} {{term td colspan 2 -command {content script}}}
	{td[content script; more script]} {{term td -command {content script; more script}}}
	{td$variable} {{term td -variable variable}}
	{span{title "Hello World" rel}} {{term span title {Hello World} -attr rel}}
	{span"Moop Moop"} {{term span -literal {Moop Moop}}}
	{span "Moop Moop"} {{term span -literal {Moop Moop}}}
    } {
	incr count
	test extended-$count {} -setup $SETUP -body [list zen parse $from] -cleanup $CLEANUP -result $to
    }

    # test HTML generation
    set count 0
    foreach {from to} {
	p+p "<p>\n</p>\n<p>\n</p>"
	p$line+p "<p>\nthis is a line\n</p>\n<p>\n</p>"
	{p "moop1" + p "moop2"} "<p>\nmoop1\n</p>\n<p>\nmoop2\n</p>"
	head>link "<head>\n<link>\n</link>\n</head>"
	{div>(ul>li)+p} "<div>\n<ul>\n<li>\n</li>\n</ul>\n<p>\n</p>\n</div>"
	{div>(ul>li)+p "moop"} "<div>\n<ul>\n<li>\n</li>\n</ul>\n<p>\nmoop\n</p>\n</div>"
	{div>(ul>li$list)+p "moop"} "<div>\n<ul>\n<li>\na list of words\n</li>\n</ul>\n<p>\nmoop\n</p>\n</div>"
	{ul>li#id\$simple*3$simple} "<ul>\n<li id='id0'>\n0\n</li>\n<li id='id1'>\n1\n</li>\n<li id='id2'>\n2\n</li>\n</ul>"
	{ul>li#id\$_*$list} "<ul>\n<li id='id0'>\na\n</li>\n<li id='id1'>\nlist\n</li>\n<li id='id2'>\nof\n</li>\n<li id='id3'>\nwords\n</li>\n</ul>"
	{ul>li#id\$_*[upto 5 $_]} "<ul>\n<li id='id0'>\n5\n</li>\n<li id='id1'>\n4\n</li>\n<li id='id2'>\n3\n</li>\n<li id='id3'>\n2\n</li>\n<li id='id4'>\n1\n</li>\n<li id='id5'>\n0\n</li>\n</ul>"
	{ul>li#"id[upto 5 $_]"*[upto 5 $_]} "<ul>\n<li id='id5'>\n5\n</li>\n<li id='id4'>\n4\n</li>\n<li id='id3'>\n3\n</li>\n<li id='id2'>\n2\n</li>\n<li id='id1'>\n1\n</li>\n<li id='id0'>\n0\n</li>\n</ul>"
    } {
	incr count
	test html-$count {} -setup $SETUP -body [list zen generate HTML $from line "this is a line" list {a list of words} simple simple] -cleanup $CLEANUP -result $to
    }

    # test CSS generation
    # selector [, selector2, ...][:pseudo-class] {
    # 	property: value;
    # 	[property2: value2;
    # 	...]
    # }
    # /* comment */
    set count 0
    foreach {from to} {
	"h1{color white background-color orange}" "h1 {color: white; background-color: orange;}"
	"h1{color white background-color {orange !important}}" "h1 {color: white; background-color: orange !important;}"
	"h1.class#id{color white background-color {orange !important}}" "h1.class#id {color: white; background-color: orange !important;}"
	"div>ul>li {color white}" "div>ul>li {color: white;}"
	{"h1 h2" {color white}} "h1 h2 {color: white;}"
	"* {color white}" {* {color: white;}}
	"(h1+h2) {color white}" "h1+h2 {color: white;}"
	{E{foo warning}{color white}} {E[n="warning"] {color: white;}}
	{E} {E {}}
	{"E F"} {E F {}}
	{E > F} {E>F {}}
	{E:first-child} {E:first-child {}}
	{E+F {color white}} .
	{"E+F" {color white}} {E+F {color: white;}}
    } {
	incr count
	test css-$count {} -setup $SETUP -body [list zen generate CSS $from] -cleanup $CLEANUP -result $to
    }

    # To see test statistics (Total/Passed/Skipped/Failed), best put this line in the end:
    cleanupTests
}
