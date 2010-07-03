# Config.tcl - support tcl-like config files

if {[info exists argv0] && ($argv0 eq [info script])} {
    lappend auto_path ~/Desktop/Work/Wub/ [file dirname [info script]]
}

if {[catch {package require Debug}]} {
    #proc Debug.config {args} {}
    proc Debug.config {args} {puts stderr HTTP@[uplevel subst $args]}
} else {
    Debug define config 10
}

package require parsetcl
package provide Config 1.0

set ::API(Utilities/Config) {
    {
	Configuration parser
    }
}

oo::class create Config {
    # parse a single section into raw alist, comments and metadata
    method parse_section {section script} {
	variable raw; variable comments; variable metadata
	variable clean 0

	# perform script transformation
	set bll ""	;# initial name used to collect per-section comments
	set body [parsetcl simple_parse_script $script]
	parsetcl walk_tree body bi Rs {} C.* {
	    set bcmd [lindex $body {*}$bi]
	    if {[llength $bi] == 1} {
		set metad [lassign $bcmd . . . bl br]
		set bll [parsetcl unparse $bl]	;# literal name
		if {![string match L* [lindex $bl 0]]} {
		    error "variable name '$bll' must be a literal ($bl)"
		}

		foreach md $metad {
		    dict set metadata $section $bll [parsetcl unparse $md]
		}

		set brl [parsetcl unparse $br]
		dict set raw $section $bll $brl
		Debug.config {BCMD $bi: ($bl) ($br) '$brl' - MD:($metadata)}
	    } elseif {[parsetcl unparse $bl] eq "expr"} {
		Debug.config {EXPR: ($br) -> ([parsetcl parse_semiwords [parsetcl unparse $br]])}
	    }
	} Nc {
	    # comment
	    dict set comments $section $bll [lindex $body {*}$bi]
	} Ne {
	    set cmd [lindex $parse {*}$index]
	    error "Syntax Error - $cmd"
	}

	Debug.config {section: raw:($raw)}
    }

    # parse a file in config format
    method load_section {section file} {
	package require fileutil
	my parse_section $section [::fileutil::cat -- $file]
    }

    # merge raw, comments and metadata dicts for given section
    method merge_section {section args} {
	variable raw; variable comments; variable metadata
	variable clean 0
	lassign $args _raw _comments _metadata
	dict set raw $section [dict merge [dict get $raw $section] $_raw]
	dict set comments $section [dict merge [dict get $comments $section] $_comments]
	dict set metadata $section [dict merge [dict get $metadata $section] $_metadata]
    }

    # parse a complete configuration into raw, comments and metadata
    method parse {script} {
	variable raw; variable comments; variable metadata

	set parse [parsetcl simple_parse_script $script]
	parsetcl walk_tree parse index Cd {
	    # body
	    #puts stderr "walk: [lindex $parse {*}$index]"
	    set cmd [lindex $parse {*}$index]
	    lassign $cmd . . . left right
	    set ll [parsetcl unparse $left]
	    if {![string match L* [lindex $left 0]]} {
		error "section name '$ll' must be a literal ($left)"
	    }

	    if {[llength $cmd] != 5} {
		error "section $ll must have one argument only"
	    }

	    # get body of section (with var substitution)
	    set sb [lindex [parsetcl unparse $right] 0]
	    my parse_section $ll $sb
	} C.* {
	    error "Don't know how to handle [lindex $parse {*}$index]"
	} Nc {
	    # comment
	    set cmd [lindex $parse {*}$index]
	    #puts stderr "Comment - $cmd"
	} Ne {
	    set cmd [lindex $parse {*}$index]
	    error "Syntax Error - $cmd"
	}

	Debug.config {parse: ($raw)}
    }

    # merge a raw, commants and metadata dicts
    method merge {args} {
	lassign $args _raw _comments _metadata
	foreach section [dict keys $_raw] {
	    my merge_section $section [dict get $_raw $section] [dict get? $_comments $section] [dict get? $_metadata $section]
	}
    }

    # parse a file in config format
    method load {file} {
	package require fileutil
	my parse [::fileutil::cat -- $file]
    }

    # substitute section-relative names into value scripts
    method VarSub {script} {
	set NS [info object namespace [self]]

	# perform variable rewrite
	set body [parsetcl simple_parse_script $script]
	parsetcl walk_tree body bi Sv {
	    set s [lindex $body {*}$bi 3 2]
	    if {![string match ::* $s] && [string match *::* $s]} {
		# this is section-relative var - we need to make a fully qualified NS
		set s "${NS}::_C::$s"
		lset body {*}$bi 3 2 $s
	    }
	    Debug.config {Var: $s}
	}
	set subbed [parsetcl unparse $body]
	set subbed [lindex [split $subbed \n] 1]	;# why is this necessary?
	Debug.config {VarSub: '$script' -> '$subbed'}
	return $subbed
    }

    # eval_section - evaluate a section dict after variable substitution
    method eval_section {section} {
	variable raw
	set ss {}
	dict for {n v} [dict get $raw $section] {
	    set sv [my VarSub $v]
	    Debug.config {eval_section '$section': $n $v ($sv)}
	    namespace eval _C::$section "variable $n $sv"
	}
    }

    # evaluate a raw dict after variable substitution
    method eval {} {
	variable clean
	variable raw
	if {!$clean} {
	    # only re-evaluate if not clean
	    foreach section [dict keys $raw] {
		my eval_section $section
	    }
	}
	set clean 1
    }

    # sections - a list of sections
    method sections {} {
	variable raw; return [dict keys $raw]
    }

    # section - get evaluated section
    method section {section} {
	my eval	;# evaluate any changes in raw
	set result {}
	foreach var [info vars _C::${section}::*] {
	    dict set result [namespace tail $var] [set $var]
	}
	return $result
    }

    # extract naming context from configuration and aggregated namespace
    method extract {{config ""}} {
	if {$config ne ""} {
	    # parse $config if proffered
	    my parse $config
	}

	my eval	;# evaluate any changes in raw

	# extract the accumulated values from _C namespace children
	set result {}
	foreach section [my sections] {
	    dict set result $section [my section $section]
	}

	return $result
    }

    # raw - access raw values
    method raw {{section ""}} {
	variable raw
	if {$section eq ""} {
	    return $raw
	} else {
	    return [dict get $raw $section]
	}
    }

    # comments - access comment values
    method comments {{section ""}} {
	variable comments
	if {$section eq ""} {
	    return $comments
	} else {
	    return [dict get $comments $section]
	}
    }

    # metadata - access metadata values
    method metadata {{section ""}} {
	variable metadata
	if {$section eq ""} {
	    return $metadata
	} else {
	    return [dict get $metadata $section]
	}
    }

    # destroy context namespace
    method clear {} {
	variable raw {}	;# association between name and tcl script giving value
	variable comments {}	;# association between name and run-on comments
	variable metadata {}	;# association between name and metadata

	# destroy evaluation namespace
	catch {namespace delete _C}
	namespace eval _C {}
	variable clean 1
    }

    constructor {args} {
	Debug.config {Creating Config [self] $args}
	if {[llength $args]%2} {
	    set cf [lindex $args end]
	    set args [lrange $args 0 end-1]
	    dict set args config $cf
	}
	variable {*}$args
	#catch {set args [dict merge [Site var? Config] $args]}	;# allow .ini file to modify defaults -- config is, itself, not Configurable

	my clear	;# start with a clean slate

	if {[info exists file]} {
	    my load $file	;# parse any file passed in
	}

	if {[info exists config]} {
	    my parse $config	;# parse any literal config passed in
	}
    }
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    Debug on config 10

    Config create config
    puts stderr "DICT: [config extract {
	section {
	    var val	;# this is a variable
	    var1 val1
	    v1 2
	    v2 [expr {$v1 ^ 2}]
	}
	# another section 
	sect1 {
	    v1 [expr {$section::v1 ^ 10}]
	    ap [list moop {*}$::auto_path] -moop moopy
	}
	sect_err {
	    xy {this is an error $moop(m}
	}
    }]"
}
