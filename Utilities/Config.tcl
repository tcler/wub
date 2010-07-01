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
    method section {script} {
	set raw {}	;# association between name and tcl script giving value
	set comments {}	;# association between name and run-on comments
	set metadata {}	;# association between name and metadata

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
		    dict lappend metadata $bll [parsetcl unparse $md]
		}

		set brl [parsetcl unparse $br]
		dict set raw $bll $brl
		Debug.config {BCMD $bi: ($bl) ($br) '$brl' - MD:($metadata)}
	    } elseif {[parsetcl unparse $bl] eq "expr"} {
		Debug.config {EXPR: ($br) -> ([parsetcl parse_semiwords [parsetcl unparse $br]])}
	    }
	} Nc {
	    # comment
	    dict lappend comments $bll [lindex $body {*}$bi]
	} Ne {
	    set cmd [lindex $parse {*}$index]
	    error "Syntax Error - $cmd"
	}

	Debug.config {section: raw:($raw)}

	return [list $raw $comments $metadata]
    }

    # parse a complete configuration into raw, comments and metadata
    method parse {script} {
	set raw {}	;# association between name and tcl script giving value
	set comments {}	;# association between name and run-on comments
	set metadata {}	;# association between name and metadata

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
	    lassign [my section $sb] sraw scomments smetadata
	    dict set raw $ll $sraw
	    dict set comments $ll $scomments
	    dict set metadata $ll $smetadata
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
	return [list $raw $comments $metadata]
    }

    # destroy context namespace
    method clear {} {
	namespace delete _C
	namespace eval _C {}
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

    # merge_section - merge a section dict after variable substitution
    method merge_section {section vars} {
	set ss {}
	dict for {n v} $vars {
	    set sv [my VarSub $v]
	    Debug.config {extract: $n $v ($sv)}
	    namespace eval _C::$section "variable $n $sv"
	}
    }

    # merge a raw dict after variable substitution
    method merge {raw} {
	dict for {section vars} $raw {
	    my merge_section $section $vars
	}
    }

    # sections - a list of sections
    method sections {} {
	set result {}
	foreach section [namespace children _C] {
	    lappend result [namespace tail $section]
	}
	return $result
    }

    # extract naming context from configuration and aggregated namespace
    method extract {{config ""}} {
	if {$config ne ""} {
	    # parse $config if proffered
	    lassign [my parse $config] raw comments metadata
	    my merge $raw
	}

	# extract the accumulated values from _C namespace children
	set result {}
	foreach ns [namespace children _C] {
	    foreach var [info vars ${ns}::*] {
		dict set result [namespace tail $ns] [namespace tail $var] [set $var]
	    }
	}

	return $result
    }

    # parse and merge a file in config format
    method section_file {section file} {
	package require fileutil
	set result [my section [::fileutil::cat -- $file]]
	my merge [list $section [lindex $result 0]]
	return $result
    }

    # parse and merge a file in config format
    method file {file} {
	package require fileutil
	set result [my parse [::fileutil::cat -- $file]]
	my merge [lindex $result 0]
	return $result
    }

    constructor {args} {
	Debug.config {Creating Config [self] $args}
	if {[llength $args]%2} {
	    set cf [lindex $args end]
	    set args [lrange $args 0 end-1]
	    dict set args config $cf
	}
	variable {*}$args
	catch {set args [dict merge [Site var? Config] $args]}	;# allow .ini file to modify defaults

	namespace eval _C {}	;# construct empty subnamespace

	if {[info exists file]} {
	    my file $file	;# parse 
	}
	if {[info exists config]} {
	    lassign [my parse $config] raw comments metadata
	    my merge $raw
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
