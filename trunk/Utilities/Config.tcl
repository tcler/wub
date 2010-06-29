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
    method section {script {varwalk 0}} {
	set body [parsetcl simple_parse_script $script]
	set NS [info object namespace [self]]

	# perform variable rewrite
	if {$varwalk} {
	    parsetcl walk_tree body bi Sv {
		set s [lindex $body {*}$bi 3 2]
		if {![string match ::* $s] && [string match *::* $s]} {
		    # this is section-relative var - we need to make a fully qualified NS
		    set s "${NS}::_C::$s"
		    lset body {*}$bi 3 2 $s
		}
		Debug.config {VAR: $s}
	    } Ne {
		# syntax error
		set cmd [lindex $body {*}$index]
		error "Syntax Error - $cmd"
	    }
	}

	# perform script transformation
	set rb {}
	set comments {}
	set raw {}

	parsetcl walk_tree body bi Rs {} C.* {
	    set bcmd [lindex $body {*}$bi]
	    lassign $bcmd . . . bl br
	    if {[llength $bi] == 1} {
		set bll [parsetcl unparse $bl]
		if {![string match L* [lindex $bl 0]]} {
		    error "variable name '$bll' must be a literal ($bl)"
		}
		if {[llength $bcmd] != 5} {
		    error "variable $bll must have one argument only"
		}
		set brl [parsetcl unparse $br]
		Debug.config {BCMD $bi: ($bl) ($br) '$brl'}

		dict set raw $bll $brl
		lappend rb "variable $bll $brl"
	    } elseif {[parsetcl unparse $bl] eq "expr"} {
		Debug.config {EXPR: ($br) -> ([parsetcl parse_semiwords [parsetcl unparse $br]])}
	    }
	} Nc {
	    # comment
	    dict set comments $bll [lindex $body {*}$bi]
	} Ne {
	    set cmd [lindex $parse {*}$index]
	    error "Syntax Error - $cmd"
	}

	Debug.config {section: ($rb)}

	return [list [join $rb \;] $raw $comments]
    }

    method parse {script {varwalk 0}} {
	set parse [parsetcl simple_parse_script $script]
	#puts stderr "Parse: $parse"
	#puts stderr "Format: [parsetcl format_tree $parse { } {   }]"
	#puts stderr "UnParse: [parsetcl unparse $parse]"
	set rendered {}
	set raw {}
	set comments {}
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
	    lassign [my section $sb $varwalk] rb sraw scomments
	    dict set raw $ll $sraw
	    dict set comments $ll $scomments
	    lappend rendered "namespace eval $ll [list $rb]"
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

	Debug.config {RENDERED: $rendered}
	set rendered [join $rendered \;]
	return [list $raw $comments $rendered]
    }

    # destroy any 
    method clear {} {
	namespace delete _C
    }

    method extract {{config ""}} {
	lassign [my parse $config 1] raw comments rendered
	namespace eval _C $rendered
	set result {}
	foreach ns [namespace children _C] {
	    foreach var [info vars ${ns}::*] {
		dict set result [namespace tail $ns] [namespace tail $var] [set $var]
	    }
	}
	return $result
    }

    method file {file} {
	package require fileutil
	my parse [::fileutil::cat -- $file]
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

	if {[info exists config]} {
	    my parse $config
	}
	if {[info exists file]} {
	    my file $file
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
	    ap [list moop {*}$::auto_path]
	}
	sect_err {
	    xy {this is an error $moop(m}
	}
    }]"
}
