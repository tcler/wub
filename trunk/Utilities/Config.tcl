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
    method parse {script} {
	set parse [parsetcl simple_parse_script $script]
	#puts stderr "Parse: $parse"
	#puts stderr "Format: [parsetcl format_tree $parse { } {   }]"
	#puts stderr "UnParse: [parsetcl unparse $parse]"
	set rendered {}

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

	    # get body
	    set body [parsetcl simple_parse_script [lindex [parsetcl unparse $right] 0]]
	    set NS [info object namespace [self]]

	    # perform variable rewrite
	    parsetcl walk_tree body bi Sv {
		set s [lindex $body {*}$bi 3 2]
		if {![string match ::* $s] && [string match *::* $s]} {
		    # this is section-relative var - we need to make a fully qualified NS
		    set s "${NS}::_C::$s"
		    lset body {*}$bi 3 2 $s
		}
		#puts stderr "VAR: $s"
	    }

	    # perform script transformation
	    set rb ""
	    parsetcl walk_tree body bi Rs {} C.* {
		set bcmd [lindex $body {*}$bi]
		lassign $bcmd . . . bl br
		if {[llength $bi] == 1} {
		    set bll [parsetcl unparse $bl]
		    if {![string match L* [lindex $bl 0]]} {
			error "section name '$bll' must be a literal ($bl)"
		    }
		    if {[llength $bcmd] != 5} {
			error "section $bll must have one argument only"
		    }
		    set brl [parsetcl unparse $br]
		    #puts stderr "BCMD $bi: ($bl) ($br) '$brl'"
		    
		    lappend rb "variable $bll $brl"
		} elseif {[parsetcl unparse $bl] eq "expr"} {
		    #puts stderr "EXPR: ($br) -> ([parsetcl parse_semiwords [parsetcl unparse $br]])"
		}
	    }
	    
	    lappend rendered "namespace eval $ll [list [join $rb \;]]"

	} C.* {
	    error "Don't know how to handle [lindex $parse {*}$index]"
	}

	set rendered [join $rendered \;]
	#puts stderr "RENDERED: $rendered"
	namespace eval _C $rendered
	return $parse
    }

    method extract {{config ""}} {
	my parse $config
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
    }]"
}
