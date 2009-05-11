# Wub module for Woof
package require Debug
Debug off woof 10

package provide Woof 1.0

class create ::Woof {

    # call woof to process URL
    method do {r} {
	# calculate the suffix of the URL relative to $mount
	lassign [Url urlsuffix $r $mount] result r suffix path
	if {!$result} {
	    return $r	;# the URL isn't in our domain
	}
	dict set r -info /$suffix
	dict set r -translated /$suffix
	set fpath [file join $root public [string trimleft $suffix /]]
	Debug.woof {Woof do [self] $suffix $path - fpath:$fpath}

	if {[file exists $fpath]} {
	    set r [$file do $r]
	} else {
	    ::woof::handle_request $r
	    dict set r -passthrough 1	;# response is generated already
	}

	return $r
    }

    destructor {
	catch {$file destroy}
    }

    variable mount file root
    constructor {args} {
	Debug.woof {Woof constructing [self] $args}

	foreach {n v} $args {
	    set [string trimleft $n -] $v
	}
	set mount /[string trim $mount /]/
	set file [File new mount $mount root [file join $root public]]	;# construct a File to handle real files
    }
}

proc woofLoad {woofdir root mount} {
    lappend ::auto_path $woofdir
    package require woof
    ::woof::init wub_server	$root ;# start up woof
    ::woof::config set url_root [string trimright $mount /]
}



if {0} {
    ::woof::init wub_server	;# start up woof

    # construct a nub for Woof
    set wmp /[string trim [::woof::config get url_root] /]/
    Nub domain $wmp Woof
}
