# Url - support for URL manipulation

package require Query
package require Dict

package provide Url 1.0

namespace eval Url {

    # flatten the -path into a -suffix
    proc flatten {req} {
	dict set req -suffix [file tail [dict get $req -path]]
	return $req
    }

    # strip off path prefix - from ::fileutil
    proc pstrip {prefix path} {
	Debug.url {pstrip $prefix $path}
	# [file split] is used to generate a canonical form for both
	# paths, for easy comparison, and also one which is easy to modify
	# using list commands.
	set trailing [expr {([string index $path end] eq "/")?"/":""}]

	# canonicalise the paths: no bracketing /, no multiple /
	set prefix [string trim [file join $prefix] /]
	set path [string trim [file join $path] /]

	if {[string equal $prefix $path]} {
	    return "/"	;# if the paths are canonically string equal, we're sweet
	}

	# split the paths into components
	set prefix [file split $prefix]
	set npath [file split $path]

	# strip non-matching prolog
	while {[llength $npath] && ![string match ${prefix}* $npath]} {
	    set npath [lrange $npath 1 end]	;# trim off an element
	}
	# $npath is empty or they match.

	# now check if there's a match
	if {[llength $npath]} {
	    # ergo there's a match - preserve dir suffix
	    set match [file join {*}[lrange $npath [llength $prefix] end] {}]$trailing
	    Debug.url {pstrip match [file join $prefix] + [file join $npath] -> $match}
	    return $match
	} else {
	    # the prefix doesn't match ... try stripping some leading prefix
	    Debug.url {pstrip no match [file join $prefix] $path}
	    return /$path
	}
    }

    # normalize -- 
    #
    #	collapse and normalize //, ../ and . components to avoid tricks
    #	like //cgi-bin that fail to match the /cgi-bin prefix
    #	and ../ that escape domains
    #
    # Arguments:
    #	args	url to normalize
    #
    # Results:
    #	normalized url
    #
    # Side Effects:
    #	none

    proc normalize {url} {
	while {[set new [regsub -all {(/+)|(^[.][.]/)|(^/[.][.])|(/[^/]+/[.][.]$)|(/[^/]+/[.][.]/)|(^[.]/)|(/[.]$)|(/[.]/)|(^[.][.]$)|(^[.]$)} $url /]] ne $url} {
	    set url $new
	}
	return "/[string trimleft $url /]"
    }

    # parse -- 
    #
    #	parse a url into its constituent parts
    #
    # Arguments:
    #	args	url to parse
    #
    # Results:
    #	array form of parsed URL elements
    #
    # Side Effects:
    #	none

    proc parse {url {normalize 1}} {
	Debug.url {Url parse $url - norm? $normalize}
	array set x {}
	regexp {^(([^:/?\#]+):)?(//([^/?\#]*))?([^?\#]*)([?]([^\#]*))?(\#(.*))?$} $url \
	    -> . x(-scheme) . x(-authority) x(-path) . x(-query) . x(-fragment)
	regexp {^(([^@]+)@)?([^@:]+)?(:([0-9]+))?$} $x(-authority) \
	    -> . x(-authority) x(-host) . x(-port)

	if {$normalize} {
	    set x(-path) [normalize $x(-path)]	;# fix up oddities in URLs
	}

	foreach n [array names x] {
	    if {$x($n) eq ""} {
		unset x($n)
	    }
	}

	Debug.url {Url parse post regexp: [array get x]}	
	#puts stderr "Url parse post regexp: [array get x]"

	if {[info exists x(-scheme)]} {
	    set x(-url) [url [array get x]]
	}

	Debug.url {Url parse: $url -> [array get x]} 30
	#puts stderr "Url parse: $url -> [array get x]"

	return [array get x]
    }

    proc Parse {url} {
	set result {}
	dict for {k v} [parse $url] {
	    lappend result [string trim $k -] $v
	}
	return $result
    }

    proc url {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	Debug.url {Url url $args}

	# minimize -port
	if {[dict exists $args -port]
	    && ([dict get $args -port] eq "" || [dict get $args -port] eq "80")} {
	    dict unset args -port
	}

	foreach {part pre post} {
	    -scheme "" :
	    -host // ""
	    -port : ""
	    -path "" ""
	} {
	    if {[dict exists $args $part]} {
		append result "${pre}[dict get $args $part]${post}"
	    }
	}
	#puts stderr "Url url $args -> $result"
	return $result
    }

    proc uri {x args} {
	set result [url $x]

	foreach {part pre post} {
	    -query ? ""
	    -fragment \# ""
	} {
	    if {[dict exists $x $part]} {
		append result "${pre}[dict get $x $part]${post}"
	    }
	}
	return $result
    }

    # construct the host part of a URL dict
    proc host {x} {
	if {[dict exists $x -port]
	    && [dict get $x -port] ne {}
	    && [dict get $x -port] != 80} {
	    return "[dict get $x -host]:[dict get $x -port]"
	} else {
	    return "[dict get $x -host]"
	}
    }

    # construct a URL from a URL dict
    proc http {x args} {
	Debug.url {Url http $x}

	foreach {part pre post} {
	    -path "" ""
	    -fragment \# ""
	    -query ? ""
	} {
	    if {[dict exists $x $part]} {
		append result "${pre}[dict get $x $part]${post}"
	    }
	}
	#puts stderr "Url url $x -> $result"
	return $result
    }

    # insert a fully expanded path, uri and url into a request
    proc path {req path} {
	dict set req -path $path
	dict set req -url [url $req]
	dict set req -uri [uri $req]
	return $req
    }

    # process a possibly local URI for redirection
    # provides a limited ability to add query $args
    # limits: overwrites existing args, ignores and removes duplicates
    proc redir {dict to args} {
	Debug.url {redir dict:$dict to:$to args:$args}
	#puts stderr "redir dict:($dict) to:$to args:$args"
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}

	set todict [parse $to 0]	;# parse the destination URL

	if {[dict exists $todict -query]} {
	    foreach {n v} [Query flatten [Query parse $todict]] {
		dict set query $n $v
	    }
	} else {
	    set query {}
	}

	# parse args as additional -query elements
	foreach {name val} $args {
	    dict set query $name $val
	}

	set q ""
	dict for {n v} $query {
	    if {$v ne ""} {
		lappend q "$n=[Query encode $v]"
	    } else {
		lappend q $n
	    }
	}
	if {$q ne {}} {
	    dict set todict -query [join $q &]
	}

	if {([Dict get? $todict -host] ne [Dict get? $dict -host])
	    || ([Dict get? $todict -port] ne [Dict get? $dict -port])
	} {
	    # this is a remote URL
	    set to [uri $todict]
	} else {
	    # local URL
	    set npath [dict get $todict -path]
	    if {[file pathtype $npath] eq "relative"} {
		set npath [normalize [file join [dict get $dict -path] $npath]]
	    }

	    set host [dict get $dict -host]
	    set port [dict get $dict -port]
	    set to [uri [dict replace $todict \
			     -path $npath \
			     -host $host \
			     -port $port]]
	}

	return $to
    }

    # change the suffix of a request
    proc suffix {x suffix} {
	dict set x -suffix $suffix
	dict set x -path [file join [dict get $x -prefix] $suffix]
	dict set x -url [url $x]
	dict set x -uri [uri $x]
	return $x
    }

    # subset a dict's URL relevant elements
    proc subset {dict args} {
	if {$args eq {}} {
	    set args {-scheme -authority -host -port -query -fragment -path -url -uri}
	}
	return [Dict subset $dict {*}$args]
    }

    # assemble: given a dict containing URL components, assemble a URL
    # optionally remove some extraneous fields
    proc assemble {dict args} {
	if {$args eq {}} {
	    set args {-fragment -authority -query}
	}
	catch {Dict defaults dict -scheme http}
	catch {Dict strip dict {*}$strip}	;# remove extraneous fields
	dict with [Dict trimkey [subset $dict]] {
	    set result ${scheme}://
	    if {[info exists authority]} {
		append result $authority
	    }
	    append result $host
	    if {[info exists port]} {
		append result :$port
	    }
	    append result "/[string trimleft $path /]"
	    if {[info exists query]} {
		append result "?$query"
	    }
	    if {[info exists fragment]} {
		append result "\#$fragment"
	    }
	}
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
