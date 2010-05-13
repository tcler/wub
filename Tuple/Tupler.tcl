# Tupler - Tuple Rendering
#
# This is a child of Tuple which provides a Direct interface
# for presentation, creation and interaction with Tuples
# and a Convert interface to permit type-based rendering.
#
# Example: Nub domain /tuple/ {Tupler ::tupler} prime [::fileutil::cat [file join [file dirname [info script]] prime.tcl]]

if {[info exists argv0] && ($argv0 eq [info script])} {
    lappend auto_path ~/Desktop/Work/Wub/ [file dirname [info script]]
}

package require Tuple

if {[catch {package require Debug}]} {
    #proc Debug.tupler {args} {}
    proc Debug.tupler {args} {puts stderr HTTP@[uplevel subst $args]}
} else {
    Debug on tupler 10
}

package require OO
package require Site
package require Direct
package require Convert
package require Http
package require Html
package require Url
package require Dict
package require fileutil

package provide Tupler 1.0

oo::class create Tupler {
    foreach {f t} {
	text text/plain
	javascript application/javascript
	css text/css
    } {
	{*}[string map [list %F% $f %T% $t] {
	    method tuple/%F%.%T% {r} {
		dict set r content-type %T%
		return $r
	    }
	}]
    }
    set ::Tuple_home [file dirname [info script]]

    method component {r name el {type html}} {
	Debug.tupler {component $name+$el $type}
	if {[catch {my fetch $name+$el} c]} {
	    Debug.tupler {component $name $el => NONE}
	    return {}
	}

	Debug.tupler {component convert tuple/[dict get $c type] to tuple/$type}
	set cpp [my convert! [list content-type tuple/[dict get $c type] -content [dict get $c content]] tuple/$type]

	# conversion of components may generate more header components
	# these must be added to the response
	foreach sub {script load style} {
	    upvar $sub $sub
	    if {[dict exists $cpp -$sub]} {
		set $sub [dict merge [set $sub] [dict get $cpp -$sub]]
		Debug.tupler {component subcomponent $name -$sub}
	    }
	}

	Debug.tupler {component $name $el => [dict get $c id]}
	return [list $r [dict get $c id] [dict get $cpp -content] $script $load $style]
    }

    method tuple/html.text/html {r} {
	# convert the Html type to pure HTML
	dict set r -raw 1	;# no more conversions after this
	
	if {[string match "<!DOCTYPE*" [dict get $r -content]]} {
	    return [Http Ok $r $html text/html]	;# content is already fully HTML
	}

	# record supplied header components
	foreach c {script load style} {
	    set $c [dict get? $r -$c]
	}
	
	# pre or post process HTML fragments by assembling their subcomponents
	set tuple [dict get $r -tuple]
	dict with tuple {
	    # fetch text/plain data (title)
	    foreach el {title} {
		set cm [my component $r $name $el text]
		if {[llength $cm]} {
		    lassign $cm r cid cc
		    dict lappend r -$el $cc
		}
	    }

	    foreach el {header nav} {
		set cm [my component $r $name $el]
		if {[llength $cm]} {
		    lassign $cm r cid cc
		    append body [<$el> id $cid $cc] \n
		}
	    }
	    
	    append body [<article> id [armour $id] [subst {
		<!-- name:'[armour $name]' left:[armour $_left] right:[armour $_right] -->
		[dict get $r -content]
		<!-- transforms [armour [dict get? $r -transforms]] -->
	    }]]

	    foreach el {aside footer} {
		set cm [my component $r $name $el]
		if {[llength $cm]} {
		    lassign $cm r cid cc
		    append body [<$el> id $cid $cc] \n
		}
	    }

	    # check for body and header components of document and assemble them
	    foreach {el et} {
		script javascript
		load {ref}
		style {css ref}
	    } {
		# fetch, convert and index header components
		if {![catch {my fetch $name+$el} c]} {
		    Debug.tupler {component $name $el => NONE}
		    set ct [string tolower [dict get $c type]]
		    set cc [dict get $c content]
		    set cid [dict get $c id]	;# default - index by component id
		    set cto html
		    if {$ct ni $et} {
			set cto [lindex $et 0]	;# convert to first expected type
		    } elseif {$ct eq "ref"} {
			set cid [lindex $cc 0]	;# index refs by URL component of ref
		    }
		    if {![dict exists [set $el] $cid]} {
			# convert metadata component to expected type
			set cpp [my convert! [list content-type tuple/$ct -tuple $c -content $cc] tuple/$cto]
			# index component by appropriate id
			dict set $el $cid [dict get $cpp -content]
		    } else {
			# don't bother converting if we already have the component
		    }
		}
	    }

	    # construct <head> part
	    if {[dict exists $r -title]} {
		set head [<title> [armour [join [dict get $r -title]]]]
	    } else {
		set head [<title> [armour [dict get $r -tuple name]]]
	    }
	    append head \n [join [dict values $load] \n]	;# add script preloads
	    append head \n [join [dict values $style] \n]	;# add style preloads
	    
	    # add scripts to <body> part
	    append body [join [dict values $script] \n] ;# add script postscripts

	    # construct the final HTML text
	    variable doctype	;# html doctype from Tupler instance
	    append html $doctype \n
	    append html <html> \n
	    append html <head> \n $head \n </head> \n
	    append html <body> \n $body \n </body> \n
	    append html </html> \n
	}	    
	return [Http Ok $r $html text/html]
    }

    # mktype - creates a bare-bones Type tuple 
    method mktype {name {type type}} {
	if {[catch {
	    my fetch $name
	} tuple]} {
	    set id [my new name $name type $type]
	    Debug.tupler {mktype $id -> [my id2tuple $id]}
	    return [my id2tuple $id]
	} elseif {[string tolower [dict get? $tuple type]] ne $type} {
	    return -code error -kind type -notfound $name "'$name' must be of type 'Type'"
	} else {
	    return $tuple
	}
    }

    # fixup - consistency and semantic supplements to tuples on their way to the store.
    method fixup {tuple} {
	set type Basic
	set t1 {}

	dict with tuple {
	    set type [string tolower $type]
	    # check for conversions and Types
	    switch -- $type {
		type {
		    # the content of a Type is its Convert postprocessor
		    if {[info exists content]
			&& $content ne ""
		    } {
			dict set tuple mime "Tcl Script"
		
			# (re)define the postprocess method
			set mname tuple/[string map {_ / " " _} [string tolower $name]]
			oo::objdefine [self] [string map [list %N% $mname %C% $content] {
			    method %N% {r} {%C%}
			}]
			my postprocess $mname [self] $mname
			Debug.tupler {added postprocess for type $name called $mname}
		    }
		}

		conversion {
		    # Conversion tuples create matching methods in [self] to handle
		    # type conversion and content negotiation.
		    if {[llength [lassign [split $name +] l r]]} {
			return -code error -kind form -notfound $name "$name must be a pair"
		    }
		    set lt [my mktype $l type]
		    set rt [my mktype $r type]

		    # conversion tuple
		    Debug.tupler {fixup conversion $l -> $r on $name}

		    # (re)define the conversion method
		    set lname tuple/[string map {_ / " " _} [string tolower [dict get $lt name]]]
		    set rname tuple/[string map {_ / " " _} [string tolower [dict get $rt name]]]
		    set mname $lname.$rname
		    
		    oo::objdefine [self] [string map [list %N% $mname %C% [dict get $tuple content]] {
			method %N% {r} {%C%}
		    }]
		    my transform $lname $rname [self] $mname
		    Debug.tupler {added conversion from $lname to $rname called '$mname'}
		}
	    }
	}

	return [next $tuple]
    }

    method getmime {tuple {default html}} {
	set mime [string map {" " _} [string tolower [dict get? $tuple mime]]]
	if {$mime eq ""} {
	    return tuple/$default
	} else {
	    return tuple/$mime
	}
    }

    method bad {r eo} {
	# failed to resolve name
	set extra [Url decode [dict get $r -extra]]
	set nfname [dict get? $eo -notfound]
	set kind [dict get? $eo -kind]
	
	# just an error - rethrow it
	if {$kind eq ""} {
	    return -options $tuple
	} else {
	    if {[catch {
		my fetch "Not Found"
	    } found]} {
		# no user-defined page - go with the system default
		variable notfound
		return [Http NotFound $r [subst $notfound] x-text/html-fragment]
	    } else {
		# found the user-defined page "Not Found"
		dict with found {
		    set mime [my getmime $found]
		    Debug.tupler {NotFound Handler: $found $mime}
		    if {$type eq ""} {
			set type basic
		    } else {
			set type [string map {" " _} [string tolower $type]]
		    }
		    set content [subst $content]
		    dict set r -tuple $found
		    return [Http Ok $r $content tuple/html]
		}
	    }
	}
    }

    # xray a tuple - presenting it as a dict
    method /xray {r args} {
	set extra [Url decode [dict get $r -extra]]
	if {[catch {my fetch $extra} tuple eo]} {
	    tailcall my bad $r $eo
	} else {
	    # resolved name
	    dict with tuple {
		dict set r -title "XRay of '$name'"
		dict set r -tuple $tuple
		dict set r -convert [self]
		return [Http Ok $r $tuple tuple/tcl_dict]
	    }
	}
    }

    # view a tuple - giving it its most natural HTML presentation
    method /view {r args} {
	set extra [Url decode [dict get $r -extra]]
	dict set r -convert [self]
	if {[catch {my fetch $extra} tuple eo]} {
	    tailcall my bad $r $eo
	} else {
	    # resolved name
	    dict with tuple {
		dict set r -tuple $tuple
		if {$type eq ""} {
		    set type basic
		} else {
		    set type [string map {" " _} [string tolower $type]]
		}
		return [Http Ok $r $content tuple/$type]
	    }
	}
    }

    # default presentation
    method / {r args} {
	set extra [dict get $r -extra]
	if {$extra eq ""} {
	    variable welcome
	    dict set r -extra $welcome
	}
	tailcall my /view $r {*}$args
    }

    superclass Tuple
    mixin Direct Convert

    constructor {args} {
	Debug.tupler {Constructing $args}
	variable welcome welcome
	variable primer prime.tcl		;# primer for Tupler
	variable doctype "<!DOCTYPE html>"	;# HTML5 doctype
	variable notfound {
	    [<h1> [string totitle "$kind error"]]
	    [<p> "'$name' not found while looking for '$extra'"]
	}
	variable {*}$args

	if {![info exists prime]} {
	    # always prime the Tuple with something
	    variable prime [::fileutil::cat [file join $::Tuple_home $primer]]
	    dict set args prime $prime
	}

	next? {*}$args conversions 0

	# add special transformations between native Types and mime types
	foreach {f t} {
	    text text/plain
	    html text/html
	    javascript application/javascript
	    css text/css
	} {
	    my transform tuple/$f $t [self] tuple/$f.$t
	}
    }
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    package require fileutil
    Debug on convert 10
    Debug on tupler 10

    set ts [Tupler new]

    if {0} {
	catch {$ts moop} e eo
	puts [string repeat = 80]
	puts "intentional error: '$e'"
    }

    if {0} {
	puts [string repeat = 80]
	puts "graph: [$ts graph]"
    }

    if {0} {
	puts [string repeat = 80]
	puts "test 'full' method:"
	foreach {n v} [$ts full] {
	    if {[lindex [$ts find #$n] 0] != $n} {
		error "Couldn't find $n"
	    }
	    puts "$n: $v"
	}
    }

    if {0} {
	puts [string repeat = 80]
	puts "test 'view' method"
	set fetched [$ts /view {-extra now}]
	puts "view fetched: $fetched"
	puts [$ts convert! $fetched text/html]
    }

    if {0} {
	puts [string repeat = 80]
	puts "test xray method"
	set fetched [$ts /xray {-extra now}]
	puts "xray fetched: $fetched"
	puts [$ts convert! $fetched text/html]
    }

    if {0} {
	puts [string repeat = 80]
	puts "test Variable type"
	set fetched [$ts /view {-extra reflect}]
	puts "Variable fetched: $fetched"
	puts [$ts convert! $fetched text/html]
    }

    if {1} {
	puts [string repeat = 80]
	puts "test Text type"
	set fetched [$ts /view {-extra "Example Text"}]
	puts "Text fetched: $fetched"
	puts [$ts convert! $fetched text/plain]
    }

    if {1} {
	puts [string repeat = 80]
	puts "test Reflect Text"
	set fetched [$ts /view {-extra "Reflect Text"}]
	puts "Reflect Text fetched: $fetched"
	puts [$ts convert! $fetched text/plain]
    }

    if {1} {
	puts [string repeat = 80]
	puts "test Example Uppercase"
	set fetched [$ts /view {-extra "Example Uppercase"}]
	puts "Example Uppercase fetched: $fetched"
	puts [$ts convert! $fetched text/plain]
    }
}
