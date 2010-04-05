# Convert -- make-like content transformation
#
# intercepts responses and transforms content according to mime-type
#
# Convert translates content by invoking translator procs
# (which are commands of the form .mime/type.mime/type)
#

package require sgraph
package require OO

package provide Convert 1.0

if {[info exists argv0] && ($argv0 eq [info script])} {
    lappend auto_path [pwd]
}

set API(Domains/Convert) {
    {
	Pseudo-Domain for performing content negotiation.

	Convert attempts to convert content from the response content-type to something permitted by the accept request field by intercepting responses from -subdomains.

	To do this, Convert maintains a table of mappings from one mime-type to another, finds a minimal path between the content-type to some acceptable type, and calls each conversion function in turn.
    }
}

class create Convert {
    # Transform - add a single transform to the transformation graph
    method transform {from to args} {
	set prefix ${from},$to
	variable transform; set transform($prefix) $args
	variable graph; dict lappend graph $from $to
    }

    # Postprocess - add a postprocessor to the set of postprocessors
    method postprocess {from args} {
	variable postprocess; set postprocess($from) $args
    }

    # object - add all transformers and postprocessors from the object
    method object {object} {
	# scan the object looking for translators
	# which are methods of the form .mime/type.mime/type

	# construct a dict from method name to the formal parameters of the method
	set defclass [info object class $object]
	set methods [lreverse [lsort -dictionary [info class methods $defclass -private -all]]]

	set convertors {}
	foreach m $methods {
	    if {[string match */*.*/* $m]} {
		lassign [split $m .] from to
		lappend convertors "$from->($m)->$to"
		my transform $from $to eval $object $m
	    }
	}

	# scan the object looking for postprocessors
	# which are methods of the form .mime/type
	foreach m $methods {
	    if {[string match /* $m]} continue
	    if {[string match */* $m]} {
		if {[string match */*.*/* $m]} continue
		lappend convertors "$m->($m)->$m"
		my postprocess $m eval $object $m
	    }
	}
	Debug.convert {Object Convertors from $defclass: $convertors over $object}
    }

    # namespace - add all transformers and postprocessors from the namespace
    method namespace {ns} {
	# scan the namespace looking for translators
	# which are commands of the form .mime/type.mime/type
	set candidates [namespace eval $ns info commands .*/*.*/*]
	Debug.convert {Namespace '$ns' transformers - $candidates}
	foreach candidate $candidates {
	    lassign [split $candidate .] -> from to
	    my transform $from $to namespace eval $ns $candidate
	}

	# scan the namespace looking for postprocessors
	# which are commands of the form .mime/type
	set postproc {}
	set candidates [namespace eval $ns info commands .*/*]
	Debug.convert {Namespace '$ns' postprocessors - $candidates}
	foreach candidate $candidates {
	    if {[string match .*/*.*/* $candidate]} continue
	    if {[lassign [split $candidate .] x from] eq ""} {
		my postprocess $from namespace eval $ns $candidate
	    }
	}
    }

    # perform applicable postprocess on content of given type
    method postprocessor {rsp} {
	set ctype [dict get $rsp content-type]
	variable postprocess
	if {[info exists postprocess($ctype)]} {
	    Debug.convert {postprocess type: $ctype ($rsp)}
	    # there is a postprocessor

	    #set rsp [Http loadContent $rsp] ;# read -fd content if any

	    Debug.convert {postprocessing: $postprocess($ctype)}
	    set rsp [{*}$postprocess($ctype) [list $rsp]]
	    catch {dict unset rsp -file}	;# forget that there's a file connected
	    Debug.convert {postprocessed: $postprocess($ctype)}
	} else {
	    Debug.convert {there is no $ctype postprocessor}
	}
	return $rsp
    }

    # path - return a path through the transformation graph
    # between source 'from' and sink 'to'
    method path {from to} {
	Debug.convert {PATH: trying to find a path from '$from' to '$to'}
	variable graph
	variable paths
	if {[info exists paths($from,$to)]} {
	    Debug.convert {CACHED path '$from' -> '$to' is $paths($from,$to)}
	    #Debug.convert {all known paths [array names paths]}
	    return $paths($from,$to)	;# cached match
	}

	Debug.convert {PATH? $from -> $to within ($graph)}

	# try a glob match
	lassign [split $from /] fmajor
	lassign [split $to /] tmajor

	# generate transformer name pairs
	foreach {accept possible} [subst {
	    1.0 [list $from,$to]
	    0.5 [list [array names paths -glob $fmajor/*,$to]]
	    0.5 [list [array names paths -glob $from,$tmajor/*]]
	    0.25 [list [array names paths -glob $fmajor/*,$tmajor/*]]
	}] {
	    if {![llength $possible]} continue
	    lassign [split [lindex $possible 0] ,] from to
	    Debug.convert {ESSAY $accept: '$from' '$to'}
	    set path [::sgraph::path $graph $from $to]
	    if {[llength $path]} {
		Debug.convert {SUCCESS path $from -> $to via $path}
		break
	    } else {
		Debug.convert {FAIL essayed path $from -> $to}
	    }
	}

	if {[llength $path]} {
	    # turn path into pairwise transformer list
	    set pairs {}
	    foreach el $path {
		if {[info exists prev]} {
		    lappend pairs "$prev,$el"
		}
		set prev $el
	    }

	    Debug.convert {path FOUND $from -> $to via '$pairs'}
	    set paths($from,$to) [list $pairs $accept]	;# cache result
	    return [list $pairs $accept]
	} else {
	    Debug.convert {NO PATH FOUND $from -> $to}
	    return {}
	}
    }

    # Find a match for an acceptable type,
    # or calculate a path through the transformation graph
    # which will generate an acceptable type

    method tpath {rsp} {
	set ctype [dict get $rsp content-type]	;# what we have
	set accept [dict get $rsp accept]	;# what we want

	# check transformation cache first
	variable tcache
	if {[info exists tcache(${ctype}@$accept)]} {
	    # found a cached transformation path - use it
	    return $tcache(${ctype}@$accept)
	}

	set path ""	;# how to get from ctype to one acceptable

	# search the acceptable for a match to what we have
	# otherwise, sort the acceptable by q parameter
	set order 10000; # dummy quality measure to preserve order
	foreach a [split [dict get $rsp accept] ,] {
	    # client will accept type $a with quality $q
	    lassign [split [string trim $a] ";"] a q

	    Debug.convert {tpath matching '$a' '$q' against '$ctype'}
	    if {[string match $a $ctype]} {
		# exact match - we have a direct conversion
		Debug.convert {tpath: DIRECT conversion to '$a'}
		set tcache(${ctype}@$accept) *
		return *
	    }

	    if {$q eq ""} {
		# give unqualified accepts a lower order
		set q [incr order -1].0 ;# ordered quality metric
	    } else {
		# determine the accept order
		set q [lindex [split $q =] 1]
	    }

	    # fix bogus quality values
	    if {![string is double -strict $q]} {
		if {[string is integer -strict $q]} {
		    set q "${q}.0"
		} else {
		    set q 1.0
		}
	    }

	    # while not an exact match, $a would be acceptable
	    lappend acceptable $a $q
	}

	Debug.convert {tpath: searching indirect among acceptable ($acceptable)}

	# there is no direct match for any accepted types
	# process acceptable types in decreasing order of acceptability
	# find all possible conversion paths (including wildcards)
	set found {}
	foreach {a q} $acceptable {
	    Debug.convert {trying '$ctype->$a' quality $q}
	    lassign [my path $ctype $a] path quality ;# find a conversion path
	    if {$path ne {}} {
		# we found a path to the most acceptable available type
		Debug.convert {possible path $path.  Quality $quality * $q}
		dict lappend found [expr {$quality * $q}] $path
	    } else {
		Debug.convert {impossible path $ctype->$a}
	    }
	}

	Debug.convert {possible successful paths: ($found)}

	# find the most acceptable path by quality
	if {[dict size $found]} {
	    set path [dict get $found [lindex [lsort -real -decreasing [dict keys $found]] 0]]
	    set path [lindex $path 0]
	    set tcache(${ctype}@$accept) $path
	}

	return $path
    }

    # Apply transformations to content
    # yielding a desired content-type
    method transformer {rsp} {
	set path [my tpath $rsp]	;# find a transformation path

	if {$path eq "*"} {
	    Debug.convert {transform identity}
	    return [list identity $rsp]
	} elseif {![llength $path]} {
	    # no transformations possible
	    Debug.convert {no transformations possible}
	    return [list none $rsp]
	}

	# a transforming path exists
	Debug.convert {TRANSFORMING: [dict get $rsp -url] along path $path}

	# perform those transformations on the path
	variable transform
	set ctype [dict get $rsp content-type]
	set oldel $ctype
	foreach el $path {
	    Debug.convert {transform element: $el with '$transform($el)'}
	    if {![set code [catch {
		{*}$transform($el) [list $rsp]	;# transform content
	    } result eo]]} {
		# transformation success - proceed
		set rsp $result
		if {[dict exists $rsp -suspend]} {
		    # the transformer wants to suspend
		    return [list suspended $rsp]
		}

		# ensure the transformation is still on-path
		set ctype [dict get $rsp content-type]
		lassign [split $el .] -> oldtype expected
		if {![string match $expected $ctype]} {
		    # a transformer hasn't set the new mime type.
		    if {[string match $oldtype $ctype]} {
			dict set rsp content-type $expected
			Debug.log {Transformer '$transform($el)' failed to set content type to $expected - FIX please}
		    } else {
			# restart the transformation with new content type
			Debug.convert {Transformer '$transform($el)' $oldtype -> $ctype - expecting '$expected' CHANGED}
			return [list changed $rsp]
		    }
		}

		Debug.convert {transform Success: $transform($el)}
	    } else {
		# transformation failure - alert the caller
		Debug.convert {transform Error: $result ($eo) ([dumpMsg $rsp])}
		return [list error [Http ServerError $rsp $result $eo]]
	    }

	    set oldtype $el
	}

	return [list complete $rsp]
    }

    # convert - perform all content negotiation on a Wub response
    method convert {rsp {to ""}} {
	if {[dict exists $rsp -file]} {
	    Debug.convert {request is a -file [dict get? $rsp content-type], return}
	    return $rsp
	} elseif {![dict exists $rsp -content]} {
	    Debug.convert {request has no content, return}
	    return $rsp
	}
	if {![dict exists $rsp content-type]} {
	    error "response has no content-type [dumpMsg $rsp]"
	}

	# raw responses get no conversion
	if {[dict get? $rsp -raw] eq "1"} {
	    return $rsp	;# this is raw - no conversion
	}

	# avoid identity conversions
	if {$to ne ""} {
	    if {[dict get? $rsp content-type] eq $to} {
		Debug.convert {Identity Conversion}
		return $rsp	;# don't need to process
	    }
	    dict set rsp accept $to
	}

	# condition 'accept' tag
	if {![dict exists $rsp accept]} {
	    # by default, accept text/html
	    dict set rsp accept "text/html"
	} else {
	    # don't allow */* as an 'accept' - they never mean it
	    dict set rsp accept [string map [list */* text/html,text/plain] [dict get $rsp accept]]
	}

	# condition 'content-type'
	dict set rsp content-type [string tolower [dict get $rsp content-type]]

	# perform any preprocessing on input type
	variable postprocess
	set preprocessed ""
	set ctype [dict get $rsp content-type]
	if {[dict exists $rsp content-type] && [info exists postprocess($ctype)]} {
	    Debug.convert {Preprocess of [dict get $rsp content-type]}
	    set rsp [my postprocessor $rsp]
	    # remember we've preprocessed
	    set preprocessed [dict get $rsp content-type]
	}

	# perform each transformation on the path
	# any step may set -raw to avoid further conversion
	while {[dict get? $rsp -raw] ne "1"} {
	    # transform according to mime type
	    # determine the current best path from the current content-type
	    # and one of the acceptable types.
	    set oldct [dict get $rsp content-type]
	    Debug.convert {transforming from '$oldct' to one of these '[dict get $rsp accept]'}
	    lassign [my transformer $rsp] state rsp
	    Debug.convert {transformed from $oldct to '[dict get $rsp content-type]'}

	    Debug.convert {STATE: $state RAW: '[dict get? $rsp -raw]'}
	    switch -- $state {
		complete -
		none -
		identity {
		    # the transformation completed normally
		    # the content is now in an acceptable form
		    # or: no transformation is possible

		    # perform any postprocessing on *transformed* type
		    # avoid doing this twice - if we've already preprocessed
		    set ctype [dict get $rsp content-type]
		    if {$preprocessed ne $ctype
			&& [info exists postprocess($ctype)]
		    } {
			set rsp [my postprocessor $rsp]
		    }
		    break
		}

		error {
		    # a transformation error has occurred
		    # we presume a ServerError has been generated
		    # we will proceed with it as content
		}

		changed {
		    # the transformation path took us in an unexpected direction
		}
		suspended {
		    # a transformer has suspended
		    # the content will be returned later
		    break
		}
	    }
	}

	Debug.convert {conversion complete: [dumpMsg $rsp]}
	return $rsp
    }

    # convert! - perform a specified transformation on a Wub response
    method convert! {rq to {mime ""} {content ""}} {
	if {$mime ne ""} {
	    dict set rq content-type $mime
	}
	if {$content ne ""} {
	    dict set rq -content $content
	}
	if {[dict exists $rq accept]} {
	    set oldaccept [dict get $rq accept]
	}
	set rq [my convert $rq $to]
	if {[info exists oldaccept]} {
	    dict set rq accept $oldaccept
	}
	return $rq
    }

    # do - perform content negotiation and transformation
    constructor {args} {
	variable conversions 1	;# include some default conversions
	variable namespace

	variable paths; array set paths {}
	variable graph	;# transformation graph - all known transforms
	variable transform	;# set of mappings from,to mime-types
	array set transform {}

	# set of mappings mime-type to postprocess script
	variable postprocess
	array set postprocess {}

	variable tcache	;# cache of known transformations

	# allow .ini file to modify defaults
	variable {*}[Site var? Convert]
	variable {*}$args

	variable conversions
	if {$conversions} {
	    package require conversions
	    my namespace ::conversions
	}

	foreach {n ns} $args {
	    if {$n eq "namespace"} {
		my namespace ::$ns
	    }
	}

	my object [self]	;# add in any conversions by child instances
    }
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    error "this is out of date"
    lappend auto_path [pwd] [pwd]/stx/

    package require Stdin
    package require Listener
    package require Httpd
    package require Host
    package require File
    package require Dump
    package require Compose

    namespace eval ::xconversions {
	proc .x-text/html-fragment.text/html {rsp} {
	    set content "<html> \n"
	    append content "<header><title>Wrapped</title></header>" \n
	    append content <body> \n
	    append content [dict get $rsp -content]
	    append content </body> \n
	    append content </html> \n
	    return [dict replace $rsp \
			-content $content \
			-raw 1 \
			content-type text/html]
	}
    }

    Host localhost -name ""
    localhost Register /dump/ DumpDispatch	;# dump dispatcher

    set fdom [File %AUTO% root [file dirname [info script]]]
    localhost Register /file/ $fdom Dispatch	;# filedomain dispatcher

    set collect [Compose %AUTO% -subdomains [list $sdom $fdom]]
    set convert [Convert %AUTO% -subdomain $collect]

    #$convert namespace ::conversions

    localhost Register / $convert Dispatch

    # start Listener
    set listener [Listener %AUTO% -port 8080]
    puts stderr "Listener $listener"

    set forever 0
    vwait forever
}
