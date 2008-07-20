# Convert -- make-like content transformation
#
# intercepts responses and transforms content according to mime-type
#
# Convert translates content by invoking translator procs
# (which are commands of the form .mime/type.mime/type)
#

package require sgraph

package provide Convert 1.0

if {[info exists argv0] && ($argv0 eq [info script])} {
    lappend auto_path [pwd]
}

namespace eval Convert {
    variable conversions 1	;# include some default conversions
    variable namespace

    variable paths
    array set paths {}
    variable graph	;# transformation graph - all known transforms
    variable transform	;# set of mappings from,to mime-types
    array set transform {}

    # Transform - add a single transform to the transformation graph
    proc Transform {from to args} {
	set prefix ${from},$to
	variable transform
	set transform($prefix) $args
	variable graph
	dict lappend graph $from $to
    }

    # set of mappings mime-type to postprocess script
    variable postprocess
    array set postprocess {}

    # Postprocess - add a postprocessor to the set of postprocessors
    proc Postprocess {from args} {
	variable postprocess
	set postprocess($from) $args
    }

    # Namespace - add all transformers and postprocessors from the namespace
    proc Namespace {ns} {

	# scan the namespace looking for translators
	# which are commands of the form .mime/type.mime/type
	set candidates [namespace eval $ns info commands .*/*.*/*]
	Debug.convert {Convert Namespace $ns transformers - $candidates}
	foreach candidate $candidates {
	    lassign [split $candidate .] -> from to
	    Transform $from $to namespace eval $ns $candidate
	}

	# scan the namespace looking for postprocessors
	# which are commands of the form .mime/type
	set postproc {}
	set candidates [namespace eval $ns info commands .*/*]
	Debug.convert {Convert Namespace $ns postprocessors - $candidates}
	foreach candidate $candidates {
	    if {[lassign [split $candidate .] x from] eq ""} {
		Postprocess $from namespace eval $ns $candidate
	    }
	}
    }

    # path - return a path through the transformation graph
    # between source 'from' and sink 'to'
    proc path {from to} {
	variable graph
	Debug.convert {path $from -> $to ($graph)}
	variable paths
	if {[info exists paths($from,$to)]} {
	    return $paths($from,$to)
	}

	# generate transformer name pairs
	set pairs {}
	set path [::sgraph::path $graph $from $to]
	Debug.convert {path $from -> $to == $path}

	foreach el $path {
	    if {[info exists prev]} {
		lappend pairs "$prev,$el"
	    }
	    set prev $el
	}

	set paths($from,$to) $pairs	;# cache result

	Debug.convert {path found $from -> $to == '$pairs'}
	return $pairs
    }

    # perform applicable postprocess on content of given type
    proc postprocess {rsp} {
	set ctype [dict get $rsp content-type]
	Debug.convert {postprocess type: $ctype ($rsp)}
	variable postprocess
	if {[info exists postprocess($ctype)]} {
	    # there is a postprocessor
	    
	    #set rsp [Http loadContent $rsp] ;# read -fd content if any
	    
	    Debug.convert {postprocessing: $postprocess($ctype)}
	    set rsp [{*}$postprocess($ctype) [list $rsp]]
	    catch {dict unset rsp -file}	;# forget that there's a file connected
	    Debug.convert {postprocessed: $postprocess($ctype)}
	}
	Debug.convert {postprocessed type: $ctype ($rsp)}
	return $rsp
    }

    variable tcache	;# cache of known transformations

    proc tpath {rsp} {
	if {![dict exists $rsp accept]} {
	    dict set rsp accept "text/html"
	} else {
	    dict set rsp accept [string map [list */* text/html] [dict get $rsp accept]]
	}

	set ctype [dict get $rsp content-type]
	set accept [dict get $rsp accept]
	set path ""

	variable tcache
	if {[info exists tcache(${ctype}=$accept)]} {
	    # found a cached transformation path - use it
	    set path $tcache(${ctype}=$accept)
	} else {
	    # search for path through conversion graph
	    set order 10000; # create dummy quality measure to preserve order
	    foreach a [split [dict get $rsp accept] ,] {
		lassign [split [string trim $a] ";"] a q
		
		Debug.convert {tpath matching '$a' '$q' against '$ctype'}
		if {$a eq $ctype} {
		    # exact match - done
		    Debug.convert {tpath: matched $a}
		    return [list * [dict replace $rsp -raw 1]]
		}

		if {$q eq ""} {
		    set q [incr order -1].0 ;# ordered quality metric
		} else {
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
		lappend acceptable [list $a $q]
	    }

	    # process acceptable types in decreasing order of acceptability
	    foreach a [lsort -real -decreasing -index 1 $acceptable] {
		set a [lindex $a 0]
		Debug.convert {trying '$ctype,$a'}
		set path [path $ctype $a]
		if {$path ne {}} {
		    set tcache(${ctype}=$accept) $path
		    break
		}
	    }
	}

	return [list $path $rsp]
    }

    # perform applicable transformations on content
    proc transform {rsp} {
	Debug.convert {transform: [dumpMsg $rsp]}
	lassign [tpath $rsp] path rsp
	if {$path eq "*"} {
	    Debug.convert {transform identity}
	    return $rsp
	}

	if {$path ne ""} {
	    # there is a transforming path
	    Debug.convert {TRANSFORMING: [dict get $rsp -url] $path}
	    set rsp [Http loadContent $rsp] ;# read -fd content if any
	    
	    # perform transformations on path
	    variable transform
	    foreach el $path {
		Debug.convert {transform element: $el with '$transform($el)'}
		if {![set code [catch {
		    {*}$transform($el) [list $rsp]
		} result eo]]} {
		    # transformation success
		    Debug.convert {transform Success: $result}
		    set rsp $result
		    catch {dict unset rsp -file} ;# forget that there's a file
		} else {
		    # transformation failure
		    Debug.convert {transform Error: $result ($eo)}
		    return [Http ServerError $rsp $result $eo]
		}
	    }
	}

	dict set rsp -raw 1	;# no more transformations - go with what we have
	return $rsp
    }

    # Convert - perform all content negotiation on a Wub response
    proc Convert {rsp {to ""}} {
	Debug.convert {Converting '[dict get $rsp content-type]' to '$to'}
	if {$to ne ""} {
	    if {[Dict get? $rsp content-type] eq $to} {
		Debug.convert {Identity Conversion}
		return $rsp	;# don't need to process
	    }
	    dict set rsp accept $to
	}

	# perform any postprocessing on input type
	if {[dict exists $rsp content-type]} {
	    set rsp [postprocess $rsp]
	    set ctype [dict get $rsp content-type]
	}

	# perform each transformation on the path
	# any step may set -raw to avoid further conversion
	while {![dict exists $rsp -raw]
	       && [dict exists $rsp content-type]} {
	    # transform according to mime type
	    Debug.convert {convert transform: [dumpMsg $rsp]}
	    set rsp [transform $rsp]
	    
	    # perform any postprocessing on *transformed* type
	    if {$ctype ne [dict get $rsp content-type]} {
		Debug.convert {post-postprocess: [dumpMsg $rsp]}
		set rsp [postprocess $rsp]
	    }
	    
	    Debug.convert {Converted ($ctype [dict get $rsp content-type]): [dumpMsg $rsp]}
	}

	Debug.convert {Conversion complete: [dumpMsg $rsp]}
	return $rsp
    }

    # Convert! - perform a specified transformation on a Wub response
    proc Convert! {rq to {mime ""} {content ""}} {
	if {$mime ne ""} {
	    dict set rq content-type $mime
	}
	if {$content ne ""} {
	    dict set rq -content $content
	}
	if {[dict exists $rq accept]} {
	    set oldaccept [dict get $rq accept]
	}
	set rq [Convert $rq $to]
	if {[info exists oldaccept]} {
	    dict set rq accept $oldaccept
	}
	return $rq
    }

    # do - perform content negotiation and transformation
    proc do {rsp} {
	variable transform
	variable postprocess
	Debug.convert {DO Respond: [dumpMsg $rsp] with: [array get transform] postproc: [array get postprocess]
	}

	# -raw overrides content negotiation
	if {[dict exists $rsp -raw]} {
	    Debug.convert {Respond Raw override}
	    return $rsp
	}

	# perform the content negotiation
	set code [catch {
	    Convert $rsp	;# perform conversion
	} r eo]

	if {$code} {
	    Debug.convert {Convert error: $code '$r' ($eo) - [dumpMsg $rsp]}
	    set rsp [Convert [Http ServerError $rsp $r $eo] text/html]
	} else {
	    # pass back the transformed response
	    set rsp $r

	    if {![dict exists $rsp -code]} {
		dict set rsp -code 200
	    }
	}

	Debug.convert {Convert complete: $code '$rsp' ($eo) - [dumpMsg $rsp]}
	return $rsp
    }

    proc init {args} {
	if {$args ne {}} {
	    variable {*}$args
	}

	variable conversions
	if {$conversions} {
	    package require conversions
	    Namespace ::conversions
	}

	foreach {n ns} $args {
	    if {$n eq "namespace"} {
		Namespace ::$ns
	    }
	}
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    # this is out of date
    lappend auto_path [pwd] [pwd]/stx/

    package require Stdin
    package require Listener
    package require Httpd
    package require Host
    package require File
    package require Snit
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

    set fdom [File %AUTO% -root [file dirname [info script]]]
    localhost Register /file/ $fdom Dispatch	;# filedomain dispatcher

    set sdom [TestSnit %AUTO%]
    localhost Register /snit/ $sdom Dispatch	;# filedomain dispatcher

    set collect [Compose %AUTO% -subdomains [list $sdom $fdom]]
    set convert [Convert %AUTO% -subdomain $collect]

    #$convert Namespace ::conversions

    localhost Register / $convert Dispatch

    # start Listener
    set listener [Listener %AUTO% -port 8080]
    puts stderr "Listener $listener"

    set forever 0
    vwait forever
}
