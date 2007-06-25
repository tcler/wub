# Convert -- make-like content transformation
#
# intercepts responses and transforms content according to mime-type

package provide Convert 1.0

package require snit
package require sgraph
package require Interp

if {[info exists argv0] && ($argv0 eq [info script])} {
    lappend auto_path [pwd]
}

snit::type Convert {
    option -subdomain {}
    option -conversions 1	;# include some default conversions

    option -namespace -configuremethod set_namespace
    method set_namespace {option value} {
	lappend options($option) $value
    }

    method Namespace {ns} {

	# scan the namespace looking for translators
	# which are commands of the form .mime/type.mime/type
	set candidates [namespace eval $ns info commands .*/*.*/*]
	Debug.convert {Convert Namespace $ns transformers - $candidates}
	foreach candidate $candidates {
	    lassign [split $candidate .] -> from to
	    $self Transform $from $to namespace eval $ns $candidate
	}

	# scan the namespace looking for postprocessors
	# which are commands of the form .mime/type
	set postproc {}
	set candidates [namespace eval $ns info commands .*/*]
	Debug.convert {Convert Namespace $ns postprocessors - $candidates}
	foreach candidate $candidates {
	    if {[lassign [split $candidate .] x from] eq ""} {
		$self Postprocess $from namespace eval $ns $candidate
	    }
	}
    }

    variable paths -array {}
    variable graph
    method path {from to} {
	Debug.convert {path $from -> $to ($graph)}
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

	return $pairs
    }

    # set of mappings from,to mime-types
    variable transform -array {}

    method Transform {from to args} {
	set prefix ${from},$to
	set transform($prefix) $args
	dict lappend graph $from $to
    }

    # set of mappings mime-type to postprocess script
    variable postprocess -array {}

    method Postprocess {from args} {
	set postprocess($from) $args
    }

    # perform applicable postprocess on content of given type
    method postprocess {rsp} {
	set ctype [dict get $rsp content-type]
	Debug.convert {postprocess: $ctype}

	if {[info exists postprocess($ctype)]} {
	    # there is a postprocessor
	    
	    #set rsp [Http loadContent $rsp] ;# read -fd content if any
	    
	    Debug.convert {postprocessing: $postprocess($ctype)}
	    set rsp [{*}$postprocess($ctype) [list $rsp]]
	    catch {dict unset rsp -file}	;# forget that there's a file connected
	    Debug.convert {postprocessed: $postprocess($ctype)}
	}
	return $rsp
    }

    # perform applicable transformations on content
    method transform {rsp} {
	Debug.convert {transform: [dumpMsg $rsp]}

	if {![dict exists $rsp accept]} {
	    dict set rsp accept "text/html"
	} else {
	    dict set rsp accept [string map [list */* text/html] [dict get $rsp accept]]
	}

	# create dummy quality measure to preserve order
	set order 10000
	set ctype [dict get $rsp content-type]
	foreach a [split [dict get $rsp accept] ,] {
	    lassign [split [string trim $a] ";"] a q

	    Debug.convert {transform matching '$a' '$q' against $ctype}
	    if {$a eq $ctype} {
		# exact match - done
		Debug.convert {transform: matched $a}
		return [dict replace $rsp -raw 1]
	    }

	    if {$q eq ""} {
		set q [incr order -1].0 ;# ordered quality metric
	    } else {
		set q [lindex [split $q =] 1]
	    }
	    lappend acceptable [list $a $q]
	}

	# sort acceptable types in decreasing order of acceptability
	set acceptable [lsort -real -decreasing -index 1 $acceptable]

	foreach a $acceptable {
	    set a [lindex $a 0]
	    Debug.convert {trying '$ctype,$a'}
	    set path [$self path $ctype $a]
	    if {$path ne {}} {
		# there is a transforming path
		Debug.convert {TRANSFORMING: $path}
		
		set rsp [Http loadContent $rsp] ;# read -fd content if any
		
		# perform transformations on path
		foreach el $path {
		    Debug.convert {transform element: $el with '$transform($el)'}
		    if {![set code [catch {
			{*}$transform($el) [list $rsp]
		    } result eo]]} {
			# transformation success
			Debug.convert {transform Success: $result}
			set rsp $result
			catch {dict unset rsp -file}	;# forget that there's a file
		    } else {
			# transformation failure
			Debug.convert {transform Error: $result ($eo)}
			return [Http ServerError $rsp $result $eo]
		    }
		}
		return $rsp	;# completed transformation path
	    }
	}

	dict set rsp -raw 1	;# no more transformations - go with what we have
	return $rsp
    }

    method Convert {rsp} {
	Debug.convert {Converting}

	# perform any postprocessing on input type
	if {[dict exists $rsp content-type]} {
	    set rsp [$self postprocess $rsp]
	    set ctype [dict get $rsp content-type]
	}

	while {![dict exists $rsp -raw]
	       && [dict exists $rsp content-type]} {
	    # transform according to mime type
	    set rsp [$self transform $rsp]
	    
	    # perform any postprocessing on *transformed* type
	    if {$ctype ne [dict get $rsp content-type]} {
		set rsp [$self postprocess $rsp]
	    }
	    
	    Debug.convert {Converted: [dumpMsg $rsp]}
	}
	return $rsp
    }

    method Convert! {rq mime to content} {
	dict set rq accept $to
	dict set rq content-type $mime
	dict set rq -content $content
	return [$self Convert $rq]
    }

    option -safe 0
    option -aliases {}

    method template {req} {
	set fpath [dict get $req -template_file]
	Debug.convert {template run: $fpath [dumpMsg $req]}

	set req [Http Depends $req $fpath] ;# cache functional dependency

	set interp [Interp name $req]
	if {$interp eq ""} {
	    # create an interpreter
	    dict set req -safe $options(-safe)
	    set interp [Interp create $req]
	}

	# fill interp with useful aliases and commands
	if {[catch {$interp eval set ::response}]} {
	    # template signals dynamic content with this
	    interp alias $interp Template_Dynamic \
		$interp dict set ::response -dynamic 1
	    interp alias $interp Template_Static \
		$interp dict set ::response -dynamic 0

	    foreach cmd $options(-aliases) {
		interp alias $interp $cmd {} ::$cmd
	    }

	    interp alias $interp Self {} $self
	    dict set req -interp $interp	;# remember interpreter
	}

	# read template into interpreter
	if {[catch {
	    set template [file read $fpath]
	    Debug.convert {template code: $template}
	} r eo]} {
	    do respond ServerError $req $r $eo
	}

	# set some variables
	catch {dict unset req -code}	;# let subst set -code value
	if {![dict exists $req content-type]} {
	    # set default mime type
	    dict set req content-type x-text/html-fragment
	}
	$interp eval set ::response [list $req]

	#trace add command $interp {rename delete} trace_pmod

	# save some sensitive values
	set root [dict get $req -root]

	# perform template substitution
	set code [catch {
	    #puts stderr "template: $template"
	    $interp eval subst [list $template]
	} result eo]	;# result is the substituted template
	Debug.convert {template result: $code ($eo) - '$result' over '$template'} 2

	# dig out the result from the error options dict
	if {[dict exists $eo -response]} {
	    Debug.convert {template has -response}
	    set rsp [dict get $eo -response]	;# get response dict
	} else {
	    # implicit return value - use the substitution
	    set rsp [$interp eval set ::response]	;# get response dict
	    dict set rsp -content $result	;# fold subst result back into response
	    Debug.convert {template implicit return - $code - '$result' ($eo) - [dumpMsg $rsp] - [$interp eval dict get \$::response -dynamic]}
	    if {![dict exists $rsp -code]} {
		#dict set rsp -code 0
	    }
	}
	Debug.convert {template return code: [Dict get? $rsp -code] dynamic: [dict get $rsp -dynamic] -content: '[dict get $rsp -content]'}

	# remember interpreter for next time this
	# transaction needs one
	dict set rsp -interp $interp

	# restore sensitive values
	dict set rsp -root $root

	if {$code && ($code < 200)} {
	    do respond ServerError $rsp $result $eo
	}

	# if the template didn't set its own content,
	# use the template subst result
	if {([Dict get? $rsp -content] eq "")} {
	    dict set rsp -content $result
	}

	# wrap returned content for %BODY% - experimental
	if {[dict exists $eo -wrapper]} {
	    dict set rsp \
		-content [string map \
			      [list %BODY% [dict get $rsp -content]] \
			      [dict get $rsp -wrapper]]
	    dict unset eo -wrapper
	}

	if {[dict exists $rsp -code]} {
	    # template has taken control and set code itself.
	    # we follow its instructions with the code given.
	    Debug.convert {template return ($fpath) [dict get $rsp -code]: $rsp - $result}
	    return -code [dict get $rsp -code] -response $rsp $result
	}

	# completed substutition successfully
	dict set rsp -content $result	;# result is the substituted template

	return $rsp
    }

    method do {rsp} {
	return [$self Respond [dict exists $rsp -code] $rsp]
    }

    method Respond {code rsp} {
	Debug.convert {Respond: [dumpMsg $rsp]
	    with: [array get transform]
	    postproc: [array get postprocess]
	}

	if {[dict exists $rsp -raw]} {
	    Debug.convert {Respond Raw override}
	    return $rsp
	}

	set code [catch {
	    # run specified template over response
	    if {[Dict get? $rsp -template_file] ne ""} {
		# apply template
		set rsp [$self template $rsp]
	    }

	    $self Convert $rsp	;# perform conversion
	} r eo]

	if {$code} {
	    if {[dict exists $eo -response]} {
		set rsp [dict get $eo -response]
		dict unset eo -response
	    } elseif {[dict exists $eo -request]} {
		set rsp [dict get $eo -request]
		dict unset eo -request
	    } else {
		# go with the rsp we've got
	    }
	    Debug.convert {Convert error: $code '$r' ($eo) - [dumpMsg $rsp]}
	} else {
	    # pass back the transformed response
	    set rsp $r
	    if {[dict exists $rsp -code]} {
		set code [dict get $rsp -code]
	    } else {
		set code 200	;# assume a standard Ok response
	    }
	}

	return $rsp
    }

    method Dispatch {req} {
	set ec [catch {
	    if {[dict exists $req -raw]} {
		# the request is raw, so just skip this domain
		Debug.convert {Convert raw}
		do jump $req $options(-subdomain) Dispatch
	    } else {
		Debug.convert {Convert normal -> $options(-subdomain)}
		do dispatch $req $options(-subdomain) Dispatch
	    }
	} r eo]
	Debug.convert {Convert return: ($eo)}

	return -options $eo $req
    }

    constructor {args} {
	$self configurelist $args

	if {$options(-conversions)} {
	    package require conversions
	    $self Namespace ::conversions
	}
	foreach ns $options(-namespace) {
	    $self Namespace ::$ns
	}
	Debug.convert {Convert graph: $graph}
    }
}

if {[info exists argv0] && ($argv0 eq [info script])} {
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
