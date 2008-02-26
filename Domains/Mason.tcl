# Mason -
#
# A domain to emulate Mason (http://www.masonhq.com/)

package require snit
package require Html

package provide Mason 1.0

::snit::type Mason {
    option -url ""	;# url for top of this domain
    option -root ""	;# file system domain root
    option -hide {^([.].*)|(.*~)$}	;# these files are never matched
    option -functional ".tml"	;# functional extension
    option -notfound ".notfound"	;# notfound handler name
    option -wrapper ".wrapper"	;# wrapper handler name
    option -auth ".auth"	;# authentication functional
    option -index index.html	;# directory index name
    option -dirhead {name size mtime *}
    option -dirfoot {}
    # additional aliases to be installed in session interpreter
    option -aliases -configuremethod addalias \
	-default {}
    method addalias {option value} {
	lappend options(-aliases) {*}$value
    }

    # when a file is not located, it will be searched for.
    # to minimise the cost of this search, -cache will
    # instruct Mason to memoize found files
    option -cache 1		;# cache file searches

    method conditional {req path} {
	# check conditional
	if {[dict exists $req if-modified-since]
	    && (![dict exists $req -dynamic] || ![dict get $req -dynamic])
	} {
	    set since [dict get $req if-modified-since]
	    if {$since eq [Http Date [file mtime $path]]} {
		# if the times are identical, it's unmodified
		Debug.mason {NotModified: $path}
		return 1
	    }
	}
	return 0
    }

    method findUp {req name} {
	Debug.mason {findUp [dict get $req -root] [dict get $req -suffix] $name} 3
	set suffix [string trim [dict get $req -suffix] /]
	if {$options(-cache)} {
	    set result [file upm [dict get $req -root] $suffix $name]
	} else {
	    set result [file up [dict get $req -root] $suffix $name]
	}
	return $result
    }

    method template {req fpath} {
	Debug.mason {template run: $fpath [dumpMsg $req]}

	dict lappend req -depends $fpath ;# cache functional dependency

	# read template into interpreter
	if {[catch {
	    set fd [open $fpath]
	    set template [read $fd]
	    close $fd
	    Debug.mason {template code: $template}
	} r eo]} {
	    catch {close $fd}
	    return [Http ServerError $req $r $eo]
	}

	# set some variables
	set response $req
	catch {dict unset response -code}	;# let subst set -code value
	#catch {dict unset response -content}	;# let subst set content
	if {![dict exists $response content-type]} {
	    # set default mime type
	    dict set response content-type x-text/html-fragment
	}

	# perform template substitution
	set code [catch {
	    #puts stderr "Mason template: $template"
	    subst $template
	} result eo]	;# result is the substituted template
	Debug.mason {template result: $code ($eo) - '$result' over '$template'} 2

	if {$code == 0} {
	    if {![dict exists $response -code]} {
		set code 200
	    }
	} elseif {$code < 200} {
	    dict set response -dynamic 1
	    return [Http ServerError $response $result $eo]
	} elseif {![dict exists $response -code]} {
	    dict set response -code $code
	}

	# implicit return value - use the substitution
	if {![dict exists $response -content]} {
	    dict set response -content $result	;# fold subst result back into response
	}

	Debug.mason {Mason Template return code: $code dynamic: [Dict get? $response -dynamic] content: '$result'}

	return $response
    }

    method functional {req fpath} {
	set rsp [$self template $req $fpath]
	Debug.mason {Mason Functional ($fpath): [dumpMsg $rsp]}

	# determine whether content is dynamic or not
	if {[dict exists $rsp -dynamic]
	    && [dict get $rsp -dynamic]
	} {
	    # it's completely dynamic - no caching
	    return [Http NoCache $rsp]
	} else {
	    # this is able to be cached.
	    #catch {dict unset rsp -dynamic}
	    return [Http CacheableContent $rsp [clock seconds]]
	}
    }

    # candidate - find a candidate for file
    method candidate {file} {
	if {[file exists $file]} {
	    return $file
	}
	
	# no such file - may be a functional?
	set fpath [file rootname $file]$options(-functional)
	if {[file exists $fpath]} {
	    return $fpath
	} else {
	    return ""
	}
    }

    method mason {req} {
	Debug.mason {Mason: [dumpMsg $req]}
	
	dict set req -mason $self
	dict set req -urlroot $options(-url)

	set http [Dict get? $req -http]
	set suffix [string trimleft [dict get $req -suffix] /]
	
	set ext [file extension $suffix]	;# file extent
	set path [file join [dict get $req -root] $suffix] ;# complete path to file
	set tail [file tail $suffix]	;# last component of path
	set url [dict get $req -url]	;# full URL
	
	Debug.mason {Mason: -url:$url - suffix:$suffix - path:$path - tail:$tail - ext:$ext}
	
	if {(($tail eq $ext) && ($ext ne "")
	     && ![dict exists $req -extonly])
	    || [regexp $options(-hide) $tail]
	} {
	    # this is a file name like '.../.tml', or is hidden
	    return [Http NotFound $req [subst {
		[<p> "'$path' has illegal name.</p>"]
	    }]]
	}
	
	# .notfound processing
	set fpath [$self candidate $path]
	if {$fpath eq ""} {
	    Debug.mason {not found $fpath - looking for $options(-notfound)}
	    set fpath [$self findUp $req $options(-notfound)]	;# get the .notfound
	    if {$fpath eq ""} {
		Debug.mason {notfound failed - really not here}
		# respond notfound template
		return [Http NotFound $req]
	    } else {
		# handle conditional request on .notfound
		if {[$self conditional $req $fpath]} {
		    return [Http NotModified $req]
		}
		dict set req -dynamic 1	;# functionals are dynamic by default
		return [$self functional $req $fpath]	;# invoke the .notfound
	    }
	} elseif {[file extension $fpath] eq $options(-functional)} {
	    # handle conditional request on functional path
	    if {[$self conditional $req $fpath]} {
		return [Http NotModified $req]
	    }

	    dict set req -dynamic 1	;# functionals are dynamic by default
	    return [$self functional $req $fpath]	;# invoke the functional
	} else {
	    set path $fpath

	    # handle conditional request on path
	    if {[$self conditional $req $path]} {
		return [Http NotModified $req]
	    }
	}

	# file $path exists
	Debug.mason {Found file '$path' of type [file type $path]}
	while {[file type $path] eq "link"} {
	    set path [file readlink $path]	;# remove links
	}

	switch -- [file type $path] {
	    file {
		# allow client caching
		return [Http CacheableFile $req $path]
	    }
	    
	    directory {
		# URL maps to a directory.
		if {![string match */ $url]} {
		    # redirect - insist on trailing /
		    return [Http Redirect $req "${url}/"]

		    # Question: Why should a URL that names a directory have
		    # a trailing slash?
		    # Answer:
		    # When a document contains relative links, they are resolved
		    # by the browser, not by the HTTP server.
		    # The browser starts with the URL for the current document,
		    # removes everything after the last slash, and appends the
		    # relative URL. If the URL for the current document names
		    # a file, this works fine, but if the URL for the current
		    # document names a directory, and the URL is missing the
		    # trailing slash, then the method fails.
		} elseif {$options(-index) ne ""} {
		    # we are instructed to use index.html (or similar)
		    # as the contents of a directory.
		    
		    # if there is an existing index file re-try this request
		    # after modifying the URL/path etc.
		    set fpath [$self candidate [file join $path $options(-index)]]
		    if {$fpath ne ""} {
			if {[file extension $fpath] eq $options(-functional)} {
			    # handle conditional request on functional path
			    if {[$self conditional $req $fpath]} {
				return [Http NotModified $req]
			    }
			    
			    dict set req -dynamic 1	;# functionals are dynamic by default
			    return [$self functional $req $fpath]	;# invoke the functional
			}
			return [Http CacheableFile $req $fpath]
		    }
		}

		Debug.mason {not found index $path - looking for .directory}
		set fpath [$self findUp $req .directory]
		if {$fpath eq ""} {
		    Debug.mason {notfound processing failed - really not here}
		    # respond notfound template
		    return [Http NotFound $req]
		}
		if {$options(-dirhead) ne {}} {
		    dict set req -thead $options(-dirhead)
		}
		if {$options(-dirfoot) eq {}} {
		    dict set req -tfoot [list [<a> href .. Up]]
		}
		dict set req -dynamic 1	;# functionals are dynamic by default
		return [$self functional $req $fpath]	;# invoke the functional
	    }
	    
	    default {
		dict lappend req -depends [file normalize $path]	;# cache notfound
		return [Http NotFound $req [subst {
		    [<p> "'$suffix' is of illegal type [file type $path]"]
		}]]
	    }
	}
    }
    
    method auth {req} {
	# run authentication and return any codes
	set fpath [$self findUp $req $options(-auth)]
	if {$fpath ne ""} {
	    Debug.mason {Mason got auth: $fpath}
	    
	    set req [$self template $req $fpath]

	    if {[dict get $req -code] != 200} {
		# auth preprocessing has an exception - we're done
		Debug.mason {Mason auth exception: [dict get $req -code]}
		return -code [dict get $req -code] -response $req
	    } else {
		Debug.mason {Mason auth OK}

		# auth passed - remove any traces
		catch {dict unset req -content}
		catch {dict unset req content-type}
	    }
	}

	return $req
    }

    method wrap {code rsp} {
	# run a wrapper over the content
	if {([dict exists $rsp -content])  && [string match 2* $code]} {
	    # filter/reprocess this response
	    set wrapper [$self findUp $rsp $options(-wrapper)]

	    if {$wrapper ne ""} {
		# found a wrapper
		# evaluate autopath template
		Debug.mason {MASON Respond autopath $options(-wrapper) - $wrapper}
		dict set req -wrapper $wrapper
		return [$self functional $rsp $wrapper]
		# never returns - straight to Respond
	    }
	}

	return [list $code $rsp]
    }

    method do {req} {
	Debug.dispatch {do}
	dict set req -root $options(-root)

	if {[dict exists $req -suffix]} {
	    # caller has munged path already
	    set suffix [dict get $req -suffix]
	} else {
	    # assume we've been parsed by package Url
	    # remove the specified prefix from path, giving suffix
	    set suffix [Url pstrip $options(-url) [dict get $req -path]]
	    if {[string match "/*" $suffix]} {
		# path isn't inside our domain suffix - error
		return [Http NotFound $req]
	    }
	    dict set req -suffix $suffix
	}

	set req [$self auth $req]	;# authenticate - must not be caught!
	dict set req -dynamic 0		;# default: static content
	set rsp [$self mason $req]	;# process request

	Debug.dispatch {MASON Respond $rsp}

	# filter/reprocess this response
	if {[string match 2* [Dict get? $rsp -code]] &&
	    ($options(-wrapper) ne "") &&
	    [dict exists $rsp -content] &&
	    ([set wrapper [$self findUp $rsp $options(-wrapper)]] ne "")
	} {
	    Debug.mason {MASON wrapper $options(-wrapper) - $wrapper}

	    # run template over request
	    set rsp [$self template $rsp $wrapper]
	    catch {dict unset rsp -root}

	    # determine whether content is dynamic or not
	    if {[dict exists $rsp -dynamic] && [dict get $rsp -dynamic]} {
		# it's completely dynamic - no caching
		return [Http NoCache $rsp]
	    } else {
		# this is able to be cached.
		return [Http CacheableContent $rsp [clock seconds]]
	    }
	}

	return $rsp
    }

    constructor {args} {
	$self configurelist $args
	set options(-root) [file normalize $options(-root)]

	if {$options(-dirhead) ne ""} {
	    # search for an element "*" in -dirhead
	    catch {unset attr}
	    file lstat $options(-root) attr
	    set oth [array get attr]
	    dict set oth name X
	    foreach {x y} [file attributes $options(-root)] {
		dict set oth $x $y
	    }
 
	    set i 0
	    set index -1
	    set hd {}
	    set rhead {}
	    foreach el $options(-dirhead) {
		if {$el eq "*"} {
		    set index $i
		    lappend rhead *
		    incr i
		} elseif {![catch {dict unset oth $el}]} {
		    lappend rhead $el
		    incr i
		}
	    }

	    if {$index ne -1} {
		set thead [lreplace $rhead $index $index {*}[lsort -dictionary [dict keys $oth]]]
		set options(-dirhead) $thead
	    }

	    #puts stderr "THEAD: $thead"
	}
    }
}

namespace eval ::MConvert {
    proc .x-text/dict.x-text/html-fragment {rsp} {
	Debug.convert {x-text/dict.x-text/html-fragment conversion: $rsp}

	# use -thead as table headers, or if there is none use the dict keys
	if {![dict exists $rsp -thead]} {
	    set thead [lsort [dict keys [lindex [dict get -contents] 1]]]
	} else {
	    set thead [dict get $rsp -thead]
	}

	dict set rsp -content [Html dict2table [dict get $rsp -content] $thead [Dict get? $rsp -tfoot]]

	if {[dict exists $rsp -title]} {
	    dict lappend rsp -headers [<title> [string trim [dict get $rsp -title]]]
	}
	set uroot [Dict get? $rsp -urlroot]
	foreach js {common css standardista-table-sorting} {
	    dict set rsp -script $uroot/scripts/$js.js {}
	}

	dict set rsp -style $uroot/css/sorttable.css) {}
	dict set rsp content-type x-text/html-fragment

	Debug.convert {x-text/dict.x-text/html-fragment conversion: $rsp}
	return $rsp
    }
}
