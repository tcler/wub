# Mason -
#
# A domain to emulate Mason (http://www.masonhq.com/)

package provide Mason 1.0

package require snit
package require Html

::snit::type Mason {
    option -root ""	;# file system domain root
    option -hide {^([.].*)|(.*~)$}	;# these files are never matched
    option -functional ".tml"	;# functional extension
    option -notfound ".notfound"	;# notfound handler name
    option -wrapper ".wrapper"	;# wrapper handler name
    option -auth ".auth"	;# authentication functional
    option -index index.html	;# directory index name
    option -dirread 0		;# are directories inherently readable?
    			;# -1: probe, 0: never, 1: always
    option -dirlist 1		;# return a multipart/x-dirlist type for directories?

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
	    dict set response content-type text/x-html-fragment
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

    method mason {req} {
	Debug.mason {Mason: [dumpMsg $req]}

	dict set req -mason $self
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
	    return [Http NotFound $req "<p>'$path' has illegal name.</p>"]
	}

	# .notfound processing
	if {![file exists $path] || $ext eq $options(-functional)} {
	    # no such file - may be a functional
	    set fpath [file rootname $path]$options(-functional)
	    if {![file exists $fpath]} {
		Debug.mason {not found $fpath - looking for $options(-notfound)}
		set fpath [$self findUp $req $options(-notfound)]	;# get the .notfound
	    }

	    if {$fpath eq ""} {
		Debug.mason {notfound processing failed - really not here}

		# respond notfound template
		dict set req -depends [file join [dict get $req -root] $suffix]
		return [Http NotFound $req]
	    }

	    # handle conditional request - return if it's unmodified
	    $self conditional $req $fpath

	    dict set req -functional $fpath
	    dict set req -dynamic 1	;# functionals are dynamic by default

	    return [$self functional $req $fpath]	;# invoke the functional
	}

	# handle conditional request on path
	if {[$self conditional $req $path]} {
	    return [Http NotModified $req]
	}

	Debug.mason {Found file '$path' of type [file type $path]}
	switch -- [file type $path] {
	    link -
	    file {
		# allow client caching
		return [Http CacheableFile $req $path]
	    }
	    
	    directory {
		# URL maps to a directory.
		if {![string match */ $url]} {
		    # redirect - insist on trailing /
		    return [Http Redirect $req "${url}/"]

		    # Question: Why should a URL that names a directory have a trailing slash?
		    # Answer:
		    # When a document contains relative links, they are resolved by the browser, 
		    # not by the HTTP server. The browser starts with the URL for the current 
		    # document, removes everything after the last slash, and appends the 
		    # relative URL. If the URL for the current document names a file, this works 
		    # fine. But if the URL for the current document names a directory, and the 
		    # URL is missing the trailing slash, then the method fails.
		}

		if {[set readable $options(-dirread)] < 0} {
		    # test underlying fs to see if it accepts dir read.
		    set dir [open $path]
		    set readable [expr ![catch {read $dir 1}]]
		    #set options(-dirread) $readable
		    close $dir

		    # Justification: Some file systems permit a thing to be both a collection
		    # of other things (a directory) and a content-ful thing, simultaneously.
		    # Unix fs does not, but (say) the URL namespace does.  We have metakit
		    # vfs which also support this duality, and it makes sense to make use of
		    # it.
		    # To that end, we attempt a simple read on each directory, and if it fails
		    # we fall back to the usual circumlocution ($dir/index.*)
		}

		if {$readable} {
		    # the underlying fs supports directory reads at this location
		    # just return the thing as a file, Conversion is expected to
		    # make some sense of it.
		    return [Http CacheableFile $req $path multipart/x-directory]
		} elseif {($options(-index) ne "")} {
		    # we are instructed to use index.html (or similar)
		    # as the contents of a directory.

		    # re-try this request after modifying the URL/path etc.
		    dict set req -suffix [file join [dict get $req -suffix] $options(-index)]

		    # remember cache dependency on dir
		    dict lappend req -depends [file normalize $path]

		    # remember the query args - order should be significant?
		    dict lappend req -cache [dict get $req -uri]

		    set req [$self mason $req]	;# recurse to find ./index.html
		    if {[Dict get? $req -code] != 404} {
			return $req
		    }
		}

		# couldn't find an index file
		dict lappend req -depends [file normalize $path]	;# cache directory
		if {$options(-dirlist)} {
		    set content [dict create]
		    foreach name [glob -directory $path *] {
			switch -- [file type $name] {
			    link {
				set file [file readlink $name]
			    }
			    file - directory {
				set file $name
			    }
			    default {
				continue
			    }
			}
			file stat $file attr
			dict set content [file tail $name] \
			    [dict merge [file attributes $file] [array get attr]]
		    }

		    #puts stderr "DIR: $content"

		    return [Http CacheableContent $req \
				[file mtime $path] $content "multipart/x-dirlist"]
		} else {
		    return [Http NotFound $req]
		}
	    }

	    default {
		dict lappend req -depends [file normalize $path]	;# cache notfound
		return [Http NotFound $req \
			    "<p>'$suffix' is of illegal type [file type $path]</p>"]
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
	dict set req -domain $self	;# record the domain
	dict set req -root $options(-root)
	set req [$self auth $req]	;# authenticate - must not be caught!
	dict set req -dynamic 0	;# default: static content
	set rsp [$self mason $req]	;# process request
	catch {dict unset rsp -domain}	;# forget the domain

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
    }
}
