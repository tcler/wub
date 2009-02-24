# Mason -
#
# A domain to emulate Mason (http://www.masonhq.com/)

package require Html
package require TclOO
namespace import oo::*

package require Debug
Debug off mason 10

package provide Mason 1.0

set API(Mason) {
    {A File-like domain, but on steroids.  Allows the definition of templates in each directory.
	Templates are run before (auth) and after (wrapper) satisfying requests, and may completely transform request and response (respectively.)
	If a requested file can't be located, the per-directory notfound template is evaluated in its stead.
	Any file ending with $functional extension is considered to be a template, and is evaluated and its value returned.
	auth, wrapper and notfound templates are searched for in parent directories, up to the root of the Mason domain.
    }
    root {Filesystem root for this domain}
    hide {a regexp to hide temp and other files (default hides .* *~ and #*)}
    functional {file extension which marks tcl scripts to be evaluated for value (default .tml)}
    notfound {template evaluated when a requested resource can't be located (default .notfound)}
    wrapper {template evaluated with successful response (default .wrapper)}
    auth {template evaluated before searching for requested file (default .auth)}
    indexfile {a file which stands for a directory (default index.html)}
}

class create Mason {
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
	#my variable cache
	if {$cache} {
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
		dict set response code 200
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
	set rsp [my template $req $fpath]
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
	#my variable functional
	set fpath [file rootname $file]$functional
	Debug.mason {candidate $fpath - [file exists $fpath]}
	if {[file exists $fpath]} {
	    return $fpath
	} else {
	    return ""
	}
    }

    method mason {req} {
	Debug.mason {Mason: [dumpMsg $req]}
	
	dict set req -mason [self]
	#my variable mount
	dict set req -urlroot $mount

	set http [Dict get? $req -http]
	set suffix [string trimleft [dict get $req -suffix] /]
	
	set ext [file extension $suffix]	;# file extent
	set path [file join [dict get $req -root] $suffix] ;# complete path to file
	set tail [file tail $suffix]	;# last component of path
	set url [dict get $req -url]	;# full URL
	
	Debug.mason {Mason: -url:$url - suffix:$suffix - path:$path - tail:$tail - ext:$ext}
	#my variable hide
	if {(($tail eq $ext) && ($ext ne "")
	     && ![dict exists $req -extonly])
	    || [regexp $hide $tail]
	} {
	    # this is a file name like '.../.tml', or is hidden
	    Debug.mason {notfound failed - illegal name}
	    return [Http NotFound $req [subst {
		[<p> "'$path' has illegal name.</p>"]
	    }]]
	}
	
	# .notfound processing
	#my variable notfound functional
	set fpath [my candidate $path]
	if {$fpath eq ""} {
	    Debug.mason {not found $fpath - looking for $notfound}
	    set fpath [my findUp $req $notfound]	;# get the .notfound
	    if {$fpath eq ""} {
		Debug.mason {notfound failed - really not here}
		# respond notfound template
		return [Http NotFound $req]
	    } else {
		# handle conditional request on .notfound
		if {[my conditional $req $fpath]} {
		    return [Http NotModified $req]
		}
		dict set req -dynamic 1	;# functionals are dynamic by default
		return [my functional $req $fpath]	;# invoke the .notfound
	    }
	} elseif {[file extension $fpath] eq $functional} {
	    # handle conditional request on functional path
	    if {[my conditional $req $fpath]} {
		return [Http NotModified $req]
	    }

	    dict set req -dynamic 1	;# functionals are dynamic by default
	    return [my functional $req $fpath]	;# invoke the functional
	} else {
	    set path $fpath

	    # handle conditional request on path
	    if {[my conditional $req $path]} {
		return [Http NotModified $req]
	    }
	}

	# file $path exists
	Debug.mason {Found file '$path' of type [file type $path]}
	set cnt 20
	while {[file type $path] eq "link" && [incr cnt -1]} {
	    # chase down links
	    set lpath $path
	    set path [file readlink $path]
	    if {[file pathtype $path] eq "relative"} {
		set path [file normalize [file join [file dirname $lpath] $path]]
	    }
	}
	if {!$cnt} {
	    return [Http NotFound $req "File path has too many symlinks"]
	}
	switch -- [file type $path] {
	    file {
		# allow client caching
		return [Http CacheableFile $req $path]
	    }
	    
	    directory {
		# URL maps to a directory.
		#my variable indexfile functional
		if {![string match */ $url]} {
		    # redirect - insist on trailing /
		    Debug.mason {Redirecting, as url '$url' doesn't end in a /, but '$path' a directory}
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
		} elseif {$indexfile ne ""} {
		    # we are instructed to use index.html (or similar)
		    # as the contents of a directory.
		    
		    # if there is an existing index file re-try this request
		    # after modifying the URL/path etc.
		    set fpath [my candidate [file join $path $indexfile]]
		    if {$fpath ne ""} {
			if {[file extension $fpath] eq $functional} {
			    # handle conditional request on functional path
			    if {[my conditional $req $fpath]} {
				return [Http NotModified $req]
			    }
			    
			    dict set req -dynamic 1	;# functionals are dynamic by default
			    return [my functional $req $fpath]	;# invoke the functional
			}
			return [Http CacheableFile $req $fpath]
		    }
		}

		Debug.mason {not found index $path - looking for .directory}
		set fpath [my findUp $req .directory]
		if {$fpath eq ""} {
		    Debug.mason {notfound processing failed - really not here}
		    # respond notfound template
		    return [Http NotFound $req]
		}
		#my variable dirhead
		if {$dirhead ne {}} {
		    dict set req -thead $dirhead
		}
		#my variable dirfoot
		if {$dirfoot eq {}} {
		    dict set req -tfoot [list [<a> href .. Up]]
		}
		dict set req -dynamic 1	;# functionals are dynamic by default
		return [my functional $req $fpath]	;# invoke the functional
	    }
	    
	    default {
		dict lappend req -depends [file normalize $path]	;# cache notfound
		Debug.mason {Mason illegal type [file type $path]}
		return [Http NotFound $req [subst {
		    [<p> "'$suffix' is of illegal type [file type $path]"]
		}]]
	    }
	}
    }
    
    method auth {req} {
	# run authentication and return any codes
	#my variable auth
	set fpath [my findUp $req $auth]
	if {$fpath ne ""} {
	    Debug.mason {Mason got auth: $fpath}
	    
	    set req [my template $req $fpath]

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
	    #my variable wrapper
	    set wrap [my findUp $rsp $wrapper]

	    if {$wrap ne ""} {
		# found a wrapper
		# evaluate autopath template
		Debug.mason {MASON Respond autopath $wrapper - $wrap}
		dict set req -wrapper $wrap
		return [my functional $rsp $wrap]
		# never returns - straight to Respond
	    }
	}

	return [list $code $rsp]
    }

    method do {req} {
	#my variable root mount
	dict set req -root $root

	if {[dict exists $req -suffix]} {
	    # caller has munged path already
	    set suffix [dict get $req -suffix]
	} else {
	    # assume we've been parsed by package Url
	    # remove the specified prefix from path, giving suffix
	    set suffix [Url pstrip $mount [string trimleft [dict get $req -path] /]]
	    Debug.mason {suffix:$suffix url:$mount}
	    if {($suffix ne "/") && [string match "/*" $suffix]} {
		# path isn't inside our domain suffix - error
		Debug.mason {[dict get $req -path] is outside domain suffix $suffix}
		return [Http NotFound $req]
	    }
	    dict set req -suffix $suffix
	}

	Debug.mason {do $suffix}

	set req [my auth $req]	;# authenticate - must not be caught!
	dict set req -dynamic 0		;# default: static content
	set rsp [my mason $req]	;# process request

	Debug.mason {processed $rsp}

	# filter/reprocess this response
	#my variable wrapper
	if {[string match 2* [Dict get? $rsp -code]] &&
	    ($wrapper ne "") &&
	    [dict exists $rsp -content] &&
	    ([set wrap [my findUp $rsp $wrapper]] ne "")
	} {
	    Debug.mason {wrapper $wrapper - $wrap}

	    # run template over request
	    set rsp [my template $rsp $wrap]
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

	Debug.mason {default response $rsp}
	return $rsp
    }

    variable mount root hide functional notfound wrapper auth indexfile dirhead dirfoot aliases cache

    constructor {args} {
	set mount ""	;# url for top of this domain
	set root ""		;# file system domain root
	set hide {^([.].*)|(.*~)$}	;# these files are never matched
	set functional ".tml"	;# functional extension
	set notfound ".notfound"	;# notfound handler name
	set wrapper ".wrapper"	;# wrapper handler name
	set auth ".auth"	;# authentication functional
	set indexfile index.html	;# directory index name
	set dirhead {name size mtime *}
	set dirfoot {}
	# additional aliases to be installed in session interpreter
	set aliases {}
	
	# when a file is not located, it will be searched for.
	# to minimise the cost of this search, -cache will
	# instruct Mason to memoize found files
	set cache 1		;# cache file searches

	foreach {n v} $args {
	    set [string trimleft $n -] $v
	}

	set root [file normalize $root]

	if {$dirhead ne ""} {
	    # search for an element "*" in -dirhead
	    catch {unset attr}
	    file lstat $root attr
	    set oth [array get attr]
	    dict set oth name X
	    foreach {x y} [file attributes $root] {
		dict set oth $x $y
	    }
 
	    set i 0
	    set index -1
	    set hd {}
	    set rhead {}
	    foreach el $dirhead {
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
		set dirhead $thead
	    }
	}
    }
}

package require Convert

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

Convert Namespace ::MConvert	;# add Mason conversions
