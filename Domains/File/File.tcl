package require TclOO
namespace import oo::*

package require Debug
Debug define file 10

package provide File 2.0

package require Mime
package require Report
package require jQ

set ::API(Domains/File) {
    {
	provides a traditional Web view for filesystem hierarchies, maping a URL suffix to a file system path.

	File domain correctly interacts with Cache domain, and does not itself interpret content (except directories, which it presents by generating HTML.)

	File domain excludes certain files, matching the -hide regexp parameter.
    }
    root {filesystem root directory of File domain}
    indexfile {name of the file which stands for a directory, such as index.html}
    hide {a regexp to hide temp and other uninteresting files (default hides .* *~ and #*)}
    redirdir {flag: should references to directories be required to have a trailing /?}
    redirindex {flag: should references to directory indices be resolved or they can be left with just a trailing /?}
    expires {a tcl clock expression indicating when contents expire from caches.}
    dateformat {a tcl clock format for displaying dates in directory listings}
    nodir {don't allow the browsing of directories (default: 0 - browsing allowed.)}
    stream {files above this size will be streamed using fcopy, not loaded and sent}
    followextlinks {follow external symlinks}
    sortparam {parameters for tablesorter}
}

class create ::File {
    method dir {req path args} {
	Debug.file {dir over $path}
	dict set files .. [list name [<a> href .. ..] type parent]

	foreach file [glob -nocomplain -directory $path *] {
	    Debug.file {dir element $file}
	    set name [file tail $file]
	    if {[regexp {^([.].*)|(.*~)|(\#.*)$} $name]} continue

	    set type [Mime type $file]
	    if {$type eq "multipart/x-directory"} {
		set type directory
		append name /
	    }

	    set title [<a> href $name $name]
	    variable dateformat
	    catch {dict set files $name [list name $title modified [clock format [file mtime $file] -format $dateformat] size [file size $file] type $type]}
	}

	set suffix [dict get $req -suffix]
	set doctitle [string trimright $suffix /]
	append content [<h1> $doctitle] \n

	variable dirparams
	append content [Report html $files {*}$dirparams headers {name type modified size}] \n

	dict set req -content $content
	dict set req content-type x-text/html-fragment
	variable sortparam
	set req [jQ tablesorter $req .sortable {*}$sortparam]

	return $req
    }

    # get - given a request and suffix, construct a response
    method get {r suffix} {
	set ext [file extension $suffix]
	variable root
	set path [file join $root [string trimleft $suffix /]]
	variable mount
	Debug.file {file: root:'$root' mount:'$mount' suffix:'$suffix' ext:'$ext' path:'$path' -path:'[dict get $r -path]'}

	if {($ext ne "")
	    && ([file tail $suffix] eq $ext)
	    && ![dict exists $r -extonly]
	} {
	    # this is a file name like '.tml'
	    return [Http NotFound $r "<p>File '$suffix' has illegal name.</p>"]
	}

	if {![file exists $path]} {
	    # if the file doesn't exist, say so.
	    return [Http NotFound $r "<p>File '$suffix' doesn't exist</p>"]
	}

	# handle conditional request
	if {[dict exists $r if-modified-since]
	    && (![dict exists $r -dynamic] || ![dict get $r -dynamic])
	} {
	    set since [Http DateInSeconds [dict get $r if-modified-since]]
	    if {[file mtime $path] <= $since} {
		Debug.file {NotModified: $path - [Http Date [file mtime $path]] < [dict get $r if-modified-since]}
		Debug.file {if-modified-since: not modified}
		return [Http NotModified $r]
	    }
	}

	# allow client caching
	if {![dict exists $r -expiry]} {
	    variable expires
	    dict set r -expiry $expires
	}

	Debug.file {FILE DISPATCH '$path' $r}
	Debug.file {Found file '$path' of type [file type $path]}
	set count 20
	while {[incr count -1]} {
	    switch -- [file type $path] {
		link {
		    set lpath $path
		    set path [file readlink $path]
		    variable followextlinks
		    if {([file pathtype $path] eq "relative") || $followextlinks} {
			set path [file normalize [file join [file dirname $lpath] $path]]
		    }
		}

		file {
		    variable crealm
		    set r [Http Cache $r [dict get $r -expiry] $crealm]
		    variable stream
		    if {[file size $path] > $stream} {
			# this is a large file - stream it using fcopy
			tailcall Http File $r $path
		    } else {
			# this is a small file - load then send
			tailcall Http CacheableFile $r $path
		    }
		}

		directory {
		    # if a directory reference doesn't end in /, redirect.
		    Debug.file {redirecting path:$path, suffix:$suffix, -path:[dict get $r -path]}
		    set rpath [dict get $r -path]
		    variable redirdir
		    if {$redirdir && ([string index $rpath end] ne "/")} {
			dict set r -path "$rpath/"
			tailcall Http Redirect $r [Url uri $r]
		    } else {
			# TODO do something to return the whole dir in one hit
		    }

		    # try to return an index file's contents in lieue of the directory
		    variable indexfile
		    if {$indexfile ne ""} {
			set indices [glob -nocomplain -tails -directory $path $indexfile]
			if {[llength $indices]} {
			    dict set r -path [file join [dict get $r -path] [lindex $indices 0]]
			    variable redirindex
                            if {$redirindex} {
                                tailcall Http Redirect $r [Url uri $r]
                            } else {
                                set path [file join $path [lindex $indices 0]]
                                # allow client caching
				variable crealm
                                set r [Http Cache $r [dict get $r -expiry] $crealm]
				variable stream
                                if {[file size $path] > $stream} {
                                    # this is a large file - stream it using fcopy
                                    tailcall Http File $r $path
                                } else {
                                    # this is a small file - load then send
                                    tailcall Http CacheableFile $r $path
                                }
                            }
			}
		    }
		    variable nodir
		    if {$nodir} {
			tailcall Http NotFound $r "<p>No Such Directory.</p>"
		    } else {
			# no index file - generate a directory listing
			set r [my dir $r $path]
			variable crealm
			tailcall Http CacheableContent [Http Cache $r [dict get $r -expiry] $crealm] [clock seconds]
		    }
		}

		default {
		    variable crealm
		    set r [Http Cache $r [dict get $r -expiry] $crealm]
		    tailcall Http NotFound $r "<p>File '$suffix' is of illegal type [file type $path]</p>"
		}
	    }
	}

	tailcall Http NotFound $r "<p>File '$suffix' doesn't resolve to a file.</p>"
    }

    method do {r} {
	# calculate the suffix of the URL relative to $mount
	variable mount
	lassign [Url urlsuffix $r $mount] result r suffix
	if {!$result} {
	    return $r	;# the URL isn't in our domain
	}

	variable lambda
	if {[info exists lambda]} {
	    set r [my get $r $suffix]
	    tailcall ::apply $lambda $r
	} else {
	    tailcall my get $r $suffix
	}
    }

    constructor {args} {
	variable indexfile "index.*"
	variable nodir 0
	variable mount /
	variable hide {^([.].*)|(.*~)|(\#.*)$}
	variable redirdir 1	;# redirect dir to dir/
	variable redirindex 1	;# redirect dir/ to dir/index.html
	variable expires 0	;# add an expiry to each response
	variable crealm ""	;# optionally make files 'public'
	variable dateformat "%Y %b %d %T"
	variable dirparams {
	    sortable 1
	    evenodd 0
	    class table
	    tparam {title "Registry for this class"}
	    hclass header
	    hparam {title "click to sort"}
	    thparam {class thead}
	    fclass footer
	    tfparam {class tfoot}
	    rclass row
	    rparam {}
	    eclass el
	    eparam {}
	    footer {}
	}
	#set stream [expr {100 * 1024 * 1024}]	;# default streaming 100Mb
	variable stream [expr {1024 * 1024}]	;# default streaming 1Mb
 	variable followextlinks no
 	variable sortparam {}
	variable {*}[Site var? File] {*}$args	;# allow .ini file to modify defaults
    }
    destructor {}
 }
