package require TclOO
namespace import oo::*

package require Debug
Debug off file 10

package provide File 2.0

package require Report
package require jQ

class create File {

    method dir {req path args} {
	Debug.file {dir over $path}
	dict set files .. [list name [<a> href .. ..] type parent]
	#my variable root hide dirtime

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
	    catch {dict set files $name [list name $title modified [clock format [file mtime $file] -format $dirtime] size [file size $file] type $type]}
	}

	set suffix [dict get $req -suffix]
	set doctitle [string trimright $suffix /]
	append content [<h1> $doctitle] \n

	#my variable dirparams
	append content [Report html $files {*}$dirparams headers {name type modified size}] \n

	dict set req -content $content
	dict set req content-type x-text/html-fragment
	set req [jQ tablesorter $req .sortable]

	return $req
    }

    method do {req} {
	#my variable prefix root

	if {[dict exists $req -suffix]} {
	    # caller has munged path already
	    set suffix [dict get $req -suffix]
	} else {
	    # assume we've been parsed by package Url
	    # remove the specified prefix from path, giving suffix
	    set suffix [Url pstrip $prefix [dict get $req -path]]
	    if {($suffix ne "/") && [string match "/*" $suffix]} {
		# path isn't inside our domain suffix - error
		return [Http NotFound $req]
	    }
	    dict set req -suffix $suffix
	}

	set ext [file extension $suffix]
	set path [file join $root [string trimleft $suffix /]]

	Debug.file {file: root:$root prefix:$prefix suffix:$suffix path:$path -path:[dict get $req -path]}

	if {($ext ne "")
	    && ([file tail $suffix] eq $ext)
	    && ![dict exists $req -extonly]
	} {
	    # this is a file name like '.tml'
	    return [Http NotFound $req "<p>File '$suffix' has illegal name.</p>"]
	}
	
	if {![file exists $path]} {
	    # if the file doesn't exist, say so.
	    return [Http NotFound $req "<p>File '$suffix' doesn't exist</p>"]
	}

	# handle conditional request
	if {[dict exists $req if-modified-since]
	    && (![dict exists $req -dynamic] || ![dict get $req -dynamic])
	} {
	    set since [Http DateInSeconds [dict get $req if-modified-since]]
	    if {[file mtime $path] <= $since} {
		Debug.file {NotModified: $path - [Http Date [file mtime $path]] < [dict get $req if-modified-since]}
		Debug.file {if-modified-since: not modified}
		return [Http NotModified $req]
	    }
	}
	
	Debug.file {FILE DISPATCH '$path' $req}
	#my variable expires redirdir
	Debug.file {Found file '$path' of type [file type $path]}
	switch -- [file type $path] {
	    link -
	    file {
		# allow client caching
		set r [Http Cache $req $expires]
		return [Http CacheableFile $r $path]
	    }
	    
	    directory {
		# if a directory reference doesn't end in /, redirect.
		Debug.file {redirecting path:$path, suffix:$suffix, -path:[dict get $req -path]}
		set rpath [dict get $req -path]
		if {$redirdir && ([string index $rpath end] ne "/")} {
		    dict set req -path "$rpath/"
		    return [Http Redirect $req [Url uri $req]]
		} else {
		    # TODO do something to return the whole dir in one hit
		}

		# try to return an index file's contents in lieue of the directory
		#my variable index
		if {$index ne ""} {
		    set indices [glob -nocomplain -tails -directory $path]
		    if {[llength $indices]} {
			dict set req -path [file join $root [lindex $indices 0]]
			return [Http Redirect $req [Url uri $req]]
		    }
		}

		# no index file - generate a directory listing
		set req [my dir $req $path]
		return [Http CacheableContent [Http Cache $req $expires] [clock seconds]]
	    }
	    
	    default {
		set req [Http Cache $req $expires]
		return [Http NotFound $req "<p>File '$suffix' is of illegal type [file type $path]</p>"]
	    }
	}
    }

    variable index prefix hide redirdir expires dirtime dirparams

    constructor {args} {
	set index "index.*"
	set prefix /
	set hide {^([.].*)|(.*~)|(\#.*)$}
	set redirdir 1	;# redirect dir to dir/
	set expires 0	;# add an expiry to each response
	set dirtime "%Y %b %d %T"
	set dirparams {
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

	foreach {n v} $args {
	    set n [string trim $n -]
	    #my variable $n
	    set $n $v
	}
    }
    destructor {}
 }
