# File -
#
# A domain to present a file system as a series of URLs

package provide File 1.0
#Debug on file 10

package require snit
package require Query
package require Mime

# TODO - handle dangling softlinks in dirlist
# TODO - some kind of permissions system

::snit::type File {
    option -prefix "/"
    option -root ""
    option -hide {^([.].*)|(.*~)$}
    option -superimpose 0
    option -redirdir 1	;# redirect dir to dir/
    option -expires 0	;# add an expiry to each output

    method dirList {req} {
	set files {}
	set suffix [dict get $req -suffix]
	set fulldir [file join $options(-root) [string trim $suffix /]]
	foreach file [glob -nocomplain -tails -directory $fulldir *] {
	    set fp [file join $fulldir $file]
	    if {![regexp $options(-hide) [file tail $file]]} {
		if {[catch {file mtime $fp} mtime]} {
		    lappend files [list $file 0 0]
		} else {
		    lappend files [list $file $mtime [file size $fp]]
		}
	    }
	}

	array set query [Query flatten [Query parse $req]]
	if {[info exists query(sort)]} {
	    set sort [string tolower $query(sort)]
	} else {
	    set sort name
	}

	array set sorter {name sort=name date sort=date size sort=size}

	if {[info exists query(reverse)]} {
	    set order -decreasing
	} else {
	    set order -increasing
	    append sorter($sort) "&reverse"
	}

	switch -- $sort {
	    name {
		set files [lsort -index 0 $order -dictionary $files]
	    }
	    date {
		set files [lsort -index 1 $order -integer $files]
	    }
	    size {
		set files [lsort -index 2 $order -integer $files]
	    }
	}

	set dp [dict get $req -path]
	set url [string trimright [dict get $req -url] /]
	Debug.file {dirList: $dp - $url - $suffix - ($files)}
	
	set dirlist "<table class='dirlist' border='1'>\n"
	append dirlist "<thead>" \n
	append dirlist "<tr><th><a href='${dp}?$sorter(name)'>Name</a></th>" \n
	append dirlist "<th><a href='${dp}?$sorter(date)'>Modified</a></th>" \n
	append dirlist "<th><a href='${dp}?$sorter(size)'>Size</a></th></tr>" \n

	set pdir [file dirname [string trimright ${dp} /]]
	append dirlist "<tr><td><a href='${pdir}'>..</a></td></tr>" \n
	append dirlist "</thead>" \n

	append dirlist "<tbody>" \n
	foreach file $files {
	    lassign $file name date size
	    append dirlist "<tr>\n"
	    append dirlist "<td><a href='${dp}$name'>$name</a></td>" \n
	    append dirlist "<td>[Http Date $date]</td>" \n
	    append dirlist "<td>$size</td>" \n
	    append dirlist "</tr>" \n
	}
	append dirlist "</tbody>" \n
	append dirlist "</table>" \n

	set dir [dict get $req -path]

	set result "title:${dir} Directory\n"
	append result "<h1>$dir</h1>" \n
	append result $dirlist \n

	return $result
    }

    proc f2dict {fname file baseurl} {
	dict set result $fname -modified [file mtime $file]
	dict set result $fname -suffix [file join $baseurl $fname]
	set ftype [Mime type $file]
	if {$ftype ne ""} {
	    dict set result $fname -type $ftype
	}
	return $result
    }
    
    # can this domain handle the given glob?
    # returns a dict from url to -modified time and (possibly) mime -type
    method handle? {glob} {
	if {[file tail $glob] ne $glob} {
	    set baseurl [file dirname $glob]
	    set basedir [file join $options(-root) $baseurl]
	    set glob [file tail $glob]
	    set ext [file extension $glob]
	    set glob [file root $glob]
	} else {
	    set baseurl ""
	    set basedir $options(-root)
	    set ext ""
	}
	
	set result [dict create]
	foreach x [list [file join $basedir $glob] [file join $basedir ${glob}.$ext]] {
	    if {[file exists $x]} {
		Dict modify result {*}[f2dict $glob $x $baseurl]
	    }
	}

	Debug.file {handle? $basedir - $glob ([glob -nocomplain -directory $basedir -tails $glob.*])}
	foreach fname [glob -nocomplain -directory $basedir -tails ${glob}.*] {
	    set file [file join $basedir $fname]
	    Dict modify result {*}[f2dict $fname $file $baseurl]
	}
	Debug.file {handle? result: $result}
	return $result
    }

    method conditional {req path} {
	# check conditional
	if {[dict exists $req if-modified-since]
	    && (![dict exists $req -dynamic] || ![dict get $req -dynamic])
	} {
	    set since [Http DateInSeconds [dict get $req if-modified-since]]
	    if {[file mtime $path] <= $since} {
		Debug.file {NotModified: $path - [Http Date [file mtime $path]] < [dict get $req if-modified-since]}
		Debug.file {if-modified-since: not modified}
		return [list [Http NotModified $req] 1]
	    }
	}
	return [list $req 0]	;# carry on
    }

    method do {req} {
	Debug.dispatch {File}
	Debug on file 10
	if {[dict exists $req -suffix]} {
	    # caller has munged path already
	    set suffix [dict get $req -suffix]
	} else {
	    # assume we've been parsed by package Url
	    # remove the specified prefix from path, giving suffix
	    set suffix [Url pstrip $options(-prefix) [dict get $req -path]]
	    if {($suffix ne "/") && [string match "/*" $suffix]} {
		# path isn't inside our domain suffix - error
		return [Http NotFound $req]
	    }
	    dict set req -suffix $suffix
	}

	set ext [file extension $suffix]
	set path [file join $options(-root) $suffix]
	#dict set req -path $path

	Debug.file {file: $suffix - $path - [dict get $req -path]}

	if {($ext ne "")
	    && ([file tail $suffix] eq $ext)
	    && ![dict exists $req -extonly]
	} {
	    # this is a file name like '.tml'
	    return [Http NotFound $req "<p>File '$suffix' has illegal name.</p>"]
	}

	if {![file exists $path]} {
	    if {([file extension $path] ne ".*")
		&& ([file extension $path] ne "")} {
		dict lappend req -depends $path	;# cache notfound
		return [Http NotFound $req "<p>File '$suffix' doesn't exist</p>"]
	    } else {
		# glob for an alternative
		set suffix [file root $suffix]
		set alternatives [$self handle? $suffix]
		Debug.file {Got alternatives: $alternatives - $suffix}
		if {[dict size $alternatives] == 0} {
		    dict lappend req -depends $path	;# cache notfound
		    return [Http NotFound $req "<p>File '$suffix' doesn't exist</p>"]
		} else {
		    # found alternatives.  Pick one and proceed.
		    set key [lindex [dict keys $alternatives] 0]
		    set suffix [dict get $alternatives $key -suffix]
		    set path [file join $options(-root) $suffix]
		    dict lappend req -depends $path	;# cache dependency
		    set mtime [dict get $alternatives $key -modified]
		    dict set req -modified $mtime
		    dict set req last-modified [Http Date $mtime]
		    Debug.file {Chose alternative: $path - $options(-root) - $suffix -> [Url uri $req]}
		    return [Http Redirect $req [Url uri $req]]
		}
	    }
	}

	# handle conditional request
	if {[lassign [$self conditional $req $path] req]} {
	    return $req
	}
	
	Debug.file {FILE DISPATCH '$path' $req}
	
	Debug.file {Found file '$path' of type [file type $path]}
	switch -- [file type $path] {
	    link -
	    file {
		# allow client caching
		set r [Http Cache $req $options(-expires)]
		return [Http CacheableFile $r $path]
	    }
	    
	    directory {
		# if a directory reference doesn't end in /, redirect.
		Debug.file {redirecting path:$path, suffix:$suffix, -path:[dict get $req -path]}
		set rpath [dict get $req -path]
		if {$options(-redirdir) && ([string index $rpath end] ne "/")} {
		    dict set req -path "$rpath/"
		    return [Http Redirect $req [Url uri $req]]
		} else {
		    # TODO do something to return the whole dir in one hit
		}

		set indices [$self handle? [file join $suffix index]]
		if {[dict size $indices] != 0} {
		    # we're going to re-try this request
		    # after modifying the URL/path etc.
		    set path [dict get $req -path]
		    set key [lindex [dict keys $indices] 0]
		    dict set req -path [file join $path $key]
		    
		    # re-dispatch to allow caching, etc
		    return [Http Redirect $req [Url uri $req]]
		}
		
		# remember cache dependency on dir
		dict lappend req -depends [file normalize $path]

		# remember the query args - order should be significant?
		dict lappend req -cache [dict get $req -uri]

		# allow client caching
		# do respond CacheableContent $req
		return [Http CacheableContent [Http Cache $req $options(-expires)] [clock seconds] [$self dirList $req] x-text/system]
	    }
	    
	    default {
		dict lappend req -depends $path	;# cache notfound
		set req [Http Cache $req $options(-expires)]
		return [Http NotFound $req "<p>File '$suffix' is of illegal type [file type $path]</p>"]
	    }
	}
    }

    method Dispatch {req} {
	Debug.dispatch {Dispatch}
	set http [dict get $req -http]

	if {$options(-superimpose)} {
	    # superimpose a single directory's contents everywhere
	    set suffix [file tail [dict get $req -suffix]]
	} else {
	    set suffix [dict get $req -suffix]
	}

	set ext [file extension $suffix]
	set path [file join $options(-root) $suffix]
	
	if {$options(-expires) ne ""} {
	    dict set req expires [Http Date [clock scan $options(-expires)]]
	}

	Debug.file {File Dispatch: $suffix - $path - [dict get $req -path]}

	if {($ext ne "")
	    && ([file tail $suffix] eq $ext)
	    && ![dict exists $req -extonly]
	} {
	    # this is a file name like '.tml'
	    do respond NotFound $req "<p>File '$suffix' has illegal name.</p>"
	}

	if {![file exists $path]} {
	    if {([file extension $path] ne ".*")
		&& ([file extension $path] ne "")} {
		dict lappend req -depends $path	;# cache notfound
		do respond NotFound $req "<p>File '$suffix' doesn't exist</p>"
	    } else {
		# glob for an alternative
		set suffix [file root $suffix]
		set alternatives [$self handle? $suffix]
		Debug.file {Got alternatives: $alternatives - $suffix}
		if {[dict size $alternatives] == 0} {
		    dict lappend req -depends $path	;# cache notfound
		    do respond NotFound $req "<p>File '$suffix' doesn't exist</p>"
		} else {
		    # found alternatives.  Pick one and proceed.
		    set key [lindex [dict keys $alternatives] 0]
		    set suffix [dict get $alternatives $key -suffix]
		    set path [file join $options(-root) $suffix]
		    dict lappend req -depends $path	;# cache dependency
		    set mtime [dict get $alternatives $key -modified]
		    dict set req -modified $mtime
		    dict set req last-modified [Http Date $mtime]
		    Debug.file {Chose alternative: $path - $options(-root) - $suffix}
		}
	    }
	}

	# handle conditional request
	$self conditional $req $path
	
	Debug.file {FILE DISPATCH '$path' $req}
	
	Debug.file {Found file '$path' of type [file type $path]}
	switch -- [file type $path] {
	    link -
	    file {
		# allow client caching
		return [Http CacheableFile [Http Cache $req $options(-expires)] [clock seconds] $path]
	    }
	    
	    directory {
		# if a directory reference doesn't end in /, redirect.
		set rpath [dict get $req -path]
		if {$options(-redirdir) && ([string index $rpath end] ne "/")} {
		    dict set req -path "[dict get $req -path]/"
		    return [Http Redirect $req [Url uri $req]]
		} else {
		    # TODO do something to return the whole dir in one hit
		}

		set indices [$self handle? [file join $suffix index]]
		if {[dict size $indices] != 0} {
		    # we're going to re-try this request
		    # after modifying the URL/path etc.
		    set path [dict get $req -path]
		    set key [lindex [dict keys $indices] 0]
		    dict set req -path [file join $path $key]
		    
		    # re-dispatch to allow caching, etc
		    return [Http Redirect $req [Url uri $req]]
		}
		
		# remember cache dependency on dir
		dict lappend req -depends [file normalize $path]

		# remember the query args - order should be significant?
		dict lappend req -cache [dict get $req -uri]

		# allow client caching
		# do respond CacheableContent $req
		return [Http CacheableContent [Http Cache $req] [clock seconds] [$self dirList $req] x-text/system]
	    }
	    
	    default {
		dict lappend req -depends $path	;# cache notfound
		return [Http NotFound $req "<p>File '$suffix' is of illegal type [file type $path]</p>"]
	    }
	}
    }
    
    constructor {args} {
	$self configurelist $args
    }
}
