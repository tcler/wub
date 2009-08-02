# Mime - calculate the mime types of file
package require WubUtils
package require Debug
Debug on mime 10

package provide Mime 1.0

namespace eval Mime {
    variable mimecache
    variable file_attributes 1	;# should we seek file attributes to tell us?
    variable home [file dirname [info script]]
    variable e2m
    array set e2m {}

    variable default text/plain	;# default mime type
    variable prime mime.types	;# file to prime map

    # simple minded file extension<->mime-type map

    # add --
    #
    # 	add a MIME type mapping
    #
    # Arguments:
    #	suffix	A file suffix
    #	type	The corresponding MIME Content-Type.
    #
    # Results:
    #       None

    proc add {ext type} {
	variable e2m
	set ext [string tolower [string trimleft $ext .]]
	set e2m(.$ext) $type
	lappend e2m($type) .$ext
    }

    proc read {file} {
	package require fileutil
	variable e2m
	#puts stderr "Mime read $file"

	::fileutil::foreachLine line $file {
	    set line [string trim $line]
	    if {($line eq "") || [string match \#* $line]} {
		continue
	    }
	    regsub {[\t ]+} $line " " line
	    set line [split $line]
	    if {[llength $line] == 1} {
		# record a known type with no extension
		set e2m([lindex $line 0]) {}
	    } else {
		if {[string match {*/*} [lindex $line 0]]} {
		    foreach ext [lrange $line 1 end] {
			add $ext [lindex $line 0]
		    }
		} else {
		    add [lindex $line 0] [lindex $line 1]
		}
	    }
	}
    }

    proc MimeOf {ext} {
	variable e2m

	init	;# lazy init, to allow user customisation

	set ext ".[string trim [string tolower $ext] .]"
	if {[info exist e2m($ext)]} {
	    return $e2m($ext)
	} else {
	    return $e2m()
	}
    }

    # init the thing
    proc init {args} {
	#puts stderr "Mime init"
	variable home
	variable tie
	variable e2m
	variable prime

	if {[file pathtype $prime] eq "relative"} {
	    set prime [file join $home $prime]
	}

	# set the default mimetype
	variable default
	set e2m() $default

	catch {read $prime}
	proc init {args} {}	;# can only be initialized once.
    }

    array set mimecache {}
    proc type {file} {
	Debug.mime {MIME type $file}

	# could be cached
	variable mimecache
	if {[info exists mimecache($file)]} {
	    Debug.mime {MIME type $file cached: $mimecache($file)}
	    return $mimecache($file)
	}

	# filesystem may know file's mime type
	variable file_attributes
	if {$file_attributes
	    &&
	    (
	     ![catch {file attributes $file -mime} type]
	     &&
	     ($type != "")
	     )} {
	    # filesystem maintains -mime type attribute
	    set mimecache($file) $type
	    return $type
	}

	# some special file types have special mime types
	set ft [string tolower [file type $file]]
	switch -- $ft {
	    directory {
		return "multipart/x-directory"
	    }

	    characterspecial -
	    blockspecial -
	    fifo -
	    socket {
		return "application/x-$ft"
	    }
	}

	# possibly do mime magic
	variable mime_magic
	if {![info exists mime_magic]} {
	    # load mime-magic if it's about ... otherwise don't
	    if {[catch {package require mime-magic} r eo]} {
		Debug.mime {MIME err: $r ($eo)}
		set mime_magic 0
	    } else {
		set mime_magic 1
	    }
	}

	Debug.mime {MIME magic? $mime_magic}

	if {$mime_magic} {
	    if {![catch {
		::magic::open $file
		set mimecache($file) [::magic::/magic.mime]
		Debug.mime {MIME magic: $mimecache($file)}
		::magic::close
	    } r eo]} {
		if {$file_attributes} {
		    # record the finding for posterity
		    catch {file attributes $file -mime $mimecache($file)}
		}

		Debug.mime {MIME cache: $mimecache($file)}
		if {$mimecache($file) ne ""} {
		    return $mimecache($file)
		}
	    } else {
		Debug.mime {MAGIC error: $r ($eo)}
	    }
	}
	
	set mimecache($file) [MimeOf [file extension $file]]
	# (note: we don't store e2m mime types in file attributes) Why?

	Debug.mime {MIME ext: $mimecache($file)}
	return $mimecache($file)
    }

    namespace import 
    namespace export -clear *
    namespace ensemble create -subcommands {}
}
