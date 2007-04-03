package provide Mime 1.0

package require cmdline
package require tie
package require WubUtils

# E2M - simple minded file extension->mime-type map
# persisted using ::tie
namespace eval E2M {
    variable home ""

    variable e2m
    array set e2m {}
    variable initialized 0

    variable default text/plain	;# default mime type
    variable prime ~/mime.types	;# file to prime map

    variable tie

    # Tie configuration variables, each of which can be
    # initialized by calling init with their names as -flags
    variable dstype file
    variable dsname ext2mime.tie
    variable dsargs {}

    # Add --
    #
    # 	Add a MIME type mapping
    #
    # Arguments:
    #	suffix	A file suffix
    #	type	The corresponding MIME Content-Type.
    #
    # Results:
    #       None

    proc Add {ext type} {
	variable e2m
	set ext [string tolower [string trimleft $ext .]]
	set e2m(.$ext) $type
	lappend e2m($type) .$ext
    }

    proc MimeOf {ext} {
	variable e2m

	if {!$::E2M::initialized} {
	    Init	;# lazy init, to allow user customisation
	}

	set ext ".[string trim [string tolower $ext] .]"
	if {[info exist e2m($ext)]} {
	    return $e2m($ext)
	} else {
	    return $e2m()
	}
    }

    # init the thing
    proc Init {args} {
	#puts stderr "Mime Init"
	variable dstype
	variable dsname
	variable dsargs
	variable prime

	foreach {name val} [::cmdline::getKnownOptions args [subst {
	    {dsname.arg [list $dsname]
		"tie data storage name"
	    }
	    {dstype.arg [list $dstype]
		"tie data storage type"
	    }
	    {dsargs.arg [list $dsargs]
		"tie data storage args"
	    }
	    {prime.arg [list $prime]
		"mime type mapping"
	    }
	}]] {
	    set $name $val
	}

	variable home
	set prime [string map [list ~ $home] $prime]

	variable tie
	variable e2m

	set reread 0
	if {[file exists $dsname] && [file exists $prime]} {
	    set reread [file newer $prime $dsname]
	} elseif {![file exists $dsname]} {
	    set reread 1
	}

	set tie [::tie::tie e2m -open $dstype $dsname {expand}$dsargs]

	if {$reread} {
	    variable default
	    set e2m() $default

	    catch {Read $prime}
	}

	::tie::untie e2m $tie
	set tie [::tie::tie e2m -save $dstype $dsname {expand}$dsargs]

	variable initialized 1	;# so we know we've been initialized
    }

    proc Read {file} {
	package require fileutil
	variable e2m
	puts stderr "Mime Read $file"

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
			Add $ext [lindex $line 0]
		    }
		} else {
		    Add [lindex $line 0] [lindex $line 1]
		}
	    }
	}
    }

    namespace export Add MimeOf Init Read
}

namespace eval Mime {
    namespace import ::E2M::*

    variable mimecache
    variable file_attributes 1
    variable debug 20

    array set mimecache {}
    proc type {file} {
	variable mimecache

	# could be cached
	if {[info exists mimecache($file)]} {
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
	
	# then fall back to file extension to mime type mapping
	if {!$::E2M::initialized} {
	    ::E2M::Init	;# lazy init, to allow user customisation
	}

	set mimecache($file) [E2M::MimeOf [file extension $file]]
	# (note: we don't store e2m mime types in file attributes) Why?

	Debug.mime {MIME ext: $mimecache($file)}
	return $mimecache($file)
    }

    namespace import 
    namespace export -clear *
    namespace ensemble create -subcommands {}
}

set E2M::home [file dirname [info script]]
