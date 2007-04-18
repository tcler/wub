#! /usr/bin/env tclsh
lappend auto_path /usr/lib/

package require Mk4tcl

foreach {name val} {
    base "/tmp/wiki"
    overwrite 0
    profile 0
    prime {}
    globaldocroot 0
    backends 5
    readonly 0
    wikidb wikit.tkd
    upflag ""
    history history
    utf8 0
} {
    set $name $val
}
set opts {}
foreach {name val} $argv {
    dict lappend opts [string trim $name -] $val
}
dict for {name val} $opts {
    set $name $val
}
if {$profile} {
    set profv [package require profiler]
    puts stderr "Profiler: $profv"
}

# env handling - remove the linked env
array set _env [array get ::env]; unset ::env
array set ::env [array get _env]; unset _env

package require snit 2.0

# set up home directory relative to script
set home [file normalize [file dirname [info script]]]

if {[info exists starkit::topdir]} {
    # starkit startup
    set topdir $starkit::topdir
    set drdir [file join $topdir docroot]
} else {
    # unpacked startup
    set topdir [file normalize [file dirname $home]]
    foreach lib {Mime extensions Wub Domains stx Utilities} {
	lappend auto_path [file join $topdir $lib]
    }
    lappend auto_path $home

    if {$globaldocroot} {
	set drdir [file join $topdir docroot]
    } else {
	set drdir [file join $home docroot]
    }
    puts stderr "drdir:$drdir topdir:$topdir home:$home"
}

package require Debug 2.0

# create data and sessionroot dirs
catch {file mkdir [set data [file join $base data]]}
catch {file mkdir [set wikitroot $data]}

# copy the local docroot to $base
set docroot [file join $base docroot]
if {![file exists $docroot]} {
    file copy $drdir [file dirname $docroot]
    file copy [file join $home doc $wikidb] $wikitroot
} elseif {$overwrite} {
    file delete -force $docroot
    file copy -force $drdir [file dirname $docroot]
    file copy -force [file join $home doc $wikidb] $wikitroot
} else {
    puts stderr "Not overwriting existing docroot '$docroot'"
}

# create history directory
if {![info exists ::env(WIKIT_HIST)]} {
    if {$history ne ""} {
	if {[file pathtype $history] ne "absolute"} {
	    set history [file join $data $history]
	}
	set ::env(WIKIT_HIST) $history
	catch {file mkdir $::env(WIKIT_HIST)}
    }
} else {
    catch {file mkdir $::env(WIKIT_HIST)}
}
#puts stderr "History: $::env(WIKIT_HIST)"

# clean up symlinks in docroot
package require functional
package require fileutil

foreach file [::fileutil::find $docroot  [lambda {file} {
    return [expr {[file type [file join [pwd] $file]] eq "link"}]
}]] {
    set dfile [file join [pwd] $file]
    file copy [file join $drdir [K [file link $dfile] [file delete $dfile]]] $dfile
}    

# initialize the mime package
package require Mime
Mime::Init -dsname [file join $data ext2mime.tie]

package require Stdin
package require Listener
package require Httpd 2.0
package require Http

Debug off socket 10
Debug off http 2
Debug off cache 10
Debug off cookies 10
Debug off dispatch 10
Debug off wikit 10

set worker_args [list profile $profile wikidb $wikidb]

if {$profile} {
    ::profiler::init
    ::profiler::suspend

    proc profout {} {
	::profiler::suspend
	set result [::profiler::print]
	::profiler::resume
	return $result
    }
}

if {$profile} {
    ::profiler::resume
}

package require Wikit::Format
namespace import Wikit::Format::*

package require Wikit::Db

Wikit::WikiDatabase [file join $wikitroot $wikidb] wdb 1

if {[mk::view size wdb.pages] == 0} {
    # copy first 10 pages of the default datafile 
    set fd [open [file join $home doc wikidoc.tkd]]
    mk::file load wdb $fd
    close $fd
    mk::view size wdb.pages 10
    mk::view size wdb.archive 0
    Wikit::FixPageRefs
}

package require utf8
if {$utf8} {
    set size [mk::view size wdb.pages]
    set bad 0
    set bogus 0
    set incr 1
    for {set i 0} {$i < $size} {incr i $incr} {
	set incr 1
	foreach f {name page} {
	    set data [mk::get wdb.pages!$i $f]
	    if {$data eq ""} continue
	    set point [utf8::findbad $data]
	    if {$point < [string length $data] - 1} {
		if {$point < 0} {
		    puts stderr "$f $i bogus $point"
		    mk::set wdb.pages!$i $f "bogus [incr bogus]"
		} else {
		    incr bad
		    incr point
		    #utf8::reportTrouble $i $data $point
		    puts stderr "$f $i bad"
		    utf8::fixBadUtf8 $data
		    if {0} {
			set incr -1
			puts stderr "$f $i bad at $point"
			mk::set wdb.pages!$i $f [string replace $data $point $point " badutf "]
		    }
		}
		mk::file commit wdb
	    }
	}
    }
    puts stderr "BAD: $bad / $size"
}

if {$upflag ne ""} {
    Wikit::DoSync $upflag
}

catch {mk::get wdb.pages!9 page}

# make the utf8 regular expression
set utf8re [::utf8::makeUtf8Regexp]

puts stderr "STARTING BACKENDS"
package require Backend
set Backend::incr $backends	;# reduce the backend thread quantum for faster testing
Backend init scriptdir [file dirname [info script]] scriptname WikitWub.tcl docroot $docroot wikitroot $wikitroot dataroot $data utf8re $utf8re {*}$worker_args

# start Listener
set listener [Listener %AUTO% -port 8080 -sockets Httpd -httpd {-dispatch "Backend incoming"}]

set done 0
while {!$done} {
    #puts stderr "Waiting at top level"
    vwait done
}
puts stderr "Shutdown top level"
