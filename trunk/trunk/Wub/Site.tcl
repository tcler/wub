#! /usr/bin/env tclsh
foreach {name val} {
    base "/tmp/wiki"
    overwrite 1
    profile 0
    globaldocroot 1
    backends 20
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

if {[info exists starkit::topdir]} {
    # starkit startup
    set topdir $starkit::topdir
    set drdir [file join $topdir docroot]
} else {
    # unpacked startup

    # set up home directory relative to script
    set home [file normalize [file dirname [info script]]]
    set topdir [file normalize [file dirname $home]]
    foreach lib {Mime extensions Wub Domains stx Utilities tclDb} {
	lappend auto_path [file join $topdir $lib]
    }
    lappend auto_path $home

    if {$globaldocroot} {
	set drdir [file join $topdir docroot]
    } else {
	set drdir [file join $home docroot]
    }
    #puts stderr "drdir:$drdir topdir:$topdir home:$home"
}

package require snit 2.0
package require Debug 2.0

# create data and sessionroot dirs
catch {file mkdir [set data [file join $base data]]}
catch {file mkdir [set sessionroot [file join $data session]]}
catch {file mkdir [set wikiroot [file join $data wiki]]}

# copy the local docroot to $base
set docroot [file join $base docroot]
if {![file exists $docroot]} {
    file copy $drdir [file dirname $docroot]
} elseif {$overwrite} {
    file delete -force $docroot
    file copy $drdir [file dirname $docroot]
} else {
    puts stderr "Not overwriting existing docroot '$docroot'"
}

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

package require File
package require Convert
package require Mason
package require Session

Debug off socket 10
Debug off http 2
Debug off cache 10
Debug off dispatch 10

set worker_args [list profile $profile]

if {$profile} {
    ::profiler::init
    ::profiler::suspend

    proc profout {} {
	::profiler::suspend
	set result [::profiler::print]
	::profiler::resume
	return $result
    }
    ::profiler::resume
}

set sdb [Db ::Session::sdb -file [file join $sessionroot session.db] -shared 1]

puts stderr "STARTING BACKENDS"
package require Backend
set Backend::incr $backends	;# reduce the backend thread quantum for faster testing
Backend init scriptdir [file dirname [info script]] scriptname BackendWorker.tcl docroot $docroot dataroot $data sdb $sdb bfl [thread::mutex create] {*}$worker_args

# start Listener
set listener [Listener %AUTO% -port 8080 -sockets Httpd -httpd {-dispatch "Backend incoming"}]

set done 0
while {!$done} {
    #puts stderr "Waiting at top level"
    vwait done
}
puts stderr "Shutdown top level"
