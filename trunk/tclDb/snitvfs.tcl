# Snit VFS handler

# Usage: vfs $name VFSType ...

# From http://wiki.tcl.tk/11938 by Steve Huntley, who writes:
# My aim for this vfs is to use it as a template for rapid development
# of new and more complex filesystems.
# The Mount, Unmount and handler procedures are completely generic and should
# never need customization for new filesystems.
# Thus the task of creating a new virtual filesystem is reduced to filling in the procedures
# for handling the eight specific subcommands of the Tclvfs API, as well as mounting and 
# unmounting specifics.

package provide snitvfs 1.0

package require snit

#set ::env(VFS_DEBUG) 100
package require vfs 1
#set ::vfs::debug 100

proc Vfs_internalerror {args} {
    puts stderr "INTERNAL ERROR: $::errorInfo"
}

snit::type Vfs {
    component vfs -inherit 1	;# underlying vfs component
    variable stdattr

    option -debug 0

    method log {msg {level 1}} {
	if {$level <= $options(-debug)} {
	    puts stderr [uplevel subst [list $msg]]
	}
    }

    constructor {vfs_type args} {
	if {[catch {
	    # construct the vfs low level object
	    $self log {vfs constructor type:$vfs_type args:$args}
	    install vfs using $vfs_type %AUTO% $self {*}$args
	    $self log {commands: $vfs - [$self cget -mount] - [info procs *]} 3

	    $self configurelist $args

	    # mount this object
	    ::vfs::filesystem mount [$self cget -mount] $self
	    ::vfs::filesystem internalerror ::Vfs_internalerror
	    
	    ::vfs::RegisterMount [$self cget -mount] [mymethod unmount]

	    #set stdattr [vfs::listAttributes]
	    set stdattr $::vfs::attributes(unix)
	} r eo]} {
	    $self log {vfs constructor ERROR: $r ($eo)}
	} else {
	    $self log {vfs constructor DONE}
	}
    }

    destructor {
	set mount [$self cget -mount]
	$vfs destroy
	catch {::vfs::filesystem unmount $mount}
    }

    # called when this is unmounted
    method unmount {args} {
	$self destroy
    }

    method access {root relative actualpath mode} {
	$self log {$self access $root $relative $actualpath $mode}

	set modeString [::vfs::accessMode $mode]
	if {$modeString == "F"} {set modeString RWX}
	set modeString [split $modeString {}]

	if {[catch {$vfs access $root $relative $actualpath $modestring $mode} result]} {
	    vfs::filesystem posixerror $::vfs::posix(EACCES)
	    return -code error $::vfs::posix(EACCES)
	}
	return 1
    }

    # allow perms values to be of the form [ugoa][+-=][wrx]+, for a more chmod feel
    method parsePerms {value} {
	array set m {u 0 g 0 o 0 a 0}
	if {[regexp {^([ugoa]*)([+-=])([wrx]+)(.*)$} $value x who op what rest]} {
	    
	    # calc the perms as an octal, then an integer
	    set what [expr "[string map {r "4 +" w "2 +" x "1 +"} $what] 0"]
	    foreach c [split $who] {
		set m($c) $what
	    }
	    if {$m(a)} {
		set m(u) $m(a)
		set m(g) $m(a)
		set m(o) $m(a)
	    }
	    set value [expr "0$m(u)$m(g)$m(o)" + 0]

	    # work out what we want done with this bitmap
	}
	return [list $op $value]
    }

    method fileattributes {root relative actualpath {index {}} {value {}}} {
	# get complete array of vfs-specific file attributes
	$self log {$self fileattributes $root $relative $actualpath index:$index value:$value}
	if {[catch {
	    array set attributes [$vfs fileattributes $root $relative $actualpath]
	} err eo]} {
	    $self log {$self fileattributes ERR: $err $eo}
	    array set attributes {}
	}
	$self log {$self called fileattributes $index $value} 2

	# the set of all attributes is the standard set plus the fs-specific set
	set myattr [concat $stdattr [lsort [array names attributes]]]

	if {$index == {}} {
	    # what is wanted is the set of all attributes
	    $self log {$self get all file attributes: [lsort [array names attributes]]} 2
	    return $myattr
	}

	# we either want to set or get an attribute, by number

	if {$index < [llength $stdattr]} {
	    # we want a standard attribute
	    set attr [lindex $stdattr $index]

	    # map some standard attributes to something more usable
	    # we only support the unix file attributes
	    switch -- $attr {

		-group {
		    set attribute -gid
		    # interpret group as gid
		}

		-owner {
		    set attribute -uid
		    # interpret owner as uid
		}

		-permissions {
		    set attribute -mode

		    # parse permissions - why can they even *be* this wild?
		    if {$value != {}} {
			foreach {op value} [$self parsePerms $value] break;
			switch -- $op {
			    + -
			    - {
				# add or remove perms
				return [$vfs permissions $root $relative $actualpath $op $value]
			    }
			    = {
				# set perms - fall through
			    }
			    default {
			    }
			}
		    }
		}

		-vfs {
		    set attribute -vfs
		    # just here to allow default to work
		}

		default {
		    $self log "Unimplemented attribute $attr"
		    error "$attr not implemented"
		}
	    }
	} else {
	    # we want a vfs-specific attribute
	    set attribute [lindex [lsort [array names attributes]] $index]
	}

	if {$value == {}} {
	    # return the attribute value
	    $self log {$self get file attribute $index done - $attributes($attribute)}
	    return $attributes($attribute)
	}

	$self log {$self calling setattribute $attribute $value} 2
	$vfs setattribute $root $relative $actualpath $attribute $value
	$self log {$self setattribute $attribute $value}
    }

    method open {root relative actualpath {mode r} {permissions {}}} {
	$self log {$self open $root $relative $actualpath mode:$mode perms:0[format "%o" $permissions]}

	# call underlying open method, expecting a list whose first element is the fd.
	# the entire returned value will be added to the $self close method
	set result [$vfs open $root $relative $actualpath $mode $permissions]

	set channelID [lindex $result 0]
	switch -glob -- $mode {
	    "" -
	    "r*" -
	    "w*" {
		catch {seek $channelID 0}
	    }
	    "a*" {
		catch {seek $channelID 0 end}
	    }
	    default {
		::vfs::filesystem posixerror $::vfs::posix(EINVAL)
		return -code error $::vfs::posix(EINVAL)
	    }
	}

	set callback [list $channelID [list $self close $root $relative $actualpath {*}$result]]
	$self log {$self open DONE: callback $callback}
	return $callback
    }

    method removedirectory {root relative actualpath recursive} {
	$self log {$self removedirectory $root $relative $actualpath $recursive}
	if {!$recursive} {
	    if {[$vfs matchindirectory $root $relative $actualpath * 0] != {}} {
		::vfs::filesystem posixerror $::vfs::posix(EEXIST)
		return -code error $::vfs::posix(EEXIST)
	    }
	}
	$vfs removedirectory $root $relative $actualpath
    }
}
