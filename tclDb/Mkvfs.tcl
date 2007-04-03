# mkvfs.tcl -- Mk4tcl Virtual File System driver
#
# 20051207 CMcC use Db.tcl interface
# 20040722 CMcC caching of _stat.  User handling.

if {[info exists argv0] && ($argv0 eq [info script])} {
    # Unit Test
    #source ./snitvfs.tcl
    lappend auto_path [pwd]
}

package provide Mkvfs 2.0

package require Tcl 8.5
package require Mk4tcl
package require snitvfs
package require Db

snit::type Mkvfs {

    # metakit interface variables
    option -db ""
    option -file ""

    component db -public db	;# mk4 database
    component vfs -inherit yes -public vfs

    # Cache is a mapping path->(inode,attributes)
    #
    # we have two levels of caching: partial just contains the inode,
    # full contains all attributes.  We can distinguish by the existence
    # of the vfs element.
    #
    # Cache entries are in array get form, so we can lappend changes
    # and expect them to be turned into an array in the correct form
    #
    # Cache is maintained between -low and -high in size, with elements
    # discarded in least recently used order.

    variable cache	;# cache of directories
    option -high 50	;# cache high water mark
    option -low 30	;# cache low water mark

    option -user ""		;# user object
    option -mount ""		;# directory mounted over
    option -perms 755	;# directory creation permissions
    option -noperms 0	;# don't worry about permissions

    # Layout of directory entry
    # - you can specify this option with additional fields
    option -layout ""

    # content compression and delivery  flags
    option -direct 0		;# read through a memchan, or from Mk4tcl if zero
    option -compress 0	;# compress
    option -zstreamed	 0	;# decompress on the fly

    component snitvfs;	# mkvfs and snitvfs are components of one another
    delegate method log to snitvfs;	# pass log back to 'parent'

    constructor {_snitvfs args} {
	puts stderr "making Mkvfs from $_snitvfs $args"
	catch {
	    set snitvfs $_snitvfs
	    set vfs return
	    $self configurelist $args

	    # create/open fs db
	    if {$options(-db) ne ""} {
		install db using Db $options(-db) -file $options(-file)
	    } else {
		install db using Db %AUTO% -file $options(-file)
	    }

	    # create a null user object if necessary
	    if {$options(-user) eq ""} {
		package require nulluser
		set options(-user) [nulluser %AUTO%]
	    }

	    # construct the metakit fs view
	    $db layout vfs [concat {
		parent:I	# parent inode
		name:S	# file name
		mtime:I	# last modified
		ctime:I	# created time
		size:I	# size of raw file
		csize:I	# compressed size
		mode:I	# file mode
		nlink:I	# number of links to file
		uid:I gid:I	# owner, group
		contents:B # file contents
	    } $options(-layout)]
	    puts stderr [$db views]
	    set vfs $db.vfs

	    if { [$self size] == 0 } {
		# create an empty directory as root
		set cur [$self insert end name "" parent -1 \
			     mtime [clock seconds] ctime [clock seconds] \
			     uid [$options(-user) uid] gid [$options(-user) gid] \
			     mode $options(-perms) \
			     size 0 csize 0 \
			     nlink 2 \
			     contents ""]
	    }
	} r eo
	puts stderr "$self - $r ($eo)"
    }

    destructor {
	# destroy any nulluser obj
	if {[$options(-user) info type] eq "nulluser"} {
	    $options(-user) destroy
	}

	# close views
	$vfs destroy
	catch {$db commit}
	$db destroy
    }

    # return a list of indices of records matching the select
    method select {args} {
	# run a selection over the both view
	set selector [$self vfs select {*}$args]
	set result {}

	# turn the selection result into a list of indices
	$selector loop cursor {
			       lappend result [$selector get $cursor index]
			   }
	$selector close

	return $result
    }

    # keyword in context match
    method keyword {keyword {fields contents}} {
	if {$options(-compress)} {
	    error "$self doesn't support keywords.  Create with -compress 0"
	}
	return [$self select -keyword {*}$fields $keyword]
    }

    # map path to row number
    method acquire {path} {
	$self log {$self acquire $path}

	# look for match in cache
	if {[info exists cache($path)]} {
	    array set sb $cache($path)
	    #set cache($path) [array get sb]
	    $self log {$self acquire $path cache hit: [array get sb]} 3
	    return $sb(ino)
	}

	# search directories for path
	set sp [::file split $path]
	set row 0
	foreach el $sp {
	    if {$el eq "."} {
		continue
	    }

	    $self log {acquire search for $el in $row} 5
	    if {[catch {
		$self find parent $row name $el
	    } row eo]} {
		$self log {$self acquire -> $path doesn't exist on $el in $row}
		vfs::filesystem posixerror $::vfs::posix(ENOENT)
	    }

	    $self log {$self acquire got $row for $el} 3
	}
	$self log {acquire -> $row} 3

	# create a partial cache entry containing the inode
	set cache($path) [list ino $row lru [clock seconds]]
	return $row
    }

    # check that this fs is writable
    method writable {} {
	if {[$db cget -readonly]} {
	    vfs::filesystem posixerror $::vfs::posix(EROFS)
	}
    }

    # set the modification time of a file - we don't support atime
    method utime {root path actual atime mtime} {
	$self log {$self utime $root $path $actual $atime $mtime}
	$self writable

	if {[catch {
	    set row [$self acquire $path]
	    $self set $row mtime $mtime

	    lappend cache($path) mtime $mtime lru [clock seconds]
	} err eo]} {
	    $self log {utime ERR: $err - $eo}
	    $self log {$self dump} 3
	}
	$self log {$self utime DONE}
    }

    method getdir {path {pat *}} {
	$self log {$self getdir '$path' '$pat'}

	set parent [$self acquire $path]		;# get parent's inode

	# Match directory entries
	set rows [::mk::select $vfs parent $parent -glob name $pat]
	$self log {$self getdir rows $parent - $pat - $rows} 2

	# construct set of names from set of matching inodes
	foreach row $rows {
	    $self log {$self getdir hit $row '[$db.vfs get $row name]'} 3
	    set hits([$self get $row name]) 1
	}
	$self log {$self getdir -> [array names hits]}

	return [lsort [array names hits]]	;# return set of names
    }

    # Interface: check that a file can be accessed in the given mode
    method access {root name actual modeS mode} {
	$self log {$self access $root $name $actual $modeS $mode}

	$self _stat $name sb
	if {$sb(mode) & $mode} {
	    vfs::filesystem posixerror $::vfs::posix(EACCESS)
	}
    }

    # get files of the given file type $ftype matching glob $pattern
    method matchindirectory {root path actual pattern ftype} {
	$self log {$self matchindirectory root:$root path:'$path' actual:'$actual' pattern:'$pattern' ftype:$ftype}
	set newres {}
	if {![string length $pattern]} {
	    # check single file
	    if {[catch {$self acquire $path} err eo]} {
		$self log {$self matchindirectory ERR1: $err - $eo}
		$self log {$self dump} 3
		return {}
	    }
	    $self log {$self matchindirectory 1} 5
	    set res [list $actual]
	    set actual ""
	} else {
	    $self log {$self matchindirectory 2} 5
	    if {[catch {
		set res [$self getdir $path $pattern]
	    } result eo]} {
		$self log {$self matchindirectory getdir err $eo} 1
		return -options $eo $result
	    } else {
		$self log {$self matchindirectory getdir ok $result $eo} 2
	    }
	    $self log {$self matchindirectory 2.5} 5
	}

	$self log {$self matchindirectory 3} 5

	if {[catch {
	    foreach p [::vfs::matchCorrectTypes $ftype $res $actual] {
		lappend newres [file join $actual $p]
	    }
	} err eo]} {
	    $self log {$self matchindirectory ERR: $err - $eo}
	    $self log {$self dump} 3
	    return -options $eo $err
	}
	$self log {$self matchindirectory result: $newres}

	return $newres
    }

    # sort cache into least recently used order
    # after Richard Suchenwirth's lsortby from http://wiki.tcl.tk/4021
    method cachesort {} {
	$self log {cachesort} 3
	set t {}
	foreach {n} [array names cache] {
	    array set sb $cache($n)
	    # if {[info exists sb(vfs)]} would only select full cache entries
	    # as deletion candidates.  Bad idea - it'll fill the cache so
	    # we'd incur lots of ineffective cachesorts
	    lappend t [list $sb(lru) $n]
	    set cache($n) [array get sb]	;# compact cache entry
	    array unset sb
	}

	set res {}
	foreach i [lsort $args -integer -index 0 $t] {
	    lappend res [lindex $i 1]
	}

	return $res
    }

    # fill cache with the path's attributes
    method _stat {path {arr ""}} {
	upvar 1 $arr sb
	if {[info exists sb]} {
	    unset sb	;# clean the stat buffer
	}

	if {[info exists cache($path)]} {

	    # fetch cache, update last used time
	    array set sb $cache($path)
	    set sb(lru) [clock seconds]

	    if {[info exists sb(vfs)]} {
		# return a full cache entry
		set cache($path) [array get sb]	;# compact cache
		$self log {$self _stat $path cache hit: [array get sb]} 4
		return
	    }
	    # full cache entry hasn't been made yet - drop through to fill it
	}

	# check cache high water mark
	if {[array size cache] > $options(-high)} {
	    # invalidate cache entries until we're left with -low entries
	    # the candidates are chosen in LRU order
	    set loss [expr {[array size cache] - $options(-low)}]

	    foreach item [lrange [$self cachesort] 0 $loss] {
		array set cent $cache($item)	;# don't lose open files' cache
		if {![info exists cent(open)]} {
		    unset cache($item)
		}
		unset cent
	    }
	}

	set row [$self acquire $path]

	$self log {$self _stat $path - '$row'} 3
	array set sb {
	    dev 0
	    ino -98
	    mode 0777
	    nlink 1
	    uid 0
	    gid 0
	    size 0
	    atime 0
	    ctime 0
	    mtime 0
	    vfs 1
	}

	$self log {$self _stat $path - '[$self get $row]'} 4
	array set sb [$self get $row]

	# the type of a file is actually quite arbitrary
	# - in this implementation directories can store data
	# and files can 'contain' other files and directories
	# we (arbitrarily) say that anything with more than one link
	# is a directory.  It's a kludge.
	set sb(type) [expr {($sb(nlink) > 1) ? "directory" : "file"}]

	set sb(ino) $row	;# inode is row within views
	set sb(lru) [clock seconds]	;# lru orders the cache

	set cache($path) [array get sb]	;# full cache entry

	$self log {$self _stat $path DONE - [array get sb]} 3
    }

    # return the subset of file attributes tcl vfs cares about
    method stat {root relative actual} {
	$self log {$self stat $root $relative $actual}
	$self _stat $relative sb

	# subset the relevant fields
	foreach name {atime ctime dev gid ino mode mtime nlink size type uid} {
	    if {[info exists sb($name)]} {
		set stat($name) $sb($name)
	    }
	}

	$self log {$self stat DONE: [array get stat]}
	return [array get stat]
    }

    # close an open file
    method close {root relative actual fd inode mode} {
	$self log {$self close $root $relative $actual}

	# closing a read file is a noop
	if {![string match {[wa]*} $mode]} {
	    $self log {$self close (read) DONE}
	    return
	}

	# file was opened in append or write mode
	if {[catch {
	    flush $fd

	    if { [string match *z* $mode] } {
		# the file need to be compressed
		fconfigure $fd -translation binary
		seek $fd 0; set data [read $fd]	;# read the data
		set cdata [vfs::zip -mode compress -- $data] ;# compress it

		# calculate the compression saving
		set len [string length $data]
		set clen [string length $cdata]

		if { $clen < $len } {
		    # save the compressed data
		    $self set $inode size $len csize $clen mtime [clock seconds] contents $cdata
		} else {
		    # compression is ineffective, save the uncompressed
		    $self set $inode size $len csize $len mtime [clock seconds] contents $data
		}
	    } else {
		# adjust the new file size dentry
		set len [string length [$self get $inode contents]]
		$self set $inode size $len csize $len mtime [clock seconds]
	    }

	    ;# invalidate cache, because attributes have now changed
	    set cache($relative) [list ino $inode]

	} err eo]} {
	    $self log {$self close ERR: $err $eo}
	    $self log {$self dump} 3
	    return -options $eo $err
	} else {
	    $self log {$self close DONE [$self dump]}
	}
    }

    # compare the perms requested with those available
    method chperms {what uid gid fuid fgid mode} {
	# construct a mask in octal from the requested perms
	set what [expr "[string map {r "4 +" w "2 +" x "1 +"} $what] 0"]
	set mask [expr 0${what}00]

	$self log {$self chperms what:$what uid:$uid gid:$gid fuid:$fuid fgid:$fgid mode:[format %o $mode]}

	if {($uid == $fuid) && (($mask & $mode) == $mask)} {
	    return	1;# user perms match
	}

	set mask [expr {$mask >> 3}]
	if {(($gid == $fgid) || [$user ingroup $fgid $uid]) && (($mask & $mode) == $mask)} {
	    return	1;# group perms match
	}

	set mask [expr {$mask >> 3}]
	if {($mask & $mode) == $mask} {
	    return	1;# other perms match
	}

	# no permission
	$self log {$self chperms FAIL: mode:[format %o $mode]}
	vfs::filesystem posixerror $::vfs::posix(EPERM)
    }

    method checkperms {what path} {
	if {$options(-noperms)} {
	    return 1;# don't check perms
	}

	# get current uid and gid
	set uid [$options(-user) uid]
	set gid [$options(-user) gid]

	# get path's mode
	$self _stat $path sb
	if {[catch {$self chperms $what $uid $gid $sb(uid) $sb(gid) $sb(mode)} err eo]} {
	    $self log {checkperms $what on $path Failed: $err - $eo} 2
	    error "Failed"
	} else {
	    return 1
	}
    }

    # create a directory entry for $path
    method createDirent {path perms {nlink 1}} {
	$self log {$self createDirent $path $perms $nlink}

	# get parent name
	set dirname [file dirname $path]

	# check write perms on parent
	$self checkperms w $dirname

	if {[catch {
	    # update parent dirent and cache
	    $self _stat $dirname parent
	    $self set $parent(ino) nlink [incr parent(nlink)]
	    lappend cache($dirname) nlink $parent(nlink) lru [clock seconds]

	    set tail [file tail $path]
	    set now [clock seconds]

	    # create cache entry for new directory
	    set uid [$options(-user) uid]
	    set gid [$options(-user) gid]
	    set cache($path) [list parent $parent(ino) \
				  name $tail size 0 csize 0 \
				  ctime $now mtime $now \
				  uid $uid gid $gid \
				  nlink $nlink mode $perms]

	    # add the cached attributes to the dir view
	    $self log {appending: $vfs $cache($path)} 2
	    set cur [$self insert end {*}$cache($path) contents ""]

	    # add some cache control entities
	    lappend cache($path) vfs 1 ino $cur lru [clock seconds] \
		type [expr {$nlink > 1 ? "directory" : "file"}]
	} err eo]} {
	    $self log {$self createDirent ERR: $err - $eo}
	    $self log {$self dump} 3
	    return -options $eo $err
	}
	$self log {$self createDirent DONE: $cur}
	return $cur	;# return new inode
    }

    # open the path
    # we enforce single-writer per file
    #
    # return a list of two elements:
    # 1. first element is the Tcl channel name which has been opened
    # 2. second element (optional) is a command to evaluate when the channel is closed.
    method open {root path actual mode perms} {
	$self log {$self open $root $path $actual mode:$mode perms:[format "%o" $perms]}
	switch -glob -- $mode {
	    {}  -
	    r {
		set mode r
		$self _stat $path sb		;# fetch the path's cache
		$self checkperms r $path	;# check read perms on file

		if { $sb(csize) != $sb(size) } {
		    # file is compressed in store, need to uncompress
		    if {$options(-zstreamed)} {
			# stream decompression
			set fd [$self channel $sb(ino) contents r]
			fconfigure $fd -translation binary
			set fd [vfs::zstream decompress $fd $sb(csize) $sb(size)]
		    } else {
			# decompress file into a memchan
			set fd [vfs::memchan]
			fconfigure $fd -translation binary
			set s [$self get $sb(ino) contents]
			puts -nonewline $fd [vfs::zip -mode decompress -- $s]
			fconfigure $fd -translation auto
			seek $fd 0
		    }
		} elseif {$options(-direct)} {
		    # read file indirectly via a memchan
		    set fd [vfs::memchan]
		    fconfigure $fd -translation binary
		    puts -nonewline $fd [$self get $sb(ino) contents]
		    fconfigure $fd -translation auto
		    seek $fd 0
		} else {
		    # read file directly from the metakit
		    set fd [$self channel $sb(ino) contents r]
		}
	    }

	    a {
		$self writable	;# fs must be writable

		if { [catch {$self _stat $path sb}] } {
		    # file doesn't exist.

		    # ensure we have write perms on parent
		    $self checkperms w [file dirname $path]

		    # Create file entry
		    set sb(ino) [$self createDirent $path $perms]

		    # adjust mode if compression is requested
		    if { [string match *z* $mode] && $options(-compress) } {
			set sb(csize) -1  ;# HACK - force compression
		    } else {
			set sb(csize) 0
		    }

		} else {
		    # check write perms on file
		    $self checkperms w $path
		    if {[info exists sb(open)]} {
			# this file is already open for writing
			error "$self $path already open for writing"
		    }
		}

		# we have a writable directory entry
		set fd [vfs::memchan]
		fconfigure $fd -translation binary
		set s [$self get $sb(ino) contents]

		if { ($sb(csize) != $sb(size)) && ($sb(csize) > 0) } {
		    # the existing file was written compressed
		    append mode z

		    # uncompress the file to memchan
		    puts -nonewline $fd [vfs::zip -mode decompress -- $s]
		} else {
		    ;# write the file to memchan
		    puts -nonewline $fd $s
		    lappend cache($path) csize $sb(csize)
		}
		fconfigure $fd -translation auto
		seek $fd 0 end

		lappend cache($path) open $fd ;# remember the file's open
	    }

	    w*  {
		$self writable	;# the fs must be writable

		# check write perms on parent
		$self checkperms w [file dirname $path]

		if { [catch {$self _stat $path sb}] } {
		    # Create new file
		    set sb(ino) [$self createDirent $path $perms]
		} elseif {[info exists sb(open)]} {
		    # this file is already open for writing
		    error "$self $path already open for writing"
		}

		if { [string match *z* $mode] && $options(-compress) } {
		    set fd [vfs::memchan]
		} else {
		    # write directly to the metakit
		    set fd [$self channel $sb(ino) contents w]
		}

		lappend cache($path) open $fd	;# remember the file's open
	    }

	    default   {
		error "illegal access mode \"$mode\""
	    }
	}

	return [list $fd $sb(ino) $mode]
    }

    # create a directory at path
    method createdirectory {root path actual} {
	$self log {$self createdirectory root:$root path:$path actual:$actual}

	if {$path eq ""} {
	    $self log {$self createdirectory root always exists}
	    return	"" ;# root node already always exists
	}

	if {[catch {
	    $self writable
	    $self createDirent $path $options(-perms) 2
	} err eo]} {
	    $self log {$self createdirectory ERR: $err - $eo}
	    $self log {$self dump} 3
	    return -code error "$self couldn't createdirectory $path"
	} else {
	    $self log {$self createdirectory DONE}
	}

	return ""
    }

    method delete {path {recursive 0}} {
	$self log {$self delete $path $recursive}
	$self writable	;# fs must be writable
	$self _stat $path sb	;# get path's cache entry

	if {$sb(type) eq "directory" } {
	    # just mark dirents as deleted
	    set contents [$self getdir $path *]

	    if {$recursive} {
		# We have to delete these manually, else
		# they (or their cache) may conflict with
		# something later
		foreach f $contents {
		    $self delete [file join $path $f] $recursive
		}
	    } else {
		if {[llength $contents]} {
		    vfs::filesystem posixerror $::vfs::posix(ENOTEMPTY)
		}
	    }
	}

	# reduce parent's nlink by one
	$self set $sb(parent) nlink [expr {[$self get $sb(parent) nlink] - 1}]

	# flag with -99, because parent -1 is not reserved for the root dir
	# deleted entries never get re-used, should be cleaned up one day
	$self set $sb(ino) parent -99 name "" size 0 csize 0 mtime [clock seconds] contents ""

	unset cache($path)	;# no path, so no cache

	return ""
    }

    method removedirectory {root name actual recursive} {
	$self log {$self removedirectory $root $name $actual $recursive}
	$self delete $name $recursive
    }

    method deletefile {root name actual} {
	$self log {$self deletefile $root $name $actual}
	$self delete $name
    }

    method copyfile {root src actual dest} {
	if {[catch {
	    set dest [string map [list $root .] [file normalize $dest]]
	    $self log {$self copyfile $src $dest}
	    $self _stat $src sb

	    if {[catch {$self acquire $dest}]} {
		# dest doesn't exist
		$self createDirent $dest $sb(mode)	;# create a new entry
		$self _stat $dest destb		;# get entry's attributes
		set attrs {}
	    } else {
		# dest exists
		$self writable	;# fs must be writable
		$self _stat $dest destb	;# get attributes
		set now [clock seconds]	;# remember now
		set attrs [list ctime $now mtime $sb(mtime)]
	    }

	    # copy attributes & content
	    $self set $destb(ino) {*}$attrs name [file tail $dest] \
		uid $sb(uid) gid $sb(gid) \
		mode $sb(mode) \
		size $sb(size) csize $sb(size) \
		contents [$self get $sb(ino) contents]
	} err eo]} {
	    $self log {$self copyfile FAIL: $eo}
	    return -options $eo $err
	} else {
	    $self log {$self copyfile $src $dest DONE}
	}
    }

    method renamefile {root src actual dest} {
	if {[catch {
	    set dest [string map [list $root .] [file normalize $dest]]
	    $self log {$self renamefile $src $dest}
	    $self _stat $src sb

	    set parent [file dirname $dest]
	    $self _stat $parent pb

	    if {![catch {$self acquire $dest}]} {
		# dest exists
		$self log {$self renamefile: $dest exists}
		$self delete $dest
	    } else {
		# dest doesn't exist
		$self log {$self renamefile: $dest doesn't exist}
		$self checkperms w $parent
	    }

	    # adjust parent links
	    $self log {$self renamefile: moving out of parent $pb(ino) / $pb(nlink)} 3

	    $self set $pb(ino) nlink [incr pb(nlink)]
	    $self log {$self renamefile: moving into parent $pb(ino)} 3
	    $self set $sb(parent) nlink [expr {[$self get $sb(parent) nlink] - 1}]

	    # now the dirent is in new parent
	    $self set $sb(ino) parent $pb(ino) name [file tail $dest]

	    # remove the old cache
	    catch {unset cache($src)}

	    $self _stat $dest

	} err eo]} {
	    $self log {$self renamefile FAIL: $eo}
	    return -options $eo $err
	} else {
	    $self log {$self renamefile $src $dest DONE}
	}
	#puts stderr "DUMP post renamefile: [$self dump]"
    }

    method fileattributes {root relative actual} {
	$self log {$self fileattributes $root $relative $actual}

	$self _stat $relative sb
	foreach name [lsort [array names sb]] {
	    lappend result "-$name" $sb($name)
	}

	$self log {$self fileattributes -> $result}

	return $result
    }

    # Interface set attribute
    method setattribute {root relative actual attribute value} {
	$self log {$self setattribute $root $relative $actual $attribute $value}
	$self writable
	set row [$self acquire $relative]
	set attr [string trimleft $attribute -]
	set err "specified vfs read only attribute"; set eo ""
	if {($attr eq "vfs") || [catch {$self set $row $attr $value} err eo]} {
	    $self log {$self setattribute ERR: $err - $eo}
	    $self log {$self dump} 3
	} else {
	    lappend cache($relative) $attr $value
	}
    }

    # return the full name of a given inode
    method inode2name {inode} {
	set result {}
	if {$inode == 0} {
	    return /
	}
	while {$inode > 0} {
	    lassign [$self get $inode name parent] name inode
	    if {$inode == -99} {
		return "<DELETED>"
	    }
	    set result [linsert $result 0 $name]
	}

	return [file join {*}$result]
    }

    method permissions {root relative actual operation value} {
	$self log {$self permissions $root $relative $actual $operation $value}
	$self writable
	$self _stat $relative sb

	# perform the operation
	switch -- $operation {
	    + {
		set value [expr {$value | $sb(mode)}]
	    }
	    - {
		set value [expr {!$value & $sb(mode)}]
	    }
	    default {
		error "bogus operation in permissions"
	    }
	}

	# save the change
	if {[catch {$self set $sb(ino) mode $value} err eo]} {
	    $self log {$self permissions ERR: permissions $err - $eo}
	    $self log {$self dump} 3
	    return -options $eo $err
	} else {
	    # update the cache
	    lappend cache($relative) mode $value
	    $self log {$self permissions DONE set mode to [format %o $value]}
	}
    }

    # TreeCL Structural generators
    method rootname {} {
	return 0	;# 0 is always root's inode
    }

    # return node's parent
    method parent {node} {
	return [$self get $node parent]
    }

    # return all immediate children of node
    method children {node} {
	$self select -exact parent $node
    }

    # return next right sibling of node
    method next {node} {
	set kids [$self children [$self parent $node]]
	set index [lsearch -exact -integer $kids $node]
	incr index
	return [lindex $kids $index]
    }

    # return left sibling of node
    method previous {node} {
	set kids [$self children [$self parent $node]]
	set index [lsearch -exact -integer $kids $node]
	incr index -1
	return [lindex $kids $index]
    }

    # TreeCL Property generators

    # get all node property (names,values) whose keys match glob (default *)
    method keys {node {glob *}} {
	array attr set [$self get $node]
	unset attr(contents)
	return [array get attr $glob]
    }

    # TreeCL Predicates

    # node has property key
    method keyexists {node key} {
	set found [lsearch [$self properties] ${key}:*]
	return [expr {$found != -1}]
    }

    # return a serialised view of the metakit
    method dump {{view vfs}} {
	set result ""
	set i 0
	for {set i 0} {$i < [[set $view] size]} {incr i} {
	    append result "[$self inode2name $i]: ino $i [$db.$view get $i]\n"
	}

	return $result
    }
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    # Unit Test

    #file mkdir ./testvfs
    set x [Vfs %AUTO% Mkvfs -debug 10 -file Mkvfs.db -mount ./testvfs -direct 1]

    puts stderr "./testvfs - [vfs::filesystem info ./testvfs]"
    puts stderr [glob -nocomplain ./testvfs]
    catch {file mkdir ./testvfs/testd}
    puts stderr [$x dump]

    set fd [open ./testvfs/testf w]
    puts $fd "Now is the winter of our discontent"
    puts $fd "moop [clock seconds]"
    close $fd
    puts stderr [$x dump]

    puts stderr "glob ./testvfs/*: [glob -nocomplain ./testvfs/*]"

    puts stderr ">>>Read test"
    set fd [open ./testvfs/testf]
    puts stderr [gets $fd]
    close $fd
    puts stderr [$x dump]

    puts stderr ">>>Stat files"
    file stat ./testvfs/testf stat
    puts stderr "stat: [array get stat]"
    file stat ./testvfs/testd stat
    puts stderr "stat: [array get stat]"

    puts stderr ">>>Copy Test: copy ./testvfs/testf ./testvfs/testf1"
    file copy -force ./testvfs/testf ./testvfs/testf1

    puts stderr ">>>Rename Test"
    file rename -force ./testvfs/testf ./testvfs/testf2
    puts stderr "glob ./testvfs/*: [glob -nocomplain ./testvfs/*]"

    puts stderr ">>>Copy Test2: copy ./testvfs/testf2 ./testvfs/testd/"
    file copy -force ./testvfs/testf2 ./testvfs/testd/
    puts stderr "glob ./testvfs/testd/*: [glob -nocomplain ./testvfs/testd/*]"

    #file link -symbolic ./testvfs/link1 ./testvfs/testf
    #file link -hard ./testvfs/link2 ./testvfs/testf
    #puts stderr "glob ./testvfs/*: [glob -nocomplain ./testvfs/*]"

    puts stderr ">>>TreeCL"
    puts stderr "Keyexists: [$x keyexists 0 parent]"
    puts stderr "Childen: [$x children 0]"
    puts stderr "Next: [$x next [lindex [$x children 0] 0]]"
    puts stderr "Prev: [$x previous [lindex [$x children 0] 0]]"

    puts stderr ">>>Dump"
    puts stderr [$x dump]

    puts [$x keyword winter]

    $x destroy
}
