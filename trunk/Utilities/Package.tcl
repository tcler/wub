package ifneeded Package 1.0 {
    package require sqlite3
    package require tdbc::sqlite3

    package provide Package 1.0

    namespace eval ::tcl::package {
	# calculate old-school package subcommands
	catch {package moop} e eo
	set e [split [lindex [split [dict get $eo -errorinfo] \n] 0] ,]
	set e [lreplace $e 0 0 [lindex [split [lindex $e 0]] end]]
	set e [lreplace $e end end [lindex [split [lindex $e end]] end]]
	variable orgsubs {}
	foreach eo $e {
	    lappend orgsubs [string trim $eo]
	}
	unset e; unset eo

	# open DB
	tdbc::sqlite3::connection create pdb ~/.tclpkg
	variable live [catch {
	    [pdb prepare {
		CREATE TABLE package (package TEXT NOT NULL,
				      version TEXT NOT NULL,
				      script TEXT NOT NULL,
				      dir TEXT
				      );
	    }] execute
	    [pdb prepare {
		CREATE INDEX pindex ON package (package,version);
	    }] execute
	    if {0} {
		[pdb prepare {
		    CREATE TABLE path (path TEXT NOT NULL,
				       date INT NOT NULL);
		}]
		[pdb prepare {
		    CREATE INDEX pathindex ON path (path);
		}]
	    }
	}]

	# construct DB statements
	foreach {name stmt} {
	    del {DELETE from package WHERE package = :package AND version = :version}
	    replace {REPLACE INTO package (package, version, script, dir) VALUES (:package, :version, :script, :dir)}
	    version {SELECT * FROM package WHERE package = :package AND version = :version}
	    find {SELECT * FROM package WHERE package = :package}
	    findD {SELECT * FROM package WHERE package = :package ORDER BY version DESC}
	} {
	    set statement($name) [pdb prepare $stmt]
	}

	# create our own [package unknown] command
	variable oldunknown [package unknown]
	proc unknown {args} {
	    puts stderr "UNKNOWN: $args"
	    variable oldunknown
	    return [{*}$oldunknown {*}$args]
	}
	package unknown [namespace code unknown]

	# create a wrapper around each existing package subcommand
	foreach n $orgsubs {
	    if {"::tcl::package::$n" ni [info commands ::tcl::package::*]} {
		{*}[string map [list %N% $n] {
		    proc %N% {args} {
			set result [uplevel [list [namespace current]::_package %N% {*}$args]]
			puts stderr "called package %N% $args -> $result"
			return $result
		    }
		}]
	    }
	}

	# forward unimplemented subcommands
	proc _unknown {cmd subcmd args} {
	    variable orgsubs
	    if {$subcmd in $orgsubs} {
		set result [list [namespace current]::_package $subcmd {*}$args]
		puts stderr "$cmd $subcmd $args -> $result"
		return $result
	    } else {
		error "bad option $subcmd: must be [join $orgsubs ,]"
	    }
	}

	# install the contents of this ns as an ensemble over ::package
	rename ::package ::tcl::package::_package

	# create ::package as an ensemble
	namespace export -clear *
	namespace ensemble create -command ::package -subcommands {} -unknown ::tcl::package::_unknown

	# prime the db
	proc prime {} {
	    puts stderr "Priming"
	    # collect and store in db each ifneeded script
	    proc ifneeded {package version script} {
		variable statement
		upvar 1 dir dir
		$statement(replace) execute
		puts stderr "Priming ifneeded $package $version $script"
	    }

	    # collect and store any additionally provided packages
	    variable statement
	    foreach package [_package names] {
		set script ""
		set versions [_package versions $package]
		if {[llength $versions]} {
		    # here's a package with multiple versions
		    foreach version $versions {
			set script [_package ifneeded $package $version]
			puts stderr "PRELOAD: $package $version $script"
			$statement(replace) execute
		    }
		} else {
		    # this package has no versions, must be builtin
		    puts stderr "BUILTIN $package"
		    set script ""
		    set version [_package present $package]
		    $statement(replace) execute
		}
	    }

	    # force traversal of the whole ::auto_path, collecting ifneeded data
	    catch {package require __MOOOP____}

	    # replace the collector ifneeded script with the real one
	    proc ifneeded {package version {script ""}} {
		if {$script eq ""} {
		    set d [$statement(version) -as dict]
		    if {[dict size $d]} {
			puts stderr "Package ifneeded $package $version -> [dict get $d script]"
			return [dict get $d script]
		    } else {
			puts stderr "Package ifneeded $package $version -> UNREGISTERED"
			return ""
		    }
		} else {
		    $statement(replace) execute
		    puts stderr "Package ifneeded $package $version -> [dict get $d script]"
		    return ""
		}
	    }
	}

	if {!$live} {
	    prime
	}

	# what versions do we know about?
	proc versions {package} {
	    variable statement
	    set v {}
	    $statement(find) foreach -as dicts d {
		lappend v [dict get $d version]
	    }
	    puts stderr "Package versions $package -> $v"
	    return $v
	}

	proc require {args} {
	    # parse args
	    if {[string match -exact [lindex $args 0]]} {
		set exact 1
		set package [lindex $args end-1]
		set version [lindex $args end]

		puts stderr "Package require -exact $package '$version'"
		if {![catch {_package present $package} present]} {
		    return $present
		}

		set match [$statement(version) allrows -as dicts]
		if {[llength $match]} {
		    uplevel #0 [dict get [lindex $match 0] script]
		    return $version
		} else {
		    return ""
		}
	    }

	    set package [lindex $args 0]
	    if {[llength $args] > 1} {
		set version [lindex $args end]
	    }

	    variable statement
	    if {[info exists version]} {
		# run query over all matches, check requirement
		set ds [$statement(findD) allrows -as dicts]
		if {![llength $ds]} {
		    return ""
		}
		foreach d $ds {
		    if {[_package vsatisfies [dict get $d version] $version]} {
			puts stderr "$package,$version is vsatisfied by ($d)"
			break
		    } else {
			puts stderr "($d) does not satisfy $package,$version"
		    }
		}
	    } elseif {![catch {_package present $package} present]} {
		return $present
	    } else {
		# no version, get highest available
		set d [$statement(findD) allrows -as dicts]
		if {![llength $d]} {
		    error "no package $package"
		    return ""
		} else {
		    set d [lindex $d 0]
		    puts stderr "no version of $package specified, found singleton ($d)"
		}
	    }

	    dict with d {
		if {$script ne ""} {
		    puts stderr "Running: $d"
		    uplevel #0 $script
		}
		return $version
	    }
	}

	proc ifneeded {package version {script ""}} {
	    if {$script eq ""} {
		set d [$statement(version) -as dict]
		if {[dict size $d]} {
		    puts stderr "Package ifneeded $package $version -> [dict get $d script]"
		    return [dict get $d script]
		} else {
		    puts stderr "Package ifneeded $package $version -> UNREGISTERED"
		    return ""
		}
	    } else {
		$statement(replace) execute
		puts stderr "Package ifneeded $package $version -> [dict get $d script]"
		return ""
	    }
	}
    }
}

package require Package
puts stderr "DONE PRIMING"
package require Tcl
package require fileutil
package require moop
