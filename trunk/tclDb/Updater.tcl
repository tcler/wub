package require Db.tcl
package require Mk4tcl

variable f2db
variable db2tid
variable version
variable db_count

# commit changes to db
proc commit {db} {
    tsv::lock db {
	set v $::version($db)
	if {$v != [tsv::get db $db]} {
	    mk::file commit $db
	    tsv::set db $db $v
	}
    }

    # inform each interested thread about the reload
    set file [dict get [mk::file open] $db]
    foreach {n script} [array get ::db2tid *,$db]  {
	lassign [split $n ,] tid
	if {$script eq {}} {
	    set script mkReload
	}
	thread::send -async -head $tid [list {*}script $db $file]}
    }

    return $v
}

proc update {db view row args} {
    tsv::lock db $db {
	set result [mk::set "$db.$view!$row" {*}$args]
	incr ::version($db)
    }
    return $result
}

proc append {db view args} {
    tsv::lock db $db {
	set result [mk::row append $db.$view {*}$args]
	incr ::version($db)
    }
    return $result
}

proc interest {tid file args} {
    set file [file normalize $file]
    if {![info exists ::f2db($file)]} {
	set ::f2db($file) [mk::file open db[incr db_count] $file]
    }
    set ::db2tid($tid,$db) $args	;# remember which threads care
    return $::f2db($file)
}

proc forget {tid db} {
    unset ::db2tid($tid,$db) $args	;# deregister interest
}

thread::wait
