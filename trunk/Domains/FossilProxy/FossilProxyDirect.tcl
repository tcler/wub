package require OO
package require Cookies
package require Query
package require jQ

package provide FossilProxyDirect 1.0

set ::API(Domains/FossilProxyDirect) {
    {
	A domain to provide helper functions for managing multiple fossil repositories.
    }
    fossil_dir {Directory where fossil repositories are located. The proxy will work for all repositories in this directory which are named *.fossil, where the basename of the repository is part of the URL. Mandatory.}
    fossil_command {Path to fossil command. Default is 'fossil'}
    prefix {Path where fossil repositories are mounted in the URL. Mounted in root is the default.}
    repositories_list_body {HTML body used to show list of available repositories. %REPOS% is replace by an unordered list of repositories. Default is empty.}
}

oo::class create FossilProxyDirect {

#     method /privs/toggle {r R U P} {
# 	variable fossil_dir
# 	variable fossil_command
# 	if {[catch {exec $fossil_command user capabilities $U -R [file join $fossil_dir $R]} Res]} {
# 	    error $Res
# 	}
# 	set idx [string first $P $Res]
# 	if {$idx >= 0} {
# 	    set Res [string replace $Res $idx $idx]
# 	    set msg "Revoked $P from user $U for repository $R: $Res"
# 	} else {
# 	    append Res $P
# 	    set msg "Granted $P to user $U for repository $R: $Res"
# 	}
# 	if {[catch {exec $fossil_command user capabilities $U $Res -R [file join $fossil_dir $R]} Res]} {
# 	    error $Res
# 	}
# 	return [Http NoCache [Http Ok $r $msg]]
#     }

    method /privs {r} {
	variable prefix
	variable fossil_dir
	variable fossil_command
	set uidl {}
	set C ""
	set repoid 0
	set privid 0
	set fnml [lsort -dictionary [glob -nocomplain -tails -dir $fossil_dir *.fossil]]
	foreach fnm $fnml {
	    append C [<a> name repo$repoid [<h2> [file rootname $fnm]]]
	    set rnm [file join $fossil_dir $fnm]
	    if {[catch {exec $fossil_command user list -R $rnm} R]} {
		error $R
	    }
	    unset -nocomplain kprivs
	    unset -nocomplain privs
	    unset -nocomplain uidl
	    foreach l [split $R \n] {
		set idx [string first " " $l]
		set uid [string trim [string range $l 0 $idx]]
		set contact [string trim [string range $l $idx end]]
		lappend uidl [list $uid $contact]
		if {[catch {exec $fossil_command user capabilities -R $rnm $uid} P]} {
		    error $P
		}
		foreach p [split $P {}] {
		    set kprivs($p) 1
		    set privs($uid,$p) 1
		}
	    }
	    set data {}
	    foreach l $uidl {
		lassign $l uid contact
		set l [list uid $uid contact $contact]
		foreach p [lsort -dictionary [array names kprivs]] {
		    #lappend l $p "<input id='$privid' type='checkbox' OnClick='togglePrivs($privid, \"$fnm\", \"$uid\", \"$p\")' [expr {[info exists privs($uid,$p)]?"checked":""}]>"
		    lappend l $p [expr {[info exists privs($uid,$p)]?"X":"-"}]
		    incr privid
		}
		lappend data [incr i] $l
		append C "  </tr>\n"
	    }
	    append C [Report html $data headers [list uid contact {*}[lsort -dictionary [array names kprivs]]] class tablesorter sortable 0 evenodd 0 htitle ""]
	    incr repoid
	}
	set T [<h1> "Repository privileges"]\n
	append T <ul>\n
	set repoid 0
	foreach fnm $fnml {
	    append T [<li> [<a> href #repo$repoid [file rootname $fnm]]]\n
	    incr repoid
	}
	append T </ul>\n
	append T $C
	set r [jQ tablesorter $r table]
	dict set r -content $T
	dict set r content-type x-text/html-fragment
	dict set r -title "Repository privileges"
	return [Http NoCache [Http Ok $r]]
    }

    method / {r} {
	variable prefix
	variable fossil_dir
	variable repositories_list_body
	set C "<ul>\n"
	foreach fnm [lsort -dictionary [glob -nocomplain -tails -dir $fossil_dir *.fossil]] {
	    append C [<li> [<a> href $prefix/[file rootname $fnm] [file rootname $fnm]]]\n
	}
	append C "</ul>\n"
	return [Http NoCache [Http Ok $r [regsub {%REPOS%} $repositories_list_body $C]]]
    }

    mixin Direct

    constructor {args} {
	variable prefix ""
	variable fossil_command "fossil"
	variable repositories_list_body "<h1>Available repositories:</h1>\n%REPOS%"

	variable {*}[Site var? FossilProxyDirect] {*}$args ;# allow .ini file to modify defaults

	if {![info exists fossil_dir]} {
	    error "fossil_dir not set"
	}

	catch {next {*}$args}
    }
}
