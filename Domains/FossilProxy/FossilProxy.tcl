package require OO
package require Cookies
package require Query

package provide FossilProxy 1.0

set ::API(Domains/FossilProxy) {
    {
	A domain to serve as proxy for the 'fossil http' web interface.
    }
    fossil_dir {Directory where fossil repositories are located. The proxy will work for all repositories in this directory which are named *.fossil, where the basename of the repository is part of the URL. Mandatory.}
    fossil_command {Path to fossil command. Default is 'fossil'}
    prefix {Path where fossil repositories are mounted in the URL. Mounted in root is the default.}
}

oo::class create FossilProxy {

    method strip_prefix { path } {
 	variable prefix
 	if {[string length $prefix] && [string match "$prefix*" $path]} {
 	    set path [string range $path [string length $prefix] end]
 	}
	return $path
    }

    method list_repositories {r} {
	variable prefix
	variable fossil_dir
	set C [<h1> "Known repositories:\n"]
	append C "<ul>\n"
	foreach fnm [lsort -dictionary [glob -nocomplain -tails -dir $fossil_dir *.fossil]] {
	    append C [<li> [<a> href $prefix/[file rootname $fnm] [file rootname $fnm]]]\n
	}
	append C "</ul>\n"
	return [Http NoCache [Http Ok $r $C]]
    }

    method do { r } { 

	variable fnmid
	variable prefix
	variable fossil_dir
	variable fossil_command

	# Construct a HTTP request to send to 'fossil http', strip the prefix as fossil doesn't know about it
	if {[dict get $r -method] eq "POST"} {
	    set fr "POST [my strip_prefix [dict get $r -path]]"
	    append fr " HTTP/1.1\n"
	} else {
	    lassign [dict get $r -header] meth url ver
	    set url [my strip_prefix $url]
	    if {$url eq "" && [file isdirectory $fossil_dir]} {
		return [my list_repositories $r]
	    }
	    set fr "$meth $url $ver\n"
	}

	# Add headers to request
	dict for {k v} $r {
	    switch -nocase -glob -- $k {
		-* {}
		default { append fr "$k: $v\n" }
	    }
	}
	# Add content to request
	if {[dict exists $r -entity]} {
	    append fr \n[dict get $r -entity]
	}
	
	# Use a thread to process the request to avoid blocking on long running calls
	return [Httpd Thread {

	    package require Cookies
	    package require Dict

	    set qfnm Q$fnmid
	    set f [open $qfnm w]
	    fconfigure $f -encoding binary -translation binary
	    puts -nonewline $f $fr
	    close $f

	    # Call fossil
	    set fnm R$fnmid
	    set f [open $fnm w]
	    fconfigure $f -encoding binary -translation binary
	    if {[catch {exec $fossil_command http $fossil_dir >@ $f < $qfnm} R]} {
		error $R
	    }
	    close $f

	    set f [open $fnm r]
	    fconfigure $f -encoding binary -translation binary
	    set R [read $f]
	    close $f

	    file delete $qfnm
	    file delete $fnm

	    # Extract headers from response
	    set n 0
	    set response 404
	    set location ""
	    set content_type "test/html"
	    set content_length -1
	    set content_found 0
	    foreach l [split $R \n] {
		incr n
		if {[string length $l] == 0} {
		    set content_found 1
		    break
		}
		switch -nocase -glob -- $l {
		    "HTTP/*" {
			lassign [split $l] http response
		    }
		    "Content-Type:*" {
			set content_type [string trim [string range $l 13 end]]
		    }
		    "Content-Length:*" {
			set content_length [string trim [string range $l 15 end]]
		    }
		    "Location:*" {
			set location [string trim [string range $l 9 end]]
		    }
		    "Set-Cookie:*" {
			# Pass on cookies, make sure to fix the path by adding prefix
			set cdict [lindex [Cookies parse4client [string trim [string range $l 11 end]]] 1]
			set r [Cookies Add $r -path $prefix[dict get? $cdict -Path] -name [dict get? $cdict -name] -value [dict get? $cdict -value] -expires "next month"]
		    }
		}
	    }
	    
	    # Extract contents from response
	    set C ""
	    if {$content_length >= 0} {
		set C [string range $R end-[expr {$content_length-1}] end]
	    }
	    
	    # Fix up prefixes if not mounted in /
 	    if {[string length $prefix] && [string match "text/html*" $content_type]} {
 		regsub -all { href=\"\/} $C " href=\"$prefix/" C
 		regsub -all { href=\'\/} $C " href='$prefix/" C
 		regsub -all { src=\"\/} $C " src=\"$prefix/" C
 		regsub -all { src=\'\/} $C " src='$prefix/" C
 	    }

	    # Send responses
	    switch -exact -- $response {
		200 {
		    return [Http NoCache [Http Ok $r $C $content_type]]
		}
		302 {
		    # Make sure to fix the path by adding prefix
		    return [Http Redirect $r $prefix$location]
		}
		404 {
		    return [Http NotFound $r]
		}
		default {
		    return [Http NoCache [Http Ok $r "Dont know what to do with 'fossil http' response:\n$R"]]
		}
	    }

	} r $r fr $fr fossil_dir $fossil_dir fossil_command $fossil_command prefix $prefix fnmid [incr fnmid]]
    }

    constructor {args} {
	variable fnmid 0
	variable prefix ""
	variable fossil_command "fossil"
	variable {*}[Site var? FossilProxy] {*}$args ;# allow .ini file to modify defaults
	if {![info exists fossil_dir]} {
	    error "fossil_dir not set"
	}
	catch {next {*}$args}
    }
}
