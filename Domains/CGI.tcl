# cgi interface.  Blerk.
package require Html
package require Debug
Debug on cgi 1000

package provide CGI 1.0

namespace eval CGI {
    variable fields {}
    proc env {r} {
	lappend env SERVER_SOFTWARE $::Httpd::server_id
	# name and version of the server. Format: name/version

	lappend env GATEWAY_INTERFACE CGI/1.1
	# revision of the CGI specification to which this server complies.
	# Format: CGI/revision

	lappend env SERVER_NAME [Dict get? $r -host]
	# server's hostname, DNS alias, or IP address
	# as it would appear in self-referencing URLs.

	lappend env SERVER_PROTOCOL [Dict get? $r -scheme]
	# name and revision of the information protcol this request came in with.
	# Format: protocol/revision

	lappend env SERVER_PORT [Dict get? $r -port]
	# port number to which the request was sent.

	lappend env REQUEST_URI [Dict get? $r -uri]
	lappend env REQUEST_METHOD [Dict get? $r -method]
	# method with which the request was made.
	# For HTTP, this is "GET", "HEAD", "POST", etc.

	lappend env QUERY_STRING [Dict get? $r -query]
	# information which follows the ? in the URL which referenced this script.
	# This is the query information. It should not be decoded in any fashion.
	# This variable should always be set when there is query information,
	# regardless of command line decoding.

	lappend env PATH_INFO [Dict get? $r -info]
	# extra path information, as given by the client.
	# Scripts can be accessed by their virtual pathname, followed by
	# extra information at the end of this path.
	# The extra information is sent as PATH_INFO.
	# This information should be decoded by the server if it comes
	# from a URL before it is passed to the CGI script.

	lappend env PATH_TRANSLATED [Dict get? $r -translated]
	# server provides a translated version of PATH_INFO,
	# which takes the path and does any virtual-to-physical mapping to it.

	lappend env SCRIPT_NAME [Dict get? $r -script]
	# A virtual path to the script being executed, used for self-referencing URLs.

	lappend env REMOTE_ADDR [Dict get? $r -ipaddr]
	# IP address of the remote host making the request.

	if {[dict exists $r -entity]} {
	    lappend env CONTENT_TYPE [Dict get? $r content-type]
	    # For queries which have attached information, such as HTTP POST and PUT,
	    # this is the content type of the data.

	    lappend env CONTENT_LENGTH [Dict get? $r content-length]
	    # The length of the said content as given by the client.
	}

	# Header lines received from the client, if any, are placed
	# into the environment with the prefix HTTP_ followed by the header name.
	# If necessary, the server may choose to exclude any or all of these headers
	# if including them would exceed any system environment limits.
	variable fields
	foreach field $field {
	    if {[dict exists $r $field]} {
		lappend env [string map {- _} [string toupper $field]] [dict get $r $field]
	    }
	}

	return $env
    }

    # parseSuffix - given a suffix, locate the named object and split its name
    # components into usable variables.
    proc parseSuffix {suffix} {
	Debug.cgi {parseSuffix $suffix}
	set dir [expr {[string index $suffix end] eq "/"}]
	if {$dir} {
	    # strip trailing '/'
	    set suffix [string range $suffix 0 end-1]
	}
	set ext [file extension $suffix]	;# file extension
	set path [file rootname $suffix]	;# entire path except extension
	set ftail [file tail $suffix]		;# last component of path
	set tail [file rootname $ftail]		;# last component except extension

	Debug.cgi {parseSuffix first cut: path:'$path' tail:'$tail' ftail:'$ftail' ext:'$ext' suffix:'$suffix' dir:$dir}

	# map names which are only extensions to their parent+extension
	# avoid sending files with hidden names, thus /fred/.add -> fred.add
	if {($tail eq "") && ($ext ne "")} {
	    # this is a file name like '.../.tml', or is hidden
	    Debug.cgi {parseSuffix transposing $ext to parent$ext}
	    error "'$path' is an illegal script"
	}

	# at this point we have a full path and inode

	# normalize path - reject any paths beginning with .
	if {[string first "/." $path] != -1} {
	    Debug.cgi {parseSuffix $suffix - $path illegal name}
	    error "'$path' has illegal name."
	} else {
	    set path [string trimleft $path "/."]
	}

	set ext [string trimleft $ext .]	;# remove leading .

	# keep the file variables in request dict for future reference
	foreach v {suffix ext tail ftail path inode dir} {
	    dict set retval $v [set $v]
	}

	Debug.cgi {parseSuffix $suffix -> $retval}
	return $retval
    }

    # a set of executors for each extension
    variable executors {.CGI ""}
    foreach {ext lang} {.TCL tclsh .PY python .PL perl .SH bash .PHP php} {
	catch {
	    set l [join [exec which [lindex $lang 0]] [lrange $lang 1 end]]
	    lappend executors $ext $l
	}
    }

    proc _do {mount root r} {
	# grab some useful file values from request's url
	if {[dict exists $r -suffix]} {
	    # caller has set suffix
	    set suffix [dict get $r -suffix]	;# suffix of request
	} else {
	    # assume we've been parsed by package Url
	    # remove the specified prefix from path, giving suffix
	    set path [dict get $r -path]
	    set suffix [Url pstrip $mount $path]
	    Debug.cgi {-suffix not given - calculated '$suffix' from '$mount' and '$path'}
	    if {($suffix ne "/") && [string match "/*" $suffix]} {
		# path isn't inside our domain suffix - error
		Debug.cgi {-suffix $suffix not in $mount domain}
		return [Http NotFound $r]
	    }
	}

	# parse suffix into semantically useful fields
	if {[catch {
	    parseSuffix $suffix
	} fparts eo]} {
	    return [Http NotFound $r $fparts]
	}
	Debug.pub {parsed URL into '$fparts'}

	dict set req -fparts $fparts	;# record file parts in request
	dict with req -fparts {}	;# grab some useful values from req
	dict set req -suffix $suffix	;# remember the calculated suffix in req

	# derive a script path from the URL path fields
	Debug.pub {searching for [file join $root $suffix]}
	set extlc [string tolower $ext]
	set suff [file split $path$extlc]
	dict set r -info {}
	while {$suff ne {}} {
	    set probe [file join $root {*}$suff]
	    set ext [file extension $probe]
	    set extlc [string tolower $ext]
	    set probe [file root $probe]$extlc
	    if {[file exists $probe]} {
		break
	    }
	    dict lappend r -info "[lindex $suff end]$extlc"
	    set suff [lrange $suff 0 end-1]
	}
	dict set r -translated $probe[dict get $r -info]

	# only execute scripts with appropriate extension
	if {[catch {
	    dict get $executors $ext
	} executor]} {
	    return [Http Forbidden $r [<p> "Can't execute files of type $ext"]]
	}

	if {$suff eq {}} {
	    # we've failed to find a match
	    return [Http NotFound $r]
	    # could do a search with different variant extensions
	} else {
	    # found our script
	    set script [file join $mount {*}$suff]
	    dict set req -script $script
	}

	array set ::env [env $r]	;# construct the environment per CGI 1.1
	variable maxcgi
	variable cgi
	if {[incr cgi] > $maxcgi} {
	    return [Http GatewayTimeout $r "Maximum CGI count exceeded"]
	}
	dict set r -Query [Query parse $r]
	if {[dict get $r -method] ne "POST"} {
	    set arglist [Query flatten [dict get $r -Query]]
	} else {
	    set arglist {}
	}

	# move into the script dir
	set pwd [pwd]
	cd [file dirname $script]

	# execute the script
	Debug.cgi {running: open "|{*}$executor $script {*}$arglist"}
	if {[catch {
	    open "|{*}$executor $script {*}$arglist <<[Dict get? $r -entity] 2>@1" r+
	} pipe eo]} {
	    cd $pwd
	    Debug.error {CGI: Error $pipe ($eo) "|{*}$executor $script {*}$arglist"}
	    return [Http ServerError $r $pipe $eo]
	} else {
	    fconfigure $pipe -translation {auto binary}
	    cd $pwd
	}

	# get content from CGI process
	while {1} {
	    set n [gets $pipe line]
	    if {$n == -1} {
		# cgi dead
		return [Http Ok $r ""]
	    } elseif {$n == 0} {
		break	;# end of header
	    } elseif {[string index $line 0] ne " "} {
		# read a new header
		set line [string trim [join [lassign [split line :] header] :]]
		dict set r $header $line
		Debug.cgi {header: $header '$line'}
	    } else {
		# get field continuation
		dict append r $header " " [string trim $line]
		Debug.cgi {continuation: $header '$line'}
	    }
	}

	# read the rest of the content
	fconfigure $pipe -translation {binary binary}
	set content [read $pipe]
	close $pipe

	Debug.cgi {read body '$content'}
	return [Http Ok $r $content]
    }

    variable mount /CGI/
    variable root /var/www/cgi-bin/

    proc init {cmd args} {
	if {$args ne {}} {
	    variable {*}$args
	}
	variable mount
	variable root
	set cmd [uplevel 1 namespace current]::$cmd
	namespace ensemble create \
	    -command $cmd -subcommands {} \
	    -map [subst {
		do "_do $mount $root"
	    }]

	return $cmd
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
