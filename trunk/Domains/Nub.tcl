# Nub - a domain for configuration of Wub

if {[info exists argv0] && ($argv0 eq [info script])} {
    lappend auto_path ../Utilities ../extensions .
}

package require Debug
Debug off nub 10

package require fileutil
package require functional
package require Url
package require Direct

package require Html
package require Form
package require jQ
package provide Nub 1.0
package provide Rewrite 1.0	;# to satisfy requirement

set API(Nub) {
    {Configuration domain for Wub.  Provides simple text and web-based website configuration.}
    nubdir {directory for user-defined nubs}
    theme {jQuery theme for Nub web interaction}
    password {password for modifying nubs.}
}

namespace eval Nub {
    set showhide {
	// http://www.learningjquery.com/2006/09/slicker-show-and-hide

	// hides the slickbox as soon as the DOM is ready
	// (a little sooner than page load)
	$('#box').hide();
	    
	// toggles the slickbox on clicking the noted link  
	$('a#toggle').click(function() {
	    $('#box').toggle(400);
	    return false;
	});
    }

    proc /css {r} {
	set css "* {zoom: 1.0;}"
	return [Http Ok $r $css text/css]
    }

    proc donub {urls key count} {
	set script [dict get $urls $key]
	set extra ""
	set disable 0
	dict with script {
	    lassign $domain domain name
	    
	    switch -- [string tolower $domain] {
		redirect {
		    set extra [Form <text> to_$count label "To:" [tclarmour $body]]
		    append extra [<p> "Redirect $section URL to the $body URL"]
		} 
		rewrite {
		    set extra [Form <textarea> to_$count class autogrow label "To:" [tclarmour $body]]
		    append extra [<p> "Rewrite $section URL to $body"]
		}
		
		block {
		    set extra [Form <checkbox> block_$count label "Block?" checked 1 value 1]
		    append extra [<p> "Block any client which accesses $section from this server."]
		    set disable 1
		}
		literal {
		    dict with body {
			append extra [<br>]
			append extra [Form <textarea> content_$count class autogrow cols 80 label "Content: " [tclarmour [armour $content]]]
			append extra [Form <text> ctype_$count label "Mime Type: " [tclarmour $ctype]]
		    }
		    append extra [<p> "Return the literal content of the given mime type."]
		}
		code {
		    dict with body {
			append extra [<br>]
			append extra [Form <textarea> content_$count class autogrow cols 80 label "Content: " [tclarmour [armour $content]]]
			append extra [Form <text> ctype_$count label "Mime Type: " [armour $ctype]]
		    }
		    append extra [<p> "Return the result of evaluating content as a Tcl expression, with the given mime type."]
		}
		default {
		    global API
		    catch {package require $domain}
		    set extra [<legend> "$domain parameters"]\n
		    set opts [lassign $API($domain) description]
		    if {[llength $opts]} {
			foreach {opt text} $opts {
			    set val [tclarmour [armour [Dict get? $body $opt]]]
			    set text [tclarmour [armour $text]]
			    if {[string match +* $text]} {
				append extra [<textarea> ${opt}_$count cols 80 class autogrow label "[string totitle $opt]: " title $text $val] \n
			    } else {
				append extra [<text> ${opt}_$count label "[string totitle $opt]: " title $text $val] \n
			    }
			}
			#puts stderr "EXTRA: $extra"
			set extra [Form <fieldset> vertical 1 $extra]
		    } else {
			set extra ""
		    }
		    append extra [<p> [tclarmour "$domain domain: $description"]]
		}
	    }

	    set path ""
	    set path [join [lassign $key host] /]	;# get host/path
	    if {$host ni {"" *}} {
		set url ${host}/$path
	    } else {
		set url $path
	    }
	    set form [<form> f$count action nubs {
		[<fieldset> {
		    [<legend> [tclarmour "$domain $section"]]
		    [<text> url_$count disable $disable label "Url: " [tclarmour $url]]
		    [tclarmour $extra]
		    [<hidden> domain_$count $domain]
		    [<submit> submit title "Change this Nub" style {float:right} value "edit $count" Change]
		    [<submit> submit title "Delete this Nub" style {float:right} value "delete $count" Delete]
		    [<reset> reset title "Reset" style {float:right} Reset]
		}]
	    }]\n
	}
	return [list "$domain $section" $form]
    }

    variable password ""
    proc credentials {r} {
	variable password
	if {$password eq ""} {
	    return "No Nub Password has been set.  Check site.ini and add a password definition under section \[nub\]."
	}
	lassign [Http Credentials $r] userid pass
	if {$pass ne $password} {
	    return "Passwords don't match.  Check site.ini for password."
	} else {
	    return ""
	}
    }

    variable loaded {}
    variable theme start
    variable keymap
    proc /nubs {r {submit ""} args} {
	Debug.nub {/nubs ($submit) $args}
	variable theme
	dict lappend r -headers [<stylesheet> css]
	set r [jQ theme $r $theme]

	variable urls

	variable keymap
	set submit [split $submit]
	set error ""
	set etitle ""

	
	if {[lindex $submit 0] ne "" && [credentials $r] ne ""} {
	    set challenge "Nub Modification"
	    return [Http Unauthorized $r [Http BasicAuth $challenge]]
	} else {
	    switch -- [lindex $submit 0] {
		edit {
		    # edit this element
		    set el [lindex $submit end]
		    set url [dict get $args url_$el]; dict unset args url_$el
		    set section [dict get $urls $keymap($el) section]
		    if {$url ne $section} {
			# they've changed the url - copy-edit its content
			set nkey [parseurl $url]
			set urls $nkey [dict merge [dict get $urls $keymap($el)] [list section $url]]
			set keymap($el) $nkey
			set section $url
		    }
		    
		    set domain [dict get $args domain_$el]; dict unset args domain_$el
		    switch -- $domain {
			Redirect -
			Rewrite {
			    dict set urls $keymap($el) body [dict get $args to_$el]
			}
			
			Block {
			    if {![dict get $args block_$el]} {
				dict unset urls $keymap($el)
			    }
			}
			Literal -
			Code {
			    dict set urls $keymap($el) body content [dict get $args content_$el]
			    dict set urls $keymap($el) body ctype [dict get $args ctype_$el]
			}
			default {
			    # this is a proper domain
			    global API
			    set opts [lassign $API($domain) description]
			    foreach {opt text} $opts {
				if {[dict exists $args ${opt}_$el]} {
				    dict set urls $keymap($el) body $opt [dict get $args ${opt}_$el]
				}
			    }
			}
		    }
		}
		delete {
		    # delete this element
		    set el [lindex $submit end]
		    dict unset urls $keymap($el)
		}
		add {
		    # add a new element
		    foreach v {host domain path} {
			set $v [dict get $args ${v}_new]
		    }
		    set path /[string trimleft $path /]
		    if {$host ni {"" *}} {
			set section $host/$path
		    } else {
			set section $path
		    }
		    set key [parseurl $section]
		    dict set urls $key [list domain $domain section $section body {}]
		}
		load {
		    if {[catch {configF [dict get $args load_file]} result]} {
			lappend error $result
		    }
		    set etitle "Loading"
		}
		save {
		    variable urls
		    generate $urls
		    set etitle "Saving"
		    if {![llength $error]} {
			set result ""
			foreach {key val} $urls {
			    dict with val {
				switch -- $domain {
				    Literal -
				    Code {
					dict with body {
					    set line "[string tolower $domain] $section [list $content] $ctype"
					}
				    }
				    Redirect -
				    Rewrite {
					set line "[string tolower $domain] $section $body"
				    }
				    Block {
					set line "[string tolower $domain] $section"
				    }
				    default {
					set line "domain [list $domain] $section $body"
				    }
				}
			    }
			    append result $line \n
			}
			variable nubdir
			set file [file rootname [file join $nubdir [file tail [dict get $args save_file]]]].nub
			
			if {[file exists $file]} {
			    file rename -force $file $file.[clock seconds]
			}
			if {[catch {
			    ::fileutil::writeFile $file $result
			} e]} {
			    set error "Failed to Save in $file: $e"
			}
			Debug.nub {SAVE: $file $e ($result)}
			lappend error "Saved $file"
		    } else {
			lappend error "Refused to Save"
		    }
		}
		apply {
		    variable urls
		    set do [generate $urls]
		    if {$error eq ""} {
			eval $do
		    }
		    set etitle "Applying"
		}
	    }
	}

	catch {unset keymap}

	global API
	set selection [lsort -dictionary [list Rewrite Block Redirect {*}[array names API] Literal Code]]
	append content [<form> new style {float:left;} {
	    [<fieldset> {
		[<legend> "New Nub"]
		[<selectlist> domain_new label "Type: " $selection]
		[<text> path_new label "Url: " ""]
		[<submit> submit value add Add]
		[<p> "Create a new nub with the given type for the given path and host."]
	    }]
	}]
	append content [<form> compile style {float:left;} {
	    [<fieldset> {
		[<legend> "Apply Nubs"]
		[<submit> submit style {float:right} value apply Apply]
		[<p> "Compile nubs and attempt to reconfigure the server."]
	    }]
	}]	
	append content [<br> clear both]

	variable nubdirSys; variable nubdir
	set selection {}
	if {[info exists nubdir] && $nubdir ne ""} {
	    lappend selection {*}[glob -nocomplain -tails -directory $nubdir *.nub]
	}
	lappend selection {*}[glob -nocomplain -tails -directory $nubdirSys *.nub]
	set selection [lsort -dictionary $selection]
	append content [<form> save style {float:left;} {
	    [<fieldset> {
		[<legend> "Load Nubs"]
		[<selectlist> load_file label "File: " $selection]
		[<submit> submit value load Load]
		[<p> "Load nub file."]
	    }]
	}]
	append content [<form> save style {float:left;} {
	    [<fieldset> {
		[<legend> "Save Nubs"]
		[<text> save_file label "Name: " ""]
		[<submit> submit disabled [expr {![info exists nubdir]}] value save Save]
		[expr {[info exists nubdir]?[<p> "Save nubs into a file in the nubdir directory, which may be then loaded to configure the server."]:[<p> "Can't save nubs until the Nub nubdir directory has been specified in site.ini"]}]
	    }]
	}]
	append content [<br> clear both][<hr>]

	variable loaded
	set header [<h3> "Nubs from $loaded"]
	append header [<a> id toggle href # "What's this? ..."]
	append header \n [subst {
	    [<div> id box style [subst {
		[<p> "This facility allows you to change the URLs the server responds to."]
		[<p> "Below, you will see a collection of 'nubs', which are mappings from a URL glob to a domain.  You may create new nubs, edit or delete existing nubs, and apply them to the currently running server."]
		[<p> "Each domain provides different functionality:"]
		[<dl> [subst {
		    [<dt> [<a> href /docs/docs/block.html Block]]
		    [<dd> "Blocks an IP address which attempts to access the URL"]
		    [<dt> [<a> href /docs/docs/rewrite.html Rewrite]]
		    [<dd> "Transforms a URL into another."]
		    [<dt> [<a> href /docs/docs/redirect.html Redirect]]
		    [<dd> "Sends the client a redirect."]
		}]]
	    }]]
	}] \n
	append header [<hr>]

	set content "$header\n$content"

	if {$error ne ""} {
	    append content [<h3> "Result of $etitle:"]
	    append content [<p> class message [join $error "</p><p class='message'>"]]
	    append content [<hr>]
	}

	# order urls by key length - longest first
	set ordered [lsort -command urlorder [dict keys $urls]]
	set count 0
	set nubs {}
	foreach key $ordered {
	    if {[dict get $urls $key domain] ne "Rewrite"} continue
	    set keymap([incr count]) $key
	    dict set nubs {*}[donub $urls $key $count]
	}

	foreach key $ordered {
	    if {[dict get $urls $key domain] ne "Block"} continue
	    set keymap([incr count]) $key
	    dict set nubs {*}[donub $urls $key $count]
	}

	foreach key $ordered {
	    if {[dict get $urls $key domain] ne "Redirect"} continue
	    set keymap([incr count]) $key
	    dict set nubs {*}[donub $urls $key $count]
	}

	foreach key $ordered {
	    if {[dict get $urls $key domain] in {Block Rewrite Redirect}} continue
	    set keymap([incr count]) $key
	    dict set nubs {*}[donub $urls $key $count]
	}
	append content [<br>][jQ dict2accordion $nubs class accordion] \n
	append content [<hr>]

	set r [jQ accordion $r .accordion active false alwaysOpen false clearStyle true autoHeight false fillSpace false]
	set r [jQ autogrow $r .autogrow]
	variable showhide; set r [jQ postscript $r $showhide]
	return [Http Ok $r $content]
    }

    proc / {r} {
	variable urls
	# order urls by key length - longest first
	set ordered [lsort -command urlorder [dict keys $urls]]
	foreach key $ordered {
	    set script [dict get $urls $key]
	    set section [dict get $script section]; dict unset script section
	    set domain [dict get $script domain]; dict unset script domain
	    set path [join [lassign $key host] /]
	    lappend u [<li> "[<a> href url?host=$host&path=$path $section] $domain"]
	}
	return [Http Ok $r [<ul> [join $u \n]]]
    }

    proc urlorder {k1 k2} {
	# make shorter lists come later in the order
	set diff [expr {[llength $k2] - [llength $k1]}]
	if {$diff != 0} {
	    return $diff
	}

	# make wildcards come later in the order
	if {[string map {* \xff} $k1]
	    >= [string map {* \xff} $k2]
	} {
	    return 1
	} else {
	    return -1
	}
    }

    proc outerr {name1 name2 op} {
	upvar $name1 error
	puts stderr "ERROR: $error"
    }

    proc generate {urls {domains {}} {defaults {}}} {
	upvar error error

	#trace add variable error {write} outerr

	# order urls by key length - longest first
	set ordered [lsort -command urlorder [dict keys $urls]]

	Debug.nub {URLs in order $ordered}
	Debug.nub {URLs: $urls}

	set processed {}
	set rewrites {}
	set redirects {}

	foreach key $ordered {
	    set section [dict get $urls $key section]
	    set domain [dict get $urls $key domain]

	    # get domain from section and constructor args, if any
	    Debug.nub {processing: $key - $section - $domain}
	    switch -- [string tolower [lindex $domain 0]] {
		redirect {
		    dict set redirects $key [dict get $urls $key body]; continue
		}
		rewrite {
		    variable uniq
		    set name _rewrite[incr uniq]	;# so make up a name
		    dict set rewrites $key $name
		    dict set domains $name [list domain $domain body [dict get $urls $key body]]
		    continue
		}
		block {
		    dict set blocks $key {}; continue
		}
		literal {
		    dict set processed $key [dict get $urls $key]; continue
		}
		code {
		    dict set processed $key [dict get $urls $key]; continue
		}

		default {
		    set name ""
		    lassign $domain domain name
		    set body [string trim [dict get $urls $key body]]
		    if {[string index $domain 0] ne [string toupper [string index $domain 0]]} {
			# this is a named domain reference, e.g. [/moop/] domain=fred
			if {[info exists domains $domain] && [llength $dargs]} {
			    lappend error "[dict get $urls $key _section]: Can't specify named domain $domain (defined in [dict get $domains $domain section] with constructor arguments.  Try just domain=$domain"
			}
			set name $domain
			set domain unknown
			dict set processed $key [list domain $domain name $name section $section]
		    } else {
			# this is a Domain definition e.g. [/moop/] domain="File fred .."
			# get the section name
			Debug.nub {defining domain: $name}
			# see if we're defining or merely referencing domain
			if {$name eq ""} {
			    # anonymous domain of type Domain
			    variable uniq
			    set name _anonymous[incr uniq]	;# so make up a name
			}
			
			if {[dict exists $domains $domain]} {
			    if {$body ne ""} {
				lappend error "$section: can't respecify arguments to Domain $name"
				continue
			    }
			} else {
			    # defining a new domain

			    # add 'mount' parameter
			    append body " mount [join [lrange $key 1 end] /]"
			    
			    # create newly defined domain
			    if {[string match */ $section]} {
				# if the key is a directory, we redirect literals
				set rkey [parseurl [string trimright $section /]]
				dict set redirects $rkey $section
			    }
			    dict set domains $name [list domain $domain body $body]
			    dict set processed $key [list domain $domain name $name section $section]
			    
			    Debug.nub {DOMAIN $name domain $domain ($body)}
			}
		    }
		}
	    }
	}

	Debug.nub {DOMAINS: $domains}
	Debug.nub {PROCESSED: $processed}
	#Debug.nub {REDIRECTS: $redirects}
	Debug.nub {REWRITES: $rewrites}
	#Debug.nub {BLOCK: $blocks}
	#Debug.nub {URLS: $urls}

	set blocking {}
	foreach {from .} $blocks {
	    set url [join [lassign $from host] /]
	    lappend blocking "$host,$url"
	}
	set blocking [join $blocking " -\n"]

	# process definitions
	set definitions ""
	foreach {n d} $domains {
	    Debug.nub {DEFINING: $n $d}
	    set domain [dict get $d domain]; dict unset d domain
	    set body [dict get $d body]; dict unset d body

	    if {![info exists defined($domain)]} {
		incr defined($domain)
		append definitions "package require $domain" \n
	    }

	    if {[string match _anonymous* $n]} {
		append definitions [string trim [string map [list %N $n %D $domain %A $body] {
		    if {[catch {set defs(%N) [%D new %A]} e eo]} {
			Debug.error {Nub Definition Error: '$e' in anonymous "%D new %A".  ($eo)}
		    }
		}] \n] \n
	    } elseif {[string match _rewrite* $n]} {
		set def [string trim [string map [list %N $n %L $body] {
		    if {[catch {set defs(%N) {::apply {r {return "%L"}}}} e eo]} {
			Debug.error {Nub Definition Error: '$e' in rewrite "lambda r {%L}".  ($eo)}
		    }
		}] \n]
		append definitions $def \n
	    } else {
		append definitions [string trim [string map [list %N $n %D $domain %A $body] {
		    if {[catch {set defs(%N) [%D create %N %A]} e eo]} {
			Debug.error {Nub Definition Error: '$e' in running "%D create %N %A".  ($eo)}
		    }
		}] \n] \n
	    }
	}
	Debug.nub {DEFINED: $definitions}

	set rewriting ""
	foreach {from name} $rewrites {
	    set url [join [lassign $from host] /]
	    append rewriting [string map [list %H [expr {$host eq "*"?".*":$host}] %U $url %N $name] {{^%H,%U$} { set url [{*}$defs(%N) $r] }}] \n
	}

	set redirecting ""
	foreach {from to} $redirects {
	    set url [join [lassign $from host] /]
	    append redirecting [string map [list %H $host %U $url %T $to] {"%H,%U" { Debug.nub {REDIR %U -> %T}; return [Http Redir $r %T] }}] \n
	}
	
	set switch ""
	foreach {u d} $processed {
	    set url [join [lassign $u host] /]
	    Debug.nub {PROCESS: $u ($d)}
	    dict with d {
		switch -- [string tolower $domain] {
		    literal {
			append switch [string map [list %H $host %U $url %CT [dict get $body ctype] %C [list [dict get $body content]]] {
			    "%H,%U" {Http Ok $r %C %CT}
			}]
		    }
		
		    code {
			append switch [string map [list %H $host %U $url %CT [dict get $body ctype] %C [dict get $body content]] {
			    "%H,%U" {Http Ok $r [%C] %CT}
			}]
		    }
		    default {
			if {![dict exists $domains $name]} {
			    lappend error "Domain $name (referenced in $section) doesn't exist."
			}
			append switch [string map [list %H $host %U $url %N $name] {
			    "%H,%U*" {
				Debug.nub {Dispatch [dict get $r -url] via %H,%U*}
				$defs(%N) do $r
			    }}]
		    }
		}
	    }
	}

	Debug.nub {REWRITING: $rewriting}
	if {$rewriting ne ""} {
	    set rw [string map [list %RW $rewriting] {
		# Rewrites
		set count 0
		set done 0
		set r [dict merge $r [Url parse [dict get $r -url]]]
		while {!$done && [incr count] < 30} {
		    Debug.nub {pre-RW [dict get $r -url]}
		    set prior [Url url $r]
		    switch -regexp -- "[dict get $r -host],[dict get $r -path]" {
			%RW
			default {
			    set url [dict get $r -url]
			    set done 1
			}
		    }
		    Debug.nub {post-RW [Url parse $url]}
		    set r [dict merge $r [Url parse $url]]
		    set post [Url url $r]
		    if {$prior eq $post} break
		    dict set r -url [Url url $r]
		}
	    }]
	} else {
	    set rw ""
	}

	# ASSEMBLE
	set default {default {Http NotFound $r}}
	set p [string map [list %B $blocking %RW $rw %RD $redirecting %DEF $default %D $definitions %S $switch] {
	    proc ::Httpd::do {op r} {
		variable defs
		if {[info exists defs]} {
		    # try to remove old definitions
		    foreach o [array names defs] {
			catch {$o destroy}
			catch {rename $o ""}
		    }
		    unset defs
		}

		# Definitions
		Debug.nub {Creating Defs}
		%D

		# this proc will replace the containing version after one run
		proc ::Httpd::do {op r} {
		    if {$op ne "REQUEST"} return
		    Debug.nub {RX: [dict get $r -uri] - [dict get $r -url] - ([Url parse [dict get $r -url]]) }
		    variable defs

		    # get URL components
		    set r [dict merge $r [Url parse [dict get $r -url]]]

		    %RW

		    # Block
		    switch -glob -- [dict get $r -host],[dict get $r -path] {
			%B { return [Block block [dict get $r -ipaddr] "Blocked by Nub [dict get $r -url]"] }
			default {}
		    }
 
		    # Redirects
		    switch -glob -- [dict get $r -host],[dict get $r -path] {
			%RD
			default {}
		    }

		    Debug.nub {PX: [dict get $r -host],[dict get $r -path]}
		    Debug.dispatch {[dict get $r -url]}
		    # Processing
		    switch -glob -- [dict get $r -host],[dict get $r -path] {
			%S
			%DEF
		    }
		    # nothing should be put here
		}
		return [do $op $r]
	    }
	}]
	Debug.nub {GEN: $p}
	return $p
    }

    proc sect2dict {sect} {
	set result {}
	foreach {n v} $sect {
	    set v [join $v]
	    set v [string trim $v \"]
	    dict set result $n $v
	}
	#puts stderr "Sect2dict: ($sect) -> ($result)"
	return $result
    }

    variable urls {}

    proc parseurl {url} {
	if {$url eq "default"} {
	    set url //*/*
	}
	set parsed [Url parse $url]
	switch -nocase -glob -- $url {
	    http://* -
	    //* {
		# absolute URL - specifies hosts
		set key [dict get $parsed -host]
		lappend key {*}[split [dict get $parsed -path] /]
		return $key
	    }
	    
	    /* {
		# relative URL - across all hosts
		set key *
		lappend key {*}[split [dict get $parsed -path] /]
		return $key
	    }
	    default {
		error "$url is not a valid url"
	    }
	}
    }

    proc literal {url content {ctype x-text/html-fragment}} {
	variable urls
	dict set urls [parseurl $url] [list domain Literal body [list content $content ctype $ctype] section $url]
    }
    
    proc code {url content {ctype x-text/html-fragment}} {
	variable urls
	dict set urls [parseurl $url] [list domain Code body [list content $content ctype $ctype] section $url]
    }

    proc rewrite {url to} {
	variable urls
	dict set urls [parseurl $url] [list domain Rewrite body $to section $url]
    }

    proc block {url} {
	variable urls
	dict set urls [parseurl $url] [list domain Block section $url]
    }

    proc redirect {url to} {
	variable urls
	dict set urls [parseurl $url] [list domain Redirect body $to section $url]
    }

    proc domain {url domain args} {
	variable urls
	dict set urls [parseurl $url] [list domain $domain body $args section $url]
    }

    proc process {file} {
	source $file
    }

    set nub {
	redirect / /wub/	;# site default is Wub
	redirect /favico.ico /images/favico.ico	;# find the icon in /images

	# important to have a robots.txt
	literal /robots.txt "User-agent: *\nDisallow: /" text/plain

	# main wub documentation and nub configuration
	domain /wub/ {Mason wub} auth .before wrapper .after root $::Site::docroot
	domain /nub/ Nub
	domain /jquery/ JQ
	domain /session/ Session	;# session manipulation interface

	# Useful static content directories
	domain /icons/ Icons
	domain /css/ {File css} root [file join $::Site::docroot css] expires tomorrow
	domain /images/ {File images} root [file join $::Site::docroot images] expires "next week"
	domain /scripts/ {File scripts} root [file join $::Site::docroot scripts] expires tomorrow
	domain /img/ {File img} root [file join $::Site::docroot img] expires "next week"
	domain /html/ {File images} root [file join $::Site::docroot html]
	domain /bin/ {File bin} root [file join $::Site::docroot bin]
    }

    proc config {{config ""}} {
	# run the config
	if {$config eq ""} {
	    variable nub
	    set config $nub
	}
	eval $config
    }

    variable nubdirSys [file join [file dirname [info script]] nubs]
    variable nubdir
    proc configF {file} {
	variable nubdirSys
	variable nubdir
	if {[file pathtype $file] eq "relative"} {
	    if {[info exists nubdir]
		&& $nubdir ne ""
		&& [file exists [file join $nubdir $file]]
	    } {
		set f [file join $nubdir $file]
	    } elseif {[file exists [file join $nubdirSys $file]]} {
		set f [file join $nubdirSys $file]
	    }
	} else {
	    set f $file
	}
	if {[file exists $f]} {
	    Debug.nub {configF $f}
	    variable loaded
	    lappend loaded $file
	    return [config [::fileutil::cat $f]]
	} else {
	    error "Nub File $file can't be found"
	}
    }

    proc apply {{urls ""}} {
	if {$urls eq ""} {
	    set urls $::Nub::urls
	}
	set do [Nub generate $urls]
	Debug.nub {DO: $do}
	#puts stderr "DO: $do"
	eval $do
    }

    proc init {args} {
	Debug.nub {construct $args}
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	
	foreach {n v} $args {
	    variable [string trimleft $n -] $v
	}
    }

    proc new {args} {
	init {*}$args
	set cmd [Direct new namespace ::Nub {*}$args ctype "x-text/html-fragment"]
	Debug.nub {new: $cmd}
	return $cmd
    }

    proc create {name args} {
	init {*}$args
	set cmd [Direct create $name namespace ::Nub {*}$args ctype "x-text/html-fragment"]
	Debug.nub {new: $cmd}
	return $cmd
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    set dir [file dirname [info script]]
    package require fileutil
    namespace eval Site {
	variable docroot DOCROOT
    }
    
    Nub config
}
