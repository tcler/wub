# Nub - a domain for configuration of Wub

if {[info exists argv0] && ($argv0 eq [info script])} {
    lappend auto_path ../Utilities ../extensions .
}

package require Debug
Debug define nub 10

package require fileutil
package require textutil
package require functional
package require Url
package require Direct

package provide Nub 1.0
package provide Rewrite 1.0	;# to satisfy synthetic requirement of codegen

set ::API(Domains/Nub) {
    {
	The Nub module controls the dispatch function of Wub.  Nub allows you to configure server's response to requests by defining a mapping from request-URLs to [http:. Domain] handlers.

	Dispatch is controlled by a collection of nubs, which are mappings from a URL glob to content or to a [http:. Domain] handler.  Nubs may be collected together in configuration files, or updated online via the Nub domain.

	Note that the URLs associated with nubs may contain host components.  If they do, the nub will match only requests directed to that host.  This is how 'virtual domains' can be implemented.  The host component (and the path component) may also contain * as wildcards.

	== Defining Nubs ==
	Nubs may be defined with the [[Nub domain]] command in the following form:

	;[[Nub domain ''url'' ''domain'' ''args...'']]: where ''url'' is the url prefix which will be handled by this nub, ''domain'' is the handler for this nub, and ''args'' are the constructor arguments for the domain.  [http:. Domains] are modules of code which transform a request dict into a response dict.

	Nubs may be defined by invoking the [[Nub domain]] command directly from tcl, but Nub loads a set of nub files when it is constructed, and also supports a Web interface for definition, saving and editing of nubs.

	== Synthetic Nubs ==
	The following synthetic nubs are available to pre-filter or manipulate the URL request, or to provide immediate content:

	;[http:../Server/Block Block]: instructs the server to place an IP address which attempts to access a matching URL onto a block list, preventing it from accessing the server in future.
	;<url> redirect <to> : Sends the client a redirect which causes it to attempt to load the specified 'to' URL.
	;<url> code <script> ?<content-type>?: Returns the result of evaluating the supplied tcl script in the Nub namespace.
	;<url> literal <literal> ?<content-type>?: Returns specified literal content to client.
	;<url> rewrite <script> or <url> rewrite -regsub <subspec>: Transforms a URL (selected by regexp) into another (as calculate by the rewrite tcl script, which is evaluated in the Nub namespace).  The rewrite script has access to all fields of the request, as elements of the dict variable named 'r'.  If the -regsub form is used, then <url> and <subspec> function as the exp and subspec elements of tcl's [[regsub]] command, whose result is returned.

	== Nub as a Domain handler ==
	Nub is itself a domain handler which presents a web interface through which a (suitably credentialed) user can create new nubs, edit or delete existing nubs, and apply nubs to the currently running server.  The Nub domain itself may be mapped into the URL space by: [[Nub domain /nub/ Nub]].
    }
    nubdir {directory for user-defined nubs}
    theme {jQuery theme for Nub web interaction}
    password {password for modifying nubs.}
    docurl {url prefix for domain docs. (default /wub/docs/Domains)}
}

oo::class create ::NubClass {
    method stxify {about} {
	set about [string trim $about "\n"]
	set about [::textutil::untabify $about]
	set about [::textutil::undent $about]

	if {[catch {
	    stx2html::translate $about
	} result eo]} {
	    puts stderr "Nub About Err: $result ($eo)"
	    return $about
	} else {
	    return $result
	}
    }

    # create Nub options from the API documentation
    method options {domain body} {
	upvar count count
	global API
	if {![info exists API(Domains/$domain)]} {
	    return ""
	}

	set opts [lassign $API(Domains/$domain) about]
	set extra ""
	if {[llength $opts]} {
	    foreach {opt text} $opts {
		set val [tclarmour [armour [dict get? $body $opt]]]
		set text [tclarmour [armour $text]]
		if {[string match +* $text]} {
		    append extra [<br>] [<textarea> ${opt}_$count cols 60 class autogrow label "[string totitle $opt]: " title $text $val] \n
		} else {
		    append extra [<br>] [<text> ${opt}_$count label "[string totitle $opt]: " title $text $val] \n
		}
	    }
	} else {
	    set extra ""
	}

	return $extra
    }

    # construct an interaction form for each nub
    method donub {urls key count} {
	set script [dict get $urls $key]
	set extra ""
	set disable 0
	set preamble ""

	dict with script {
	    # form URL for nub
	    set path ""
	    set path [join [lassign $key host] /]	;# get host/path
	    if {$host ni {"" *}} {
		set url ${host}/$path
	    } else {
		set url $path
	    }

	    lassign $domain domain name

	    switch -- [string tolower $domain] {
		redirect {
		    set extra [Form <text> to_$count class autogrow size 40 label "To:" [tclarmour $body]]
		    set preamble [<p> "Redirect $section URL to the $body URL"]
		}

		rewrite {
		    set extra [Form <textarea> to_$count class autogrow cols 60 label "To:" [tclarmour $body]]
		    set preamble [<p> "Rewrite $section URL to $body"]
		}

		regsub {
		    set extra [Form <textarea> to_$count class autogrow cols 60 label "To:" [tclarmour $body]]
		    set preamble [<p> "Regsub $section URL to $body"]
		}

		block {
		    set extra [Form <checkbox> block_$count label "Block?" checked 1 value 1]
		    set preamble [<p> "Block any client which accesses $section from this server."]
		    set disable 1
		}

		literal {
		    set section $url
		    dict with body {
			append extra [<br>]
			append extra [Form <textarea> content_$count class autogrow cols 60 label "Content: " [tclarmour [armour $content]]]
			append extra [Form <text> ctype_$count label "Mime Type: " [tclarmour $ctype]]
		    }
		    set preamble [<p> "Return the literal content of the given mime type."]
		}

		code {
		    set section $url
		    dict with body {
			append extra [<br>]
			append extra [Form <textarea> content_$count class autogrow cols 60 label "Content: " [tclarmour [armour $content]]]
			append extra [Form <text> ctype_$count label "Mime Type: " [armour $ctype]]
		    }
		    set preamble [<p> "Return the result of evaluating content as a Tcl expression, with the given mime type."]
		}

		default {
		    if {[catch {package require $domain} e eo]} {
			Debug.error {Couldn't load package '$domain': $e ($eo)}
		    }
		    set section $url
		    append extra [my options $domain $body]

		    global API
		    if {[info exists API(Domains/$domain)]} {
			lassign $API(Domains/$domain) about

			if {[string index $about 0] eq "\n"} {
			    set about [string trim $about "\n"]
			}
			set about [lindex [split $about] 0]
			variable docurl
			set preamble [<p> [tclarmour "[<a> href [file join $docurl $domain] $domain] domain: $about"]]
		    }
		}
	    }

	    set form [<form> f$count action nubs {
		$preamble
		[<fieldset> {
		    [<legend> [tclarmour "$domain $section parameters"]]
		    [<text> url_$count disable $disable label "Url: " [tclarmour $url]]
		    [tclarmour $extra]
		    [<hidden> domain_$count $domain]
		    [<hidden> el $count]
		    [<submit> submit title "Change this Nub" style {float:right} value edit Change]
		    [<submit> submit title "Delete this Nub" style {float:right} value delete Delete]
		    [<reset> reset title "Reset" style {float:right} Reset]
		}]
	    }]\n
	}

	return [list $domain $section $form]
    }

    # urls - accessor for urls var
    method urls {} {
        variable urls
        return $urls
    }

    method donubStyle {ordered style} {
	variable urls
	upvar count count
	set acc {}
	foreach key $ordered {
	    if {[dict get $urls $key domain] ne $style} continue
	    set keymap([incr count]) $key
	    lappend acc {*}[my donub $urls $key $count]
	}
	return $acc
    }

    method /css {r} {
	variable css
	return [Http Ok $r $css text/css]
    }

    # display all nubs
    method all {r {etitle ""} {error ""}} {
	variable theme
	dict lappend r -headers [<stylesheet> css]
	set r [jQ theme $r $theme]

	# construct New Nub section
	global API
	set domnames {}
	foreach n [array names API Domains/*] {
	    lappend domnames [file tail $n]
	}
	set selection [lsort -dictionary [list Rewrite Regsub Block Redirect {*}$domnames Literal Code]]
	append content [<form> new action add style {width:99%;float:left;} {
	    [<fieldset> {
		[<legend> "New Nub"]
		[<selectlist> domain_new label "Type: " $selection]
		[<text> path_new label "Url: " ""]
		[<submit> submit value add Add]
		[<p> "Create a new nub with the given type for the given path and host."]
	    }]
	}]

	# construct Apply Nubs section
	append content [<form> compile action apply style {width:49%; float:left;} {
	    [<fieldset> {
		[<legend> "Apply Nubs"]
		[<submit> submit style {float:right} value apply Apply]
		[<p> "Compile nubs and attempt to reconfigure the server."]
	    }]
	}]

	# construct Load Nubs section
	variable nubdirSys; variable nubdir
	set selection {}
	if {[info exists nubdir] && $nubdir ne ""} {
	    lappend selection {*}[glob -nocomplain -tails -directory $nubdir *.nub]
	}
	lappend selection {*}[glob -nocomplain -tails -directory $nubdirSys *.nub]
	set selection [lsort -dictionary $selection]
	append content [<form> load action load style {width:49%; float:right;} {
	    [<fieldset> {
		[<legend> "Load Nubs"]
		[<selectlist> load_file label "File: " $selection]
		[<submit> submit value load Load]
		[<p> "Load nub file."]
	    }]
	}]
	append content [<br> clear both]

	# construct Save Nubs section
	append content [<form> save action save style {width:99%; float:left;} {
	    [<fieldset> {
		[<legend> "Save Nubs"]
		[<text> save_file label "Name: " ""]
		[<submit> submit disabled [expr {![info exists nubdir]}] value save Save]
		[expr {[info exists nubdir]?[<p> "Save nubs into a file in the nubdir directory, which may be then loaded to configure the server."]:[<p> "Can't save nubs until the Nub nubdir directory has been specified in site.ini"]}]
	    }]
	}]
	append content [<br> clear both][<hr>]

	# header tells us whence the nubs were loaded
	variable loaded
	set header [<h3> "Nubs from $loaded"]

	append header [<hr>]

	set content "$header\n$content"

	# construct error section
	if {$error ne ""} {
	    append content [<h3> "Result of $etitle:"]
	    append content [<p> class message [join $error "</p><p class='message'>"]]
	    append content [<hr>]
	}

	# construct Nub Bodies section - an accordion of nubs

	# order urls by key length - longest first
	variable urls
	set ordered [lsort -command {::Url order} [dict keys $urls]]
	Debug.nub {ORDERED $ordered}

	set count 0
	set nubs {}
	set acc {}

	foreach style {Rewrite Regsub Block Redirect} {
	    foreach {h s f} [my donubStyle $ordered $style] {
		lappend acc [<h3> [<a> href # "$h $s"]] $f
	    }
	}

	foreach key $ordered {
	    if {[dict get $urls $key domain] in {Block Rewrite Regsub Redirect}} continue
	    set keymap([incr count]) $key
	    lassign [my donub $urls $key $count] h s f
	    lappend acc [<h3> [<a> href # "$h $s"]] $f
	}

	append content [<br>]
	append content [<div> class accordion [join $acc \n]]
	append content [<hr>]

	set r [jQ accordion $r .accordion active false alwaysOpen false clearStyle true autoHeight false fillSpace false]
	set r [jQ autogrow $r .autogrow]
	return [Http Ok $r $content]
    }

    method parseurl {url} {
	if {$url eq "default"} {
	    set url //*/*
	}
	set parsed [Url parse $url]
	lassign [split [dict get $parsed -path] "#"] path	;# remove #-part
	switch -nocase -glob -- $url {
	    http://* -
	    https://* -
	    //* {
		# absolute URL - specifies hosts
		set key [dict get $parsed -host]
		lappend key {*}$path
		return $key
	    }

	    /* {
		# relative URL - across all hosts
		set key *
		lappend key {*}$path
		return $key
	    }

	    default {
		error "$url is not a valid url"
	    }
	}
    }

    method auth_part {url} {
	set auth ""
	set auth [join [lassign [split $url "#"] body]]
	return $auth
    }

    method non_auth {url} {
	set url ""
	lassign [split $url "#"] url
	return $url
    }

    method auth {url realm} {
	if {$realm eq ""} return

	variable auths
	variable realms
	set purl [my parseurl $url]
	dict set auths $purl $realm
	dict lappend realms $realm $purl
    }

    method credentials {r} {
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

    method /nubs/edit {r args} {
	if {[my credentials $r] ne ""} {
	    set challenge "Nub Modification"
	    return [Http Unauthorized $r [Http BasicAuth $challenge]]
	}

	# edit this element
	variable keymap
	variable urls
	set el [dict get $args el]
	set url [dict get $args url_$el]; dict unset args url_$el
	set section [dict get $urls $keymap($el) section]

	if {$url ne $section} {
	    # they've changed the url - copy-edit its content
	    set nkey [my parseurl $url]
	    set urls $nkey [dict merge [dict get $urls $keymap($el)] [list section [my non_auth $url] auth [my auth_part $url]]]
	    set keymap($el) $nkey
	    set section [my non_auth $url]
	}

	set domain [dict get $args domain_$el]; dict unset args domain_$el
	switch -- $domain {
	    Redirect -
            Regsub -
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
		if {[info exists API(Domains/$domain)]} {
		    set opts [lassign $API(Domains/$domain) about]

		    foreach {opt text} $opts {
			if {[dict exists $args ${opt}_$el]} {
			    dict set urls $keymap($el) body $opt [dict get $args ${opt}_$el]
			}
		    }
		}
	    }
	}

	return [my all $r Editing]
    }

    method /nubs/delete {r args} {
	if {[my credentials $r] ne ""} {
	    set challenge "Nub Modification"
	    return [Http Unauthorized $r [Http BasicAuth $challenge]]
	}

	# delete this element
	variable keymap
	variable urls
	dict unset urls $keymap([dict get $args el])

	return [my all $r Deleting]
    }

    method /nubs/add {r args} {
	if {[my credentials $r] ne ""} {
	    set challenge "Nub Modification"
	    return [Http Unauthorized $r [Http BasicAuth $challenge]]
	}

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
	set key [my parseurl $section]

	variable urls
	dict set urls $key [list domain $domain section $section body {}]

	return [my all $r Adding]
    }

    method /nubs/load {r args} {
	if {[my credentials $r] ne ""} {
	    set challenge "Nub Modification"
	    return [Http Unauthorized $r [Http BasicAuth $challenge]]
	}

	if {![catch {configF [dict get $args load_file]} error]} {
	    set error ""
	}
	return [my all $r Loading $error]
    }

    method /nubs/save {r args} {
	if {[my credentials $r] ne ""} {
	    set challenge "Nub Modification"
	    return [Http Unauthorized $r [Http BasicAuth $challenge]]
	}

	variable urls
	generate $urls

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
                        Regsub -
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

	return [my all $r Saving $error]
    }

    method /nubs/apply {r args} {
	if {[my credentials $r] ne ""} {
	    set challenge "Nub Modification"
	    return [Http Unauthorized $r [Http BasicAuth $challenge]]
	}

	variable urls
	set do [my generate $urls]
	if {$error eq ""} {
	    eval $do
	}
	return [my all $r Applying]
    }

    method /nubs {r {submit ""} args} {
	Debug.nub {/nubs ($submit) $args}

	switch -- $submit {
	    edit -
	    delete {
		return /$op $r {*}$args
	    }
	}

	if {[lindex $submit 0] ne "" && [my credentials $r] ne ""} {
	    set challenge "Nub Modification"
	    return [Http Unauthorized $r [Http BasicAuth $challenge]]
	}

	return [my all $r]
    }

    method / {r} {
	# Nub documentation
	variable whatsthis; variable docurl
	append huh $whatsthis \n

	foreach {n v} [array get ::API Domains/*] {
	    append huh ";\[[file join $docurl $n] [file tail $n]\]: "
	    set v [lindex $v 0]

	    if {[string index $v 0] eq "\n"} {
		set v [string trim $v "\n"]
	    }
	    append huh [lindex [split $v \n] 0] \n
	}
	set huh [my stxify $huh]

	return [Http Ok $r $huh]
    }

    method outerr {name1 name2 op} {
	upvar $name1 error
	puts stderr "ERROR: $error"
    }

    method armr {str} {
	return [string map [list \" \\\"] $str]
    }

    # this is called at runtime when a domain constructor fails
    method failed {domain e eo} {
	return [string map [list %N $domain %EM [::Nub armr $e] %EO $eo] [lambda {do r} {Http ServerError $r "Nub failed to construct domain '%N' because '%EM' upon construction" [list %EO]}]]
    }

    # generate definition of each regsub
    method gen_regsubdefs {regsubs} {
	upvar 1 defs defs	;# for accumulating definitions

	set definitions ""
	dict for {n d} $regsubs {
	    Debug.nub {DEFINING REGSUB: $n $d}
            set map [list %N $n %URL% [dict get $d body regexp] %SS% [dict get $d body subspec] %AURL% [tclarmour $url] %ASS% [tclarmour $subspec]]
            set rw [string map $map {
                if {[catch {
                    set defs(%N) {
                        ::apply {r {
                            return [regsub -- {%URL%} //[dict get $r -host]/[dict get $r -path] %SS%]
                        }}
                    }
                } e eo]} {
                    Debug.error {Nub Definition Error: '$e' in rewrite "-regsub %AURL% -> %ASS%".  ($eo)}
                    set defs(%N) [::Nub failed "%N -regsub %URL%->%SS%" $e $eo]
                }
            }]
            append definitions [string trim $rw \n] \n
        }
        Debug.nub {DEFINED REGSUBS: $definitions}
        return $definitions
    }

    # generate regsub code
    method gen_regsubs {regsubs} {
	variable defs
	set rewriting ""
	foreach {url name} $regsubs {
	    if {[string match ^* $url]
                && ![string match ^http* [string tolower $url]]
            } {
		set url "^http:[string range $url 1 end]"	;# rewrite to contain http prefix
	    }
	    Debug.nub {gen_regsubs url:'$url' name:'$name'}
	    append rewriting [string map [list %URL% $url %N% $name %AURL% [tclarmour $url]] {{%URL%} {
		set url [{*}$defs(%N%) $r]
		Debug.nub {rewrite: '%AURL%' -> '$url' ($defs(%N%))}
		lappend rw_transforms {%URL%} $url
	    }}] \n
	}
	return $rewriting
    }

    # generate definitions for rewrites
    method gen_rewritedefs {rewrites} {
	upvar 1 defs defs	;# for accumulating definitions

	set definitions ""
	dict for {n d} $rewrites {
	    Debug.nub {DEFINING REWRITE: $n $d}
            set script [dict get $d body rewrite]
            set map [list %N $n %L% $script %AL% [tclarmour $script]]
            set rw [string map $map {
                if {[catch {
                    set defs(%N) {
                        ::apply {r {
                            return "%L%"
                        }}
                    }
                } e eo]} {
                    Debug.error {Nub Definition Error: '$e' in rewrite "lambda r {%AL%}".  ($eo)}
                    set defs(%N) [::Nub failed %N $e $eo]
                }
            }]
            append definitions [string trim $rw \n] \n
        }

	Debug.nub {DEFINED REWRITES: $definitions}
	return $definitions
    }

    # generate rewritingcode
    method gen_rewrites {rewrites} {
	variable defs
	set rewriting ""
	foreach {url name} $rewrites {
	    if {[string match ^* $url] && ![string match ^http* [string tolower $url]]} {
		set url "^http:[string range $url 1 end]"	;# rewrite to contain http prefix
	    }
	    Debug.nub {gen_rewrites url:'$url' name:'$name'}
	    append rewriting [string map [list %URL% $url %N% $name %AURL% [tclarmour $url]] {{%URL%} {
		set url [{*}$defs(%N%) $r]
		Debug.nub {rewrite: '%AURL%' -> '$url' ($defs(%N%))}
		lappend rw_transforms {%URL%} $url
	    }}] \n
	}
	return $rewriting
    }


    # generate code for rewriting
    # TODO: this code doesn't include port in comparison - it likely should.
    method code_rewrites {rewriting} {
	Debug.nub {code_rewrites: $rewriting}
	if {$rewriting eq ""} {
	    return ""
	}

        return [string map [list %RW% $rewriting] {
	    # Rewrites
	    set count 0
	    set done 0
	    set rw_transforms {}
	    set r [dict merge $r [Url parse [dict get $r -url]]]
	    while {!$done && [incr count] < 30} {
		set prior [Url url $r]
		Debug.nub {pre-RW '$prior'}
		switch -regexp -- $prior {
		    %RW%
		    default {
			set url [dict get $r -url]
			set done 1
		    }
		}
		Debug.nub {post-RW [Url parse $url] transforms:($rw_transforms)}
		set r [dict merge $r [Url parse $url]]
		set post [Url url $r]
		if {$prior eq $post} break
		dict set r -url [Url url $r]
	    }
	}]
    }

    # generate constructors for each domain
    # each definition is a named sub-dictionary
    # we generate catch-wrapped code for each domain definition
    # to set a runtime defs() with the value of the constructor
    # rewrites and regsubs are a special case.
    # the result of this code is emitted into "definitions"
    method gen_definitions {domains} {
	upvar 1 defs defs	;# for accumulating definitions

	set definitions ""
	dict for {n d} $domains {
	    Debug.nub {DEFINING: $n $d}
	    set body [dict get $d body]; dict unset d body

	    # handle domain package require
	    set domain [dict get $d domain]; dict unset d domain
            variable defined
	    if {![info exists defined($domain)]} {
		# emit a single "package require" per domain.
		incr defined($domain)
		append definitions [string map [list %D% $domain] {
		    if {[catch {package require %D%} e eo]} {
			Debug.error {Couldn't load 'package %D%' - '$e' ($eo)}
			dict set def_errors %D% [list $e $eo]
		    }
		}]
	    }

	    # generate code to construct the domain
	    if {[string match _anonymous* $n]} {
		# anonymous domain definition
		append definitions [string trim [string map [list %N% $n %D% $domain %A% $body %AA% [tclarmour $body]] {
		    if {[catch {set defs(%N%) [%D% new %A%]} e eo]} {
			Debug.error {Nub Definition Error: '$e' in anonymous "%D% new %AA%".  ($eo)}
			dict lappend def_errors %N% [list $e $eo]
			set defs(%N%) [::Nub failed %N% $e $eo]
		    }
		}] \n] \n
	    } else {
		# named domain definition
		append definitions [string trim [string map [list %N% $n %D% $domain %A% $body %AA% [tclarmour $body]] {
		    if {[catch {
			set defs(%N%) [%D% create %N% %A%]
		    } e eo]} {
			Debug.error {Nub Definition Error: '$e' in running "%D% create %N% %AA%".  ($eo)}
			set defs(%N%) [::Nub failed %N% $e $eo]
		    }
		}] \n] \n
	    }
	}

	Debug.nub {DEFINED: $definitions}
	return $definitions
    }

    # generate redirect code
    method gen_redirects {redirects} {
	set redirecting ""
	foreach {from body} $redirects {
            # $path is the from path
            # $host is the from host
	    set path [join [lassign $from host] /]

            # allow the caller to specify which redirect Http method to use.
            if {[dict exists $body method]} {
                set method [dict exists $body method]
            } else {
                set method Redir
            }

            if {![dict exists $body host]
                && ![dict exists $body port]
                && ![dict exists $body path]
            } {
                # this is a simple literal redirection
                set to [dict get? $body redirect]
                Debug.nub {gen_redirects simple host:'$host' path:'$path' to:'$to'}
                append redirecting [string map [list %H% $host %P% $path %T% $to  %M% $method] {
                    "%H%,%P%" {
                        Debug.nub {REDIR %P% -> %T%}
                        return [Http %M% $r %T%]
                    }
                }] \n
            } else {
                # this is a redirection which substitutes url parts
                if {[dict exists $body host]} {
                    # want to substitute the host part
                    set HOST [list dict set r -host [dict get $body host]];
                } else {
                    set HOST ""
                }
                if {[dict exists $body port]} {
                    # want to substitute the port part
                    set PORT [list dict set r -port [dict get $body port]];
                } else {
                    set PORT ""
                }
                if {[dict exists $body path]} {
                    # want to substitute the path part
                    set PATH [list dict set r -path [dict get $body path]];
                } else {
                    set PATH ""
                }

                Debug.nub {gen_redirects compound host:'$host' path:'$path'}
                append redirecting [string map [list %H% $host %P% $path %M% $method %HOST% $HOST %PORT% $PORT %PATH% $PATH] {
                    "%H%,%P%" {
                        Debug.nub {REDIR //%H%%P%}
                        %HOST%%PORT%%PATH%
                        return [Http %M% $r [Url uri $r]]
                    }
                }] \n
            }
        }

	return [string map [list %RD% $redirecting] {
	    # Redirects
	    switch -glob -- [dict get $r -host],[dict get $r -path] {
		%RD%
		default {}
	    }
	}]
    }

    # generate blocking code
    method gen_blocking {blocking} {
	if {[llength $blocking]} {
	    set blocking [string map [list %B% [join $blocking " -\n"]] {
		switch -glob -- [dict get $r -host],[dict get $r -path] {
		    %B% {
			set reason "Blocked by Nub [dict get $r -url] ([dict get? $r user-agent])"
			Block block [dict get $r -ipaddr] $reason
			return [Http Forbidden $r [<p> $reason]]
		    }
		    default {}
		}
	    }]
	}
	return $blocking
    }

    # reprocess domain:
    # 1) substituting referenced domain defs
    # 2) ensuring the content has a name
    # we then reconstruct the processed contents
    method accum_domains {key dom} {
	variable redirect_dirs
	upvar 1 processed processed
	upvar 1 domains domains
	upvar 1 redirects redirects
	upvar 1 error error

	set section [dict get $dom section]	;# original URL

	# get the body part - arguments to the Domain constructor
	set body [string trim [dict get $dom body]]

	# get the auth part, if any
	set auth [dict get? $dom auth]	;# auth if any

	# split domain part into domain identifier and possible name
	set domain [dict get $dom domain]	;# processing element
	set name ""
	lassign $domain domain name	;# get the domain and possibly name

	if {[string index $domain 0]
	    ne [string toupper [string index $domain 0]]
	} {
	    # named domain reference, e.g. [/moop/] domain fred
	    # where fred is not further defined, and not Capitalized
	    # we consider it a reference to a named domain

	    # TODO: this isn't used, or well defined.  Reconsider
	    if {0 && [info exists domains $domain] && [llength $dargs]} {
		# domain references can't add arguments
		lappend error "[dict get $dom section]: Can't specify named domain $domain (defined in [dict get $domains $domain section] with constructor arguments.  Try just domain=$domain"
	    }

	    set name $domain
	    set domain unknown
	    dict set processed $key [list domain $domain name $name section $section auth $auth]
	} else {
	    # Domain definition e.g. [/moop/] domain File fred ..
	    Debug.nub {defining domain: $name}

	    # determine or construct a name for the domain
	    if {$name eq ""} {
		# the Domain constructor doesn't specify a name
		# for the object/domain, so we invent one
		variable uniq
		set name _anonymous[incr uniq]	;# so make up a name
	    }

	    # see if we're defining or merely referencing domain
	    if {[dict exists $domains $domain]} {
		# this named domain already exists
		if {$body ne ""} {
		    # domain references can't add arguments
		    lappend error "$section: can't respecify the arguments to Domain $name"
		    continue
		}
	    } else {
		# Finally: defining a new domain with this element

		# add 'mount' parameter to constructor
		append body " mount [join [lrange $key 1 end] /]"

		# generate dict-/ redirects
		if {$redirect_dirs && [string match */ $section]} {
		    # if the key is a directory, we redirect anything
		    # which doesn't specify the trailing /
		    set rkey [my parseurl [string trimright $section /]]
		    dict set redirects $rkey $section
		}

		# record this domain for possible later reference
		dict set domains $name [list domain $domain body $body]

		# we have now reprocessed the domain definition
		dict set processed $key [list domain $domain name $name section $section auth $auth]
		Debug.nub {accum_domain: $name domain $domain ($body)}
	    }
	}

	# record authentication
	if {[dict get? $dom auth] ne ""} {
	    my auth $key [dict get $dom auth]
	}
    }

    method code_auths {auths} {
	if {![dict size $auths]} {return ""}

	dict for {k a} $auths {
	    set url [join [lassign $k host] /]
	    append code [string map [list %H% $host %U% $url %A% $a] {
		"%H%,%U%" {
		    Http Auth $r "%A%"
		}
	    }] \n
	}

	return [string map [list %C% $code] {
	    set switch -glob -- [dict get $r -host],[dict get $r -path] {
		%C%
	    }
	}]
    }

    method code_trailing {processed} {
	upvar 1 domains domains
	set switch ""
	foreach {u d} $processed {
	    set url [join [lassign $u host] /]
	    Debug.nub {code_trailing: $u ($d)}
	    dict with d {
		lappend switch "$host,$url*"
	    }
	}
	set switch [join $switch " -\n"]
	append switch { {
	    Http Redir $r [dict get $r -path]/
	}}
	append switch {
	    default {
		NotFound $r
	    }
	}

	# set up a selector to specify trailing/
	set selector {"[dict get $r -host],[dict get $r -path]/"}
	catch {unset body}
	append body "Debug.nub {trailing: \[dict get \$r -host],\[dict get \$r -path]/}" \n
	append body "set result \[switch -glob -- $selector [list $switch]\]" \n
	append body "Debug.nub {trailing: \[dict get? \$result -code] \[dict get? \$result location]}" \n
	append body "return \$result" \n

	Debug.nub {code_trailing: ($body)}
	variable NS
	namespace eval $NS [list proc trailing {r} $body]
    }

    # code processed domains into a big switch
    method code_domains {processed} {
	upvar 1 domains domains
	set switch ""
	foreach {u d} $processed {
	    set url [join [lassign $u host] /]
	    Debug.nub {code_domains: $u ($d)}
	    dict with d {
		switch -- [string tolower $domain] {
		    literal {
			# literal nub
			append switch [string map [list %H $host %U $url %CT [dict get $body ctype] %C [list [dict get $body content]]] {
			    "%H,%U" {
				Debug.nub {Literal [dict get $r -url] via %H,%U*}
				Http Ok $r %C %CT
				# TODO: handle if-modified-since etc depending on nub-date
			    }
			}]
		    }

		    code {
			# code nub
			append switch [string map [list %H $host %U $url %CT [dict get $body ctype] %C [dict get $body content]] {
			    "%H,%U" {
				Debug.nub {Code [dict get $r -url] via %H,%U*}

				# construct a query domain for people who want it.
				dict set r -Query [Query parse $r]
				set Query [Query flatten [dict get $r -Query]]

				dict set r -code 200	;# default return code
				dict set r content-type %CT	;# default content-type
				set content [%C]
				Http Pass $r $content	;# pass the content back
			    }
			}]
		    }

		    default {
			# domain nub
			if {![dict exists $domains $name]} {
			    lappend error "Domain $name (referenced in $section) doesn't exist."
			}
			append switch [string map [list %H $host %U $url %N $name] {
			    "%H,%U*" {
				Debug.nub {Dispatch [dict get $r -url] via %H,%U* to cmd '$defs(%N)'}
				{*}$defs(%N) do $r
			    }}]
		    }
		}
	    }
	}
	return $switch
    }
		    variable uniq

    method generate {urls {domains {}} {defaults {}}} {
	upvar error error

	#trace add variable error {write} outerr

	# order urls by key length - longest first
	set ordered [lsort -command {::Url order} [dict keys $urls]]

	Debug.nub {URLs in order $ordered}
	Debug.nub {URLs: $urls}

	# ordered set of nubs is sorted into one of these categories
	foreach cat {processed rewrites regsubs
            rewritedefs regsubdefs
            redirects blocking auths definitions
        } {
	    set $cat {}
	}
        variable uniq

	foreach key $ordered {
	    set section [dict get $urls $key section]	;# original URL
	    set domain [dict get $urls $key domain]	;# processing element

	    # get domain from section and constructor args, if any
	    Debug.nub {processing: $key - $section - $domain}
	    switch -- [string tolower [lindex $domain 0]] {
		redirect {
		    # sort redirects into redirect category
		    dict set redirects $key [dict get $urls $key body]; continue
		}

		rewrite {
		    # sort rewrites into rewrite category
		    set name _rewrite[incr uniq]	;# make up a name for rewrite
		    dict set rewrites $key $name

		    # also record this rewrite as a domain so we can generate
                    # the code defining the rewrite operation
		    dict set rewritedefs $name [list domain $domain body [dict get $urls $key body] url $section]
		}

                regsub {
		    # sort rewrites into rewrite category
		    set name _regsub[incr uniq]	;# make up a name for rewrite
		    dict set regsubs $key $name

		    # also record this rewrite as a domain so we can generate
                    # the code defining the rewrite operation
		    dict set regsubdefs $name [list domain $domain body [dict get $urls $key body] url $section]
                }

		block {
		    # accumulate blocks into a list of patterns to block
		    set url [join [lassign $key host] /]
		    lappend blocking "$host,$url"
		}

		literal {
		    # sort literals into processed category
		    dict set processed $key [dict get $urls $key]; continue
		}

		code {
		    # sort codes into processed category
		    dict set processed $key [dict get $urls $key]; continue
		}

		default {
		    # everything else should be domains
		    # add to processed category
		    my accum_domains $key [dict get $urls $key]
		}
	    }
	}

	Debug.nub {DOMAINS: $domains}
	Debug.nub {PROCESSED: $processed}
	#Debug.nub {REDIRECTS: $redirects}
	Debug.nub {REWRITES: $rewrites}
	#Debug.nub {BLOCK: $blocking}
	#Debug.nub {URLS: $urls}

	# generate code for each of the categories
	# order is important
	set blocking [my gen_blocking $blocking]
	append definitions [my gen_definitions $domains] \n	;# mods defs()

	set rewriting [my gen_rewrites $rewrites]
        append definitions [my gen_rewritedefs $rewritedefs] \n

	set regsubbing [my gen_regsubs $regsubs]
        append definitions [my gen_regsubdefs $regsubdefs] \n

	set redirecting [my gen_redirects $redirects]
	set switch [my code_domains $processed]
	my code_trailing $processed	;# handle trailing/ problem
	set rw [my code_rewrites [dict merge $rewriting $regsubbing]]
	set au [my code_auths $auths]

	# ASSEMBLE generated code
	# the code becomes a self-modifying proc within ::Httpd
	# its function is to dispatch on URL
	variable NS
	set p [string map [list %NS% $NS %B% $blocking %RW% $rw %RD% $redirecting %D% $definitions %S% $switch %AUTH% $au] {
	    proc ::%NS%::nub {} {
		# this code generates definitions once, when invoked
		# it then rewrites itself with code to use those definitions
		variable defs
		variable def_errors {}
		if {[info exists defs]} {
		    # try to remove any old definitions
		    foreach o [array names defs] {
			catch {$o destroy}
			catch {rename $o ""}
		    }
		    unset defs
		}

		# Definitions
		Debug.nub {Creating Defs}
		%D%	;# this code generates definitions into defs()
		return [dict size $def_errors]
	    }

	    # this proc processes requests
	    proc ::%NS%::do {op r} {
		Debug.nub {RX: [dict get? $r -uri] - [dict get? $r -url] - ([Url parse [dict get? $r -url]]) }
		variable defs	;# functional definitions

		# get URL components to be used in URL switches
		set r [dict merge $r [Url parse [dict get $r -url]]]

		%RW%	;# apply rewrites

		# apply Blocks
		%B%

		# apply Redirects
		%RD%

		# apply Auth rules
		%AUTH%

		Debug.nub {PX: [dict get $r -host],[dict get $r -path]}
		Debug.dispatch {[dict get $r -url]}
		# Processing rules
		switch -glob -- [dict get $r -host],[dict get $r -path] {
		    %S%
		    default {
			# this is the default behaviour
			trailing $r
		    }
		}
		# nothing should be put here, as the above switch returns values
	    }
	}]

	namespace eval $NS {
	    ::proc NotFound {r} {
		Http NotFound $r [<p> "page '[dict get $r -uri]' Not Found."]
	    }
	}
	Debug.nub {GEN: $p}
	return $p
    }

    method sect2dict {sect} {
	set result {}
	foreach {n v} $sect {
	    set v [join $v]
	    set v [string trim $v \"]
	    dict set result $n $v
	}
	#puts stderr "Sect2dict: ($sect) -> ($result)"
	return $result
    }

    # rewriter - alternate constructor for rewrite
    method rewriter {url args} {
        if {[llength $args] == 1} {
            set args [lindex $args 0]
        }
	variable urls
	dict set urls $url [list domain Rewrite body $args section $url]
    }

    method rewrite {url body args} {
	#puts stderr "RW: '$url' '$body'"
        if {[llength $args] == 1} {
            set args [lindex $args 0]
        }
	variable urls
	dict set urls $url [list domain Rewrite body [list rewrite $body {*}$args] section $url]
    }

    # regsubber - alternate constructor for regsub
    method regsubber {url args} {
	#puts stderr "REGSUB: '$url' '$body'"
        if {[llength $args] == 1} {
            set args [lindex $args 0]
        }
	variable urls
	dict set urls $url [list domain Regsub body $args section $url]
    }

    method regsub {url regsub args} {
	#puts stderr "REGSUB: '$url' '$body'"
        if {[llength $args] == 1} {
            set args [lindex $args 0]
        }
	variable urls
	dict set urls $url [list domain Regsub body [list regsub $regsub {*}$args] section $url]
    }

    method block {url} {
	variable urls
	dict set urls [my parseurl $url] [list domain Block section $url]
    }

    # redirector - alternate constructor for redirect,
    method redirector {url args} {
        if {[llength $args] == 1} {
            set args [lindex $args 0]
        }
	variable urls
	dict set urls [my parseurl $url] [list domain Redirect body $args section $url]
    }

    method redirect {url to args} {
        if {[llength $args] == 1} {
            set args [lindex $args 0]
        }
	variable urls
	dict set urls [my parseurl $url] [list domain Redirect body [list url $to {*}$args] section $url]
    }

    method literal {url content {ctype x-text/html-fragment}} {
	variable urls
	dict set urls [my parseurl $url] [list domain Literal body [list content $content ctype $ctype] section [my non_auth $url]]
	my auth [my parseurl $url] [my auth_part $url]
    }

    method code {url content {ctype x-text/html-fragment}} {
	variable urls
	dict set urls [my parseurl $url] [list domain Code body [list content $content ctype $ctype] section [my non_auth $url]]
	my auth [my parseurl $url] [my auth_part $url]
    }

    method domain {url domain args} {
	variable urls
	dict set urls [my parseurl $url] [list domain $domain body $args section [my non_auth $url]]
	my auth [my parseurl $url] [my auth_part $url]
    }

    method process {file} {
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

	# Useful static content directories
	domain /icons/ Icons
	domain /css/ {File css} root [file join $::Site::docroot css] expires tomorrow
	domain /images/ {File images} root [file join $::Site::docroot images] expires "next week"
	domain /scripts/ {File scripts} root [file join $::Site::docroot scripts] expires tomorrow
	domain /img/ {File img} root [file join $::Site::docroot img] expires "next week"
	domain /html/ {File images} root [file join $::Site::docroot html]
	domain /bin/ {File bin} root [file join $::Site::docroot bin]
    }

    method config {{config ""}} {
	# run the config
	if {$config eq ""} {
	    variable nub
	    set config $nub
	}
	eval $config
    }

    method configF {file} {
	if {$file eq ""} return
	variable nubdirSys
	variable nubdir
	if {[file pathtype $file] eq "relative"} {
	    if {[info exists nubdir]
		&& $nubdir ne ""
		&& [file exists [file join $nubdir $file]]
	    } {
		set f [file join $nubdir $file]
	    } elseif {[file exists $file]} {
		set f $file
	    } elseif {[file exists [file join $nubdirSys $file]]} {
		set f [file join $nubdirSys $file]
	    } else {
		Debug.error {Can't locate $file in directories nubdir:$nubdir, pwd:[pwd] or $nubdirSys}
		return
	    }
	} else {
	    set f $file
	}

	if {[file exists $f]} {
	    Debug.nub {configF $f}
	    variable loaded
	    lappend loaded $file
	    return [my config [::fileutil::cat $f]]
	} else {
	    error "Nub File $file can't be found"
	}
    }

    method apply {{_urls ""}} {
	if {$_urls eq ""} {
	    variable urls
	    set _urls $urls
	}
	set do [my generate $_urls]
	Debug.nub {apply: $do}
	eval $do
	variable NS
	namespace eval $NS {nub}
    }

    method new {args} {
	return [self]
    }

    superclass Direct
    constructor {args} {
	variable urls {}
	variable nubdirSys [file join [file dirname [info script]] nubs]
	variable nubdir ""
	variable css {* {zoom: 1.0;}}
	variable auths {}
	variable realms {}

	variable loaded {}
	variable theme start
	variable keymap
        variable defined; array set defined {}
	variable docurl /wub/docs/Domains/	;# this interfaces to the Mason domain /wub, if it's there
	variable password ""
	variable mount ""
	variable whatsthis [::textutil::undent [::textutil::untabify {
	    = Nub Domain =
	    Nubs define the mapping between URLs and Domains.

	    The nubs defining this site can be viewed and edited here: [nubs/]

	    Nub itself is a Domain which allows you to change the URLs the server responds to, and to create new nubs, edit or delete existing nubs, and apply them to the currently running server.

	    A nub is a mapping from a URL glob to content.  The content may be provided by domain handlers, listed below.

	    == Synthetic Nubs ==
	    The following synthetic nubs are available to pre-filter or manipulate the URL request, or to provide immediate content:

	    ;Block: Blocks an IP address which attempts to access the URL.
	    ;Redirect: Sends the client a redirect.
	    ;Code: Returns the result of evaluating tcl code.
	    ;Literal: Returns literal content to client.
	    ;Rewrite: Transforms a URL (selected by regexp) into another (as calculate by the rewrite tcl script).

	    == Domain Nubs ==
	    Domains are modules which generate content from URLs. This is the currently available collection of domain 'nubs':
	}]]
	variable redirect_dirs 1	;# do we emit redirection for non-/ dir refs?

	Debug.nub {construct $args}
	variable NS ::Httpd

	variable {*}[Site var? Nub]	;# allow .ini file to modify defaults
	variable {*}$args

	if {$mount ne ""} {
	    package require jQ
	    package require stx2html
	    package require Html
	    package require Form
	    next mount $mount	;# this creates a /nub domain
	    my domain $mount Nub
	}
    }
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    set dir [file dirname [info script]]
    package require fileutil
    namespace eval Site {
	variable docroot DOCROOT
    }

    NubClass create ::Nub
}
