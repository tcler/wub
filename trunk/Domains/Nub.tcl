# Nub - a domain for configuration of Wub

if {[info exists argv0] && ($argv0 eq [info script])} {
    lappend auto_path ../Utilities ../extensions .
}

package require Debug
Debug off nub 10

package require fileutil
package require textutil
package require functional
package require Url
package require Direct

package require stx2html
package require Html
package require Form
package require jQ
package provide Nub 1.0
package provide Rewrite 1.0	;# to satisfy synthetic requirement of codegen

set API(Domains/Nub) {
    {
	The Nub module controls the dispatch function of Wub.  Nub allows you to configure server's response to requests by defining a mapping from request-URLs to [http:. Domain] handlers.

	The dispatch mapping is defined by a collection of nubs, where a nub is a mapping from a URL glob to content or to a [http:. Domain] handler, listed below.  Nubs may be collected together in configuration files, or updated online via the Nub domain.

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
    docurl {url prefix for domain docs. (default /wub/docs/)}
}

namespace eval Nub {
    proc stxify {about} {
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

    variable docurl /wub/docs/	;# this interfaces to the Mason domain /wub, if it's there

    variable whatsthis [::textutil::undent [::textutil::untabify {
	Nub allows you to change the URLs the server responds to, and to create new nubs, edit or delete existing nubs, and apply them to the currently running server.
	
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

    proc options {domain body} {
	upvar count count
	global API
	if {![info exists API(Domains/$domain)]} {
	    return ""
	}

	set opts [lassign $API(Domains/$domain) about]
	if {[llength $opts]} {
	    foreach {opt text} $opts {
		set val [tclarmour [armour [dict get? $body $opt]]]
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

	if {[string index $about 0] eq "\n"} {
	    set about [string trim $about "\n"]
	}
	set about [lindex [split $about] 0]
	variable docurl
	append extra [<p> [tclarmour "[<a> href [file join $docurl $domain] $domain] domain: $about"]]

	return $extra
    }

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

    # construct an interaction form for each nub
    proc donub {urls key count} {
	set script [dict get $urls $key]
	set extra ""
	set disable 0
	dict with script {
	    lassign $domain domain name
	    
	    switch -- [string tolower $domain] {
		redirect {
		    set extra [Form <text> to_$count class autogrow size 80 label "To:" [tclarmour $body]]
		    append extra [<p> "Redirect $section URL to the $body URL"]
		} 
		rewrite {
		    set extra [Form <textarea> to_$count class autogrow cols 80 label "To:" [tclarmour $body]]
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
		    catch {package require $domain}
		    set extra [<legend> "$domain parameters"]\n
		    append extra [options $domain $body]
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
		    [<br>][tclarmour $extra]
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

    proc /css {r} {
	set css "* {zoom: 1.0;}"
	return [Http Ok $r $css text/css]
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
			set urls $nkey [dict merge [dict get $urls $keymap($el)] [list section [non_auth $url] auth [auth_part $url]]]
			set keymap($el) $nkey
			set section [non_auth $url]
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
	set domnames {}
	foreach n [array names API Domains/*] {
	    lappend domnames [file tail $n]
	}
	set selection [lsort -dictionary [list Rewrite Block Redirect {*}$domnames Literal Code]]
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

	variable whatsthis; variable docurl
	append header [<a> id toggle href # "What's this? ..."]

	set huh $whatsthis
	foreach {n v} [array get ::API Domains/*] {
	    append huh ";\[[file join $docurl $n] [file tail $n]\]: "
	    set v [lindex $v 0]

	    if {[string index $v 0] eq "\n"} {
		set v [string trim $v "\n"]
	    }
	    append huh [lindex [split $v \n] 0] \n
	}
	set huh [stxify $huh]

	append header \n [<div> id box style {display:none} $huh] \n
	append header [<hr>]

	set content "$header\n$content"

	if {$error ne ""} {
	    append content [<h3> "Result of $etitle:"]
	    append content [<p> class message [join $error "</p><p class='message'>"]]
	    append content [<hr>]
	}

	# order urls by key length - longest first
	set ordered [lsort -command urlorder [dict keys $urls]]
	Debug.nub {ORDERED $ordered}

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
	set l1 [split $k1 /]
	set l2 [split $k2 /]
	set diff [expr {[llength $l2] - [llength $l1]}]
	if {$diff != 0} {
	    #Debug.nub {urlorder '$k1'=[llength $l1] '$k2'=[llength $l2] -> $diff}
	    return $diff
	}

	# make wildcards come later in the order for the same length
	if {[string map {* ~~} $k1] >= [string map {* ~~} $k2]} {
	    #Debug.nub {urlorder '$k1' '$k2' -> 1}
	    return 1
	} else {
	    #Debug.nub {urlorder '$k1' '$k2' -> -1}
	    return -1
	}
    }

    proc outerr {name1 name2 op} {
	upvar $name1 error
	puts stderr "ERROR: $error"
    }

    proc armr {str} {
	return [string map [list \" \\\"] $str]
    }

    # this is called at runtime when a domain constructor fails
    proc failed {domain e eo} {
	return [string map [list %N $domain %EM [Nub armr $e] %EO $eo] [lambda {do r} {Http ServerError $r "Nub failed to construct domain '%N' because '%EM' upon construction" [list %EO]}]]
    }


    # generate constructors for each domain
    # each definition is a named sub-dictionary
    # we generate catch-wrapped code for each domain definition
    # to set a runtime defs() with the value of the constructor
    # rewrites are a special case.
    # the result of this code is emitted into "definitions"
    proc gen_definitions {domains} {
	upvar 1 defs defs	;# for accumulating definitions

	set definitions ""
	dict for {n d} $domains {
	    Debug.nub {DEFINING: $n $d}
	    set body [dict get $d body]; dict unset d body
	    
	    if {[string match _rewrite* $n]} {
		# this is a rewrite definition - generate code to do the rewrite
		if {[lindex $body 0] eq "-regsub"} {
		    lassign $body -> subspec
		    set url [dict get $d url]

		    set rwtemplate {
			if {[catch {set defs(%N) {::apply {r {return [regsub -- {%URL%} //[dict get $r -host]/[dict get $r -path] %SS%]}}}} e eo]} {
			    Debug.error {Nub Definition Error: '$e' in rewrite "-regsub %URL% -> %SS%".  ($eo)}
			    set defs(%N) [Nub failed "%N -regsub %URL%->%SS%" $e $eo]
			}
		    }
		    append definitions [string trim [string map [list %N $n %URL% $url %SS% $subspec] $rwtemplate] \n] \n
		} else {
		    set script [lindex $body 0]
		    set rwtemplate {
			if {[catch {set defs(%N) {::apply {r {return "%L"}}}} e eo]} {
			    Debug.error {Nub Definition Error: '$e' in rewrite "lambda r {%L}".  ($eo)}
			    set defs(%N) [Nub failed %N $e $eo]
			}
		    }
		    append definitions [string trim [string map [list %N $n %L $script] $rwtemplate] \n] \n
		}
		continue
	    }
	    
	    # handle domain package require
	    set domain [dict get $d domain]; dict unset d domain
	    if {![info exists defined($domain)]} {
		# emit a single "package require" per domain.
		incr defined($domain)
		append definitions "package require $domain" \n
	    }

	    # generate code to construct the domain
	    if {[string match _anonymous* $n]} {
		# anonymous domain definition
		append definitions [string trim [string map [list %N $n %D $domain %A $body] {
		    if {[catch {set defs(%N) [%D new %A]} e eo]} {
			Debug.error {Nub Definition Error: '$e' in anonymous "%D new %A".  ($eo)}
			set defs(%N) [Nub failed %N $e $eo]
		    }
		}] \n] \n
	    } else {
		# named domain definition
		append definitions [string trim [string map [list %N $n %D $domain %A $body] {
		    if {[catch {set defs(%N) [%D create %N %A]} e eo]} {
			Debug.error {Nub Definition Error: '$e' in running "%D create %N %A".  ($eo)}
			set defs(%N) [Nub failed %N $e $eo]
		    }
		}] \n] \n
	    }
	}

	Debug.nub {DEFINED: $definitions}
	return $definitions
    }
    
    # generate rewritingcode
    proc gen_rewrites {rewrites} {
	set rewriting ""
	foreach {url name} $rewrites {
	    append rewriting [string map [list %URL% $url %N% $name] {{%URL%} {set url [{*}$defs(%N%) $r]}}] \n
	}
	return $rewriting
    }
    
    # generate redirect code
    proc gen_redirects {redirects} {
	set redirecting ""
	foreach {from to} $redirects {
	    set url [join [lassign $from host] /]
	    append redirecting [string map [list %H% $host %U% $url %T% $to] {
		"%H%,%U%" { Debug.nub {REDIR %U% -> %T%}; return [Http Redir $r %T%] }
	    }] \n
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
    proc gen_blocking {blocking} {
	if {[llength $blocking]} {
	    set blocking [string map [list %B% [join $blocking " -\n"]] {
		switch -glob -- [dict get $r -host],[dict get $r -path] {
		    %B% { return [Block block [dict get $r -ipaddr] "Blocked by Nub [dict get $r -url] ([dict get? $r user-agent])"] }
		    default {}
		}
	    }]
	}
	return $blocking
    }

    variable redirect_dirs 1	;# do we emit redirection for non-/ dir refs?

    # reprocess domain:
    # 1) substituting referenced domain defs
    # 2) ensuring the content has a name
    # we then reconstruct the processed contents
    proc accum_domains {key dom} {
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
	    if {[info exists domains $domain] && [llength $dargs]} {
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
		    set rkey [parseurl [string trimright $section /]]
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
	    auth $key [dict get $dom auth]
	}
    }

    proc code_auths {auths} {
	if {![dict size $auths]} {return ""}

	dict for {k a} $auths {
	    set url [join [lassign $k host] /]
	    append code [string map [list %H% $host %U% $url %A% $a] {
		"%H%,%U%" {
		    Httpd Auth $r "%A%"
		}
	    }] \n
	}

	return [string map [list %C% $code] {
	    set switch -glob -- [dict get $r -host],[dict get $r -path] {
		%C%
	    }
	}]
    }

    # code processed domains into a big switch
    proc code_domains {processed} {
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

    # generate code for rewriting
    proc code_rewrites {rewriting} {
	Debug.nub {code_rewrites: $rewriting}
	if {$rewriting eq ""} {
	    return ""
	}

	return [string map [list %RW $rewriting] {
	    # Rewrites
	    set count 0
	    set done 0
	    set r [dict merge $r [Url parse [dict get $r -url]]]
	    while {!$done && [incr count] < 30} {
		Debug.nub {pre-RW [dict get $r -url]}
		set prior [Url url $r]
		switch -regexp -- "//[dict get $r -host]/[dict get $r -path]" {
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
    }

    proc generate {urls {domains {}} {defaults {}}} {
	upvar error error

	#trace add variable error {write} outerr

	# order urls by key length - longest first
	set ordered [lsort -command urlorder [dict keys $urls]]

	Debug.nub {URLs in order $ordered}
	Debug.nub {URLs: $urls}

	# ordered set of nubs is sorted into one of these categories
	foreach cat {processed rewrites redirects blocking auths definitions} {
	    set $cat {}
	}

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
		    variable uniq
		    set name _rewrite[incr uniq]	;# make up a name for rewrite
		    dict set rewrites $key $name

		    # record this rewrite as a domain so we can generate the code
		    # defining the rewrite operation
		    dict set domains $name [list domain $domain body [dict get $urls $key body] url $section]
		    continue
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
		    accum_domains $key [dict get $urls $key]
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
	set blocking [gen_blocking $blocking]
	set definitions [gen_definitions $domains]	;# mods defs()
	set rewriting [gen_rewrites $rewrites]
	set redirecting [gen_redirects $redirects]
	set switch [code_domains $processed]
	set rw [code_rewrites $rewriting]
	set au [code_auths $auths]

	# ASSEMBLE generated code
	# the code becomes a self-modifying proc within ::Httpd
	# its function is to dispatch on URL
	set p [string map [list %B% $blocking %RW% $rw %RD% $redirecting %D% $definitions %S $switch %AUTH% $au] {
	    proc ::Httpd::do {op r} {
		# this code generates definitions once, when invoked
		# it then rewrites itself with code to use those definitions
		variable defs
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

		# this proc will replace the containing version after first run
		proc ::Httpd::do {op r} {
		    Debug.nub {RX: [dict get? $r -uri] - [dict get? $r -url] - ([Url parse [dict get? $r -url]]) }
		    variable defs	;# functional definitions

		    # get URL components to be used in URL switches
		    set r [dict merge $r [Url parse [dict get $r -url]]]

		    %RW%	;# apply rewrite rules

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
			%S
			default {
			    # this is the default behaviour
			    Http NotFound $r [<p> "page '[dict get $r -uri]' Not Found."]
			}
		    }
		    # nothing should be put here, as the above switch returns values
		}

		return [do $op $r]	;# call replacement [do] on first invocation
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

    proc rewrite {url args} {
	variable urls
	dict set urls $url [list domain Rewrite body $args section $url]
    }

    proc block {url} {
	variable urls
	dict set urls [parseurl $url] [list domain Block section $url]
    }

    proc redirect {url to} {
	variable urls
	dict set urls [parseurl $url] [list domain Redirect body $to section $url]
    }

    proc auth_part {url} {
	set auth ""
	set auth [join [lassign [split $url "#"] body]]
	return $auth
    }
    proc non_auth {url} {
	set url ""
	lassign [split $url "#"] url
	return $url
    }

    variable auths {}
    variable realms {}
    proc auth {url realm} {
	if {$realm eq ""} return

	variable auths
	variable realms
	set purl [parseurl $url]
	dict set auths $purl $realm
	dict lappend realms $realm $purl
    }

    proc literal {url content {ctype x-text/html-fragment}} {
	variable urls
	dict set urls [parseurl $url] [list domain Literal body [list content $content ctype $ctype] section [non_auth $url]]
	auth [parseurl $url] [auth_part $url]
    }

    proc code {url content {ctype x-text/html-fragment}} {
	variable urls
	dict set urls [parseurl $url] [list domain Code body [list content $content ctype $ctype] section [non_auth $url]]
	auth [parseurl $url] [auth_part $url]
    }

    proc domain {url domain args} {
	variable urls
	dict set urls [parseurl $url] [list domain $domain body $args section [non_auth $url]]
	auth [parseurl $url] [auth_part $url]
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
    variable nubdir ""
    proc configF {file} {
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
	    return [config [::fileutil::cat $f]]
	} else {
	    error "Nub File $file can't be found"
	}
    }

    proc apply {{urls ""}} {
	if {$urls eq ""} {
	    set urls $::Nub::urls
	}
	set do [generate $urls]
	Debug.nub {apply: $do}
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