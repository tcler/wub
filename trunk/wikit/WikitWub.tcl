package require Mk4tcl
package require Wikit::Db
package require Wikit::Search
package require File
package require Convert
package require Direct
package require Html
package require fileutil

package require Debug
package require Url
package require struct::queue
package require Http
package require Cookies

package require utf8

package provide WikitWub 1.0

# create a queue of pending work
::struct::queue inQ
variable request ""

# ::Wikit::GetPage {id} - 
# ::Wikit::Expand_HTML {text}
# ::Wikit::pagevars {id args} - assign values to named vars

# ::Wikit::SavePage $id $C $host $name

# LookupPage {name} - find/create a page named $name in db, return its id
# InfoProc {db name} - lookup $name in db,
# returns a list: /$id (with suffix of @ if the page is new), $name, modification $date

namespace eval WikitWub {
    variable htmlhead {<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">}
    variable motd ""
    proc .text/x-system.text/x-html-fragment {rsp} {
	# split out headers
	set headers ""
	set body [split [dict get $rsp -content] \n]
	set start 0
	set headers {}
	foreach line $body {
	    set line [string trim $line]
	    if {$line eq ""} continue
	    if {[string match <* $line]} break

	    # this is a header line
	    incr start
	    set val [lassign [split $line :] tag]
	    dict lappend rsp -headers "<$tag>[string trim [join $val]]</$tag>"
	}

	set content "[join [lrange $body $start end] \n]\n"

	return [dict replace $rsp \
		    -content $content \
		    content-type text/x-html-fragment]
    }
    variable language "en"

    variable head {
	<meta name='robots' content='noindex,nofollow' />
	<style type='text/css' media='all'>@import url(/wikit.css);</style>
    }

    proc .text/x-html-fragment.text/html {rsp} {
	set rspcontent [dict get $rsp -content]

	if {[string match "<!DOCTYPE*" $rspcontent]} {
	    # the content is already fully HTML
	    set content $rspcontent
	} else {
	    variable htmlhead
	    set content "${htmlhead}\n"

	    variable language
	    append content "<html lang='$language'>" \n

	    append content <head> \n
	    if {[dict exists $rsp -headers]} {
		append content [join [dict get $rsp -headers] \n] \n
		dict unset rsp -headers
	    }

	    # add in some wikit-wide headers
	    variable head
	    append content $head

	    append content </head> \n

	    append content <body> \n
	    append content $rspcontent
	    append content </body> \n
	    append content </html> \n
	}

	return [dict replace $rsp \
		    -content $content \
		    -raw 1 \
		    content-type text/html]
    }

    proc /rev {args} {
	upvar 1 response r
	set r [Http NotImplemented $r]
    }

    # Ref - utility proc to generate an <A> from a page id
    proc Ref {url {name "" }} {
	if {$name eq ""} {
	    set page [lindex [file split $url] end]
	    if {[catch {
		set name [mk::get wdb.pages!$page name]
	    }]} {
		set name $page
	    }
	}
	return "<a href='/[string trimleft $url /]'>[armour $name]</a>"
    }

    variable protected
    variable menus
    array set protected {Home 0 About 1 Search 2 Help 3 Changes 4 HoneyPot 7 TOC 6 Init 9}
    foreach {n v} [array get protected] {
	set protected($v) $n
	set menus($v) [Ref $v $n]
    }

    variable maxAge "next month"
    variable cookie "wikit"

    proc /login {{nickname ""} {R ""}} {
	upvar 1 response r
	set r [Http NoCache $r]

	# cleanse nickname
	regsub -all {[^A-Za-z0-0_]} $nickname {} nickname

	if {$nickname eq ""} {
	    # this is a call to /login with no args,
	    # in order to generate the /login page
	    return "<form><fieldset><legend>Login</legend><label for='nickname'>Nickname</label><input type='text' name='nickname'><input type='submit' value='login'></fieldset><input type='hidden' name='R' value='[armour [Http Referer $r]]'></form>"
	}

	if {[dict exists $r -cookies]} {
	    set cdict [dict get $r -cookies]
	} else {
	    set cdict [dict create]
	}
	set dom [dict get $r -host]

	# include an optional expiry age
	variable maxAge
	if {![string is integer -strict $maxAge]} {
	    if {[catch {
		expr {[clock scan $maxAge] - [clock seconds]}
	    } maxAge]} {
		set age {}
	    } else {
		set age [list -max-age $maxAge]
	    }
	}
	
	if {$maxAge} {
	    #set age [list -max-age $maxAge]
	    set then [expr {$maxAge + [clock seconds]}]
	    set age [clock format $then -format "%a, %d-%b-%Y %H:%M:%S GMT" -gmt 1]
	    set age [list -expires $age]
	} else {
	    set age {}
	}

	variable cookie
	#set cdict [Cookies add $cdict -path / -name $cookie -value $nickname {*}$age]
	set cdict [Cookies add $cdict -path / -name $cookie -value $nickname {*}$age]
	dict set r -cookies $cdict
	if {$R eq ""} {
	    set url "http://[dict get $r host]/0"
	    set r [Http Redirect $r $url]
	} else {
	    set r [Http Redirect $r $R]
	}
    }

    proc who {r} {
	variable cookie
	set cdict [dict get $r -cookies]
	set cl [Cookies match $cdict -name $cookie]
	if {[llength $cl] != 1} {
	    return ""
	}
	return [dict get [Cookies fetch $cdict -name $cookie] -value]
    }

    proc invalidate {r url} {
	Debug.wikit {invalidating $url} 3
	::thread::send -async $::thread::parent [list Cache delete http://[dict get $r host]/$url]
    }

    proc /search {S} {
	Debug.wikit {/search: '$S'}
	dict set request -suffix $S
	dict set request -prefix "/$S"
	upvar 1 response r
	set r [WikitWub do $r $S]
    }

    proc /save {N C O save} {
	upvar 1 response r

	if {![string is integer -strict $N]} {
	    set r [Http NotFound $r]
	    return
	}

	set r [Http NoCache $r]
	if {[catch {
	    ::Wikit::pagevars $N name date who
	}]} {
	    set r [Http NotFound $r <h2>$N is not a valid page.</h2>]
	    return
	}
    
	# is the caller logged in?
	set nick [who $r]
	Debug.wikit {/save who: $nick - modified:"$date $who" O:$O }

	# if there is new page content, save it now
	variable protected
	if {$N ne ""
	    && $C ne ""
	    && ![info exists protected($N)]
	} {
	    # added 2002-06-13 - edit conflict detection
	    if {$O != [list $date $who]} {
		return "
	<h2>Edit conflict on page $N - [Ref $N $name]</h2>
	<p><bold>Your changes have NOT been saved</bold>,
	because	someone (at IP address $who) saved
	a change to this page while you were editing.</p>
	<p><italic>Please restart a new [Ref /_edit/$N edit]
	and merge your version,	which is shown in full below.</italic></p>
	<hr size=1 />
	<p><pre>[armour $C]</pre></p>
	<hr size=1 />"
	    }

	    # newline-normalize content
	    set C [string map {\r\n \n \r \n} $C]

	    # check the content for utf8 correctness
	    set point [::utf8::findbad $C]
	    if {$point < [string length $C] - 1} {
		if {$point >= 0} {
		    incr point
		    set C [string replace $C $point $point "BOGUS"]
		}
		return "<h2>Encoding error on page $N - [Ref $N $name]</h2>
	<p><bold>Your changes have NOT been saved</bold>,
	because	the content your browser sent contains bogus characters.
	At character number $point.</p>
	<p><italic>Please check your browser.</italic></p>
	<hr size=1 />
	<p><pre>[armour $C]</pre></p>
	<hr size=1 />"
	    }

	    # Only actually save the page if the user selected "save"
	    if {$save eq "Save" && $nick ne ""} {
		invalidate $r $N
		invalidate $r 4
		invalidate $r _ref/$N

		# if this page did not exist before:
		# remove all referencing pages.
		#
		# this makes sure that cache entries point to a filled-in page
		# from now on, instead of a "[...]" link to a first-time edit page
		if {$date == 0} {
		    foreach from [mk::select wdb.refs to $N] {
			invalidate $r [mk::get wdb.refs!$from from]
		    }
		}

		set who $nick@[dict get $r -ipaddr]
		::Wikit::SavePage $N $C $who $name
		mk::file commit wdb
	    }
	}

	set r [Http Redirect $r "http://[dict get $r host]/$N"]
	return
    }

    proc GetPage {id} {
	return [mk::get wdb.pages!$id page]
    }

    # /reload - direct url to reload numbered pages from fs
    proc /reload {} {
	foreach {}
    }

    # called to generate an edit page
    proc /edit {N args} {
	upvar 1 response r
	set r [Http NoCache $r]

	if {![string is integer -strict $N]} {
	    set r [Http NotFound $r]
	    return
	}
	if {$N < 11} {
	    set r [Http Forbidden $r]
	    return
	}

	# is the caller logged in?
	set nick [who $r]
	if {$nick eq ""} {
	    set login "<p>You must <a href='/_login'>login</a> to post<p>"
	    # TODO KBK: Perhaps allow anon edits with a CAPTCHA?
	    # Or at least give a link to the page that gets the cookie back.
	} else {
	    set login ""
	}

	::Wikit::pagevars $N name date who

	set who_nick ""
	regexp {^(.+)[,@]} $who - who_nick

	set result "
	<h2>[Ref $N]</h2>
	$login
	<form method='post' action='/_save/$N'>
	 <textarea rows='30' cols='72' name='C'>[GetPage $N]</textarea>
	 <p />
	<input type='hidden' name='O' value='[list $date $who]'>
	<input type='submit' name='save' value='Save' [expr {$nick eq "" ? "disabled" : ""}] />
	<input type='submit' name='cancel' value='Cancel'] />
	</form>"

	if {$date != 0} {
	    append result "<italic>Last saved on <bold>[clock format $date -gmt 1 -format {%e %b %Y, %R GMT}]</bold></italic>"
	}
	if {$who_nick ne ""} {
	    append result "<italic> by <bold>$who_nick</bold></italic>"
	}
	if {$nick ne ""} {
	    append result " (you are: <bold>$nick</bold>)"
	}
	return $result
    }

    proc /motd {} {
	variable motd
	catch {set motd [::fileutil::cat [file join $::config(docroot) motd]]}

	upvar 1 response r
	invalidate $r 4	;# make it show up

	set R [Http Referer $r]
	if {$R eq ""} {
	    set r [Http Redirect $r "http://[dict get $r host]/0"]
	} else {
	    set r [Http Redirect $r $R]
	}
    }

    # called to generate a page with references
    proc /ref {N} {
	upvar 1 response r
	#set N [dict get $r -suffix]
	Debug.wikit {/ref $N}
	if {![string is integer -strict $N]} {
	    set r [Http NotFound $r]
	    return
	}

	set refList ""
	foreach from [mk::select wdb.refs to $N] {
	    set from [mk::get wdb.refs!$from from]
	    ::Wikit::pagevars $from name
	    lappend refList [list $name $from]
	}

	# the items are a list, if we would just sort on them, then all
	# single-item entries come first (the rest has {}'s around it)
	# the following sorts again on 1st word, knowing sorts are stable

	set refList [lsort -dict -index 0 [lsort -dict $refList]]
	set result ""
	foreach x $refList {
	    lassign $x name from
	    ::Wikit::pagevars $from who date
	    append result <li>[::Wikit::GetTimeStamp $date]
	    append result " . . . [Ref $from] . . . $who"
	}

	variable protected
	variable menus
	set menu {}
	foreach m {Search Changes About Home} {
	    lappend menu $menus($protected($m))
	}

	return "
	<h2>References to [Ref $N]</h2>
	<ul>$result</ul>
	<hr noshade />
	<p id='footer'>[join $menu { - }]</p>"
    }

    proc locate {page} {
	Debug.wikit {locate '$page'}
	variable cnt

	# try exact match on page name
	if {[string is integer -strict $page]} {
	    Debug.wikit {locate - is integer $page}
	    return $page
	}

	set N [mk::select wdb.pages name $page -min date 1]
	switch [llength $N] {
	    1 { # uniquely identified, done
		Debug.wikit {locate - unique by name - $N}
		return $N
	    }
	    
	    0 {
		# no match on page name,
		# do a glob search over names,
		# where AbCdEf -> *[Aa]b*[Cc]d*[Ee]f*
		# skip this if the search has brackets (WHY?)
		if {[string first "\[" $page] < 0} {
		    regsub -all {[A-Z]} $page \
			{*\\[&[string tolower &]\]} temp
		    set temp "[subst -novariable $temp]*"
		    set N [mk::select wdb.pages -glob name $temp -min date 1]
		}
		if {[llength $N] == 1} {
		    # glob search was unambiguous
		    Debug.wikit {locate - unique by title search - $N}
		    return $N
		}
	    }
	}

	# ambiguous match or no match - make it a keyword search
	set ::Wikit::searchLong [regexp {^(.*)\*$} $page x ::Wikit::searchKey]
	Debug.wikit {locate - kw search}
	return 2	;# the search page

	# these two globals (searchKey and searchLong) control the
	# representation of page 2 - they will cause it to return
	# a list of matches
    }

    proc InfoProc {ref} {
	set id [::Wikit::LookupPage $ref wdb]
	::Wikit::pagevars $id date name

	if {$date == 0} {
	    set id _edit/$id ;# enter edit mode for missing links
	}
    
	return [list /$id $name $date]
    }

    proc search {key} {
	Debug.wikit {search: '$key'}
	set long [regexp {^(.*)\*$} $key x key]
	set fields name
	if {$long} {
	    lappend fields page
	}
	
	set rows [mk::select wdb.pages -rsort date -keyword $fields $key]

	# tclLog "SearchResults key <$key> long <$searchLong>"
	set count 0
	set result "Searched for \"'''$key'''\" (in page titles"
	if {$long} {
	    append result { and contents}
	}
	append result "):\n\n"
	
	foreach i $rows {
	    # these are fake pages, don't list them
	    if {$i == 2 || $i == 4} continue

	    ::Wikit::pagevars $i date name
	    # these are fake pages, don't list them
	    if {$date == 0} continue

	    # ignore "near-empty" pages with at most 1 char, 30-09-2004
	    if {[mk::get wdb.pages!$i -size page] <= 1} continue

	    incr count
	    if {$count > 100} {
		append result "''Remaining [expr {[llength $rows] - 100}] matches omitted...''"
		break
	    }
	    
	    append result "   * [::Wikit::GetTimeStamp $date] . . . \[$name\]\n"
	}
	
	if {$count == 0} {
	    append result "   * '''''No matches found'''''\n"
	} else {
	    append result "   * ''Displayed $count matches''\n"
	}
	
	if {!$long} {
	    append result "\n''Tip: append an asterisk to search the page contents as well as titles.''"
	}

	return $result
    }

    proc do {r term} {
	# handle searches
	if {![string is integer -strict $term]} {
	    set N [locate $term]
	    if {$N == "2"} {
		return [Http Redirect $r "http://[dict get $r host]/2" "" "" S $term]
	    } elseif {$N ne $term} {
		# we really should redirect
		return [Http Redirect $r "http://[dict get $r host]/$N"]
	    }
	} else {
	    set N $term	;# it's a simple single page
	    if {$N == 2} {
		# it's a search - get the search term
		set qd [dict get? $r -Query]
		if {[Query exists $qd S]
		    && [set term [Query value $qd S]] ne ""
		} {
		    set search "value='[armour $term]'"
		    set C [::Wikit::TextToStream [search $term]]
		    lassign [::Wikit::StreamToHTML $C / ::WikitWub::InfoProc] C U
		} else {
		    set search ""
		    set C ""
		}

		set result "title: Search
		    <h2 class='title'>Search</h2>
		    <form action='/_search' method='post'>
		    <p>Enter the search phrase:<input name='S' type='text' $search> Append an asterisk (*) to search page contents as well</p>
		    </form>
		    $C
		"
		dict set r content-type text/x-system

		return [Http NoCache [Http Ok $r $result text/x-system]]
	    }
	}

	Debug.wikit {located: $N}

	# set up a few standard URLs an strings
	if {[catch {::Wikit::pagevars $N name date who}]} {
	    return [Http NotFound $r]
	}

	set refs [mk::select wdb.refs to $N]
        switch [llength $refs] {
	    0 {
		set backRef ""
		set Refs ""
		set Title [armour $name]
	    }
	    1 {
		set backRef /_ref/$N
		set Refs "[Ref $backRef Reference] - " 
		set Title [Ref $backRef $name]
	    }
	    default {
		set backRef /_ref/$N
		set Refs "[llength $refs] [Ref $backRef References]"
		set Title [Ref $backRef $name]
	    }
	}

	set updated ""
	if {$date != 0} {
	    set update [clock format $date -gmt 1 -format {%e %b %Y, %R GMT}]
	    set updated "Updated $update"
	}

	if {[regexp {^(.+)[,@]} $who - who_nick] && $who_nick ne ""} {
	    append updated " by $who_nick"
	}
	set menu [list $updated]

	variable protected
	if {![info exists protected($N)]} {
	    if {!$::roflag} {
		lappend menu [Ref /_edit/$N Edit]
		lappend menu [Ref /_rev/$N Revisions]
	    }

	    variable menus
	    lappend menu "Go to [Ref 0]"
	    foreach m {About Search Changes Help} {
		if {$N != $protected($m)} {
		    lappend menu $menus($protected($m))
		}
	    }
	}

	# get the contents
	if {$N == 4} {
	    variable motd
	    set C [::Wikit::TextToStream "${motd}[::Wikit::RecentChanges wdb]"]
	} else {
	    set C [::Wikit::TextToStream [GetPage $N]]
	}
	lassign [::Wikit::StreamToHTML $C / ::WikitWub::InfoProc] C U

	# get the page title
	if {![regsub {^<p>(<img src=".*?")>} $C [Ref \1 $backRef] C]} {
	    set Title "<h2 class='title'>$Title</h2>"
	} else {
	    set Title ""
	}

	set result {title: [armour $name]
	$Title
	$index
	<p>$C</p>
	<hr noshade />
	<p id='footer'>[join $menu { - }]</p>}

	# is this a search? add index
	set index ""
	set r [Http CacheableContent $r $date [subst $result] text/x-system]
	return [Http DCache $r]
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

set roflag 0
if {$roflag} {
    proc WikitWub::/save {args} {
	return "<h2>Read Only</h2><p>Your changes have not been saved.</p>"
    }
}

proc do {args} {
    variable response
    variable request
    set code [catch {{*}$args} r eo]
    switch -- $code {
	1 {
	    set response [Http ServerError $request $r $eo]
	    return 1
	}
	default {
	    set response $r
	    if {$code == 0} {
		set code 200
	    }
	    if {![dict exists $response -code]} {
		dict set response -code $code
	    }
	    Debug.wikit {Response code: $code / [dict get $response -code]}
	    return 0
	}
    }
}

Direct wikit -namespace ::WikitWub -ctype "text/x-html-fragment"
Convert convert -namespace ::WikitWub

foreach {dom expiry} {css {tomorrow} images {next week} templates 0 scripts {tomorrow} img {next week}} {
    File $dom -root [file join $config(docroot) $dom] -expires $expiry
}

catch {
    set ::WikitWub::motd [::fileutil::cat [file join $config(docroot) motd]]
}

proc incoming {req} {
    inQ put $req

    variable response
    variable request
    while {([dict size $request] == 0)
	   && ([catch {inQ get} req eo] == 0)
       } {
	set request $req

	set path [dict get $request -path]
	dict set request -cookies [Cookies parse4server [dict get? $request cookie]]

	# get a plausible prefix/suffix split
	Debug.wikit {incoming path: $path}
	set suffix [file join {} {*}[lassign [file split $path] -> fn]]
	dict set request -suffix $suffix
	dict set request -prefix "/$fn"
	Debug.wikit {invocation: fn:$fn suffix:$suffix}

	switch -glob -- $path {
	    /*.jpg -
	    /*.gif -
	    /*.png -
	    /favicon.ico {
		Debug.wikit {image invocation}
		# need to silently redirect image files
		dict set request -suffix [file join {} {*}[lrange [file split $path] 1 end]]
		dict set request -prefix "/images"
		do images do $request
	    }

	    /*.css {
		# need to silently redirect css files
		Debug.wikit {css invocation}
		dict set request -suffix [file join {} {*}[lrange [file split $path] 1 end]]
		dict set request -prefix "/css"
		do css do $request
	    }

	    /*.js {
		# need to silently redirect js files
		Debug.wikit {css invocation}
		dict set request -suffix [file join {} {*}[lrange [file split $path] 1 end]]
		dict set request -prefix "/scripts"
		do scripts do $request
	    }

	    /_motd -
	    /_edit/* -
	    /_save/* -
	    /_rev/* -
	    /_ref/* -
	    /_search/* -
	    /_search -
	    /_login {
		Debug.wikit {direct invocation}
		dict set request -suffix [string trimleft $fn _]
		set qd [Query add [Query parse $request] N $suffix]
		dict set request -Query $qd
		Debug.wikit {direct N: [Query value $qd N]}
		do wikit do $request
	    }

	    / {
		Debug.wikit {/ invocation}
		dict set request -suffix 0
		dict set request -Query [Query parse $request]
		do WikitWub do $request 0
	    }

	    default {
		Debug.wikit {default invocation}
		dict set request -suffix $fn
		dict set request -Query [Query parse $request]
		do WikitWub do $request $fn
	    }
	}
	Debug.wikit {Got Response:[set x $response; dict set x -content <ELIDED>; return $x]} 4

	# send response
	do convert do $response	;# convert page
	dict set response -transaction [dict get $request -transaction]
	dict set response -generation [dict get $request -generation]

	::thread::send -async [dict get $request -worker] [list send $response]
	set request [dict create]	;# go idle
    }
}

# fetch and initialize Wikit
package require Wikit::Format
#namespace import Wikit::Format::*
package require Wikit::Db
package require Wikit::Cache
Wikit::BuildTitleCache

set script [mk::get wdb.pages!9 page]
#puts stderr "Script: $script"
eval $script

# move utf8 regexp into utf8 package
set ::utf8::utf8re $config(utf8re); unset config(utf8re)

Debug off wikit 10
Debug off direct 10
Debug off cookies 10
Debug off socket 10

thread::wait
