# WubTk.tcl - a domain built around coroutines from tcl8.6
#
# Inspired by Roy Keene's http://www.rkeene.org/projects/tkweb/

# TODO:
#* comet - ajax push - for mods to local vars ... push them to client DONE?
#* fix Site so it grabs WubTk
#* rename to Toplevel?
#* [exit] to redirect DONE
#* [toplevel] to create a new window/tab - the old Tk hangs around
#* frame -> fieldset
#* ensure text is working
#* enable/push wm title changes
#* what to do about event [update] stuff? - currently stubbed
#* spinning wheel on updates DONE
#* menus
#* comboboxes, spinboxes
#* frames, labelframes
#* add -grid {r c rs cs} option to widgets, to take over the [grid configure .w] function
#* add [Widget relative] method to give name-within-grid
#* move all widget commands into an interp

package require Debug
Debug define wubtk 10
package require Http
package require md5

package require WubWidgets

package provide WubTk 1.0
if {[catch {package present Tk}]} {
    package provide Tk 8.6
}

set ::API(Domains/WubTk) {
    {WubTk - a Web emulation of Tk}
}

set ::WubTk_dir [file normalize [file dirname [info script]]]

class create ::WubTkI {
    method buttonJS {{what .button}} {
	return [string map [list %B% [jQ S $what]] {
	    $('%B%').click(function () { 
		//alert($(this).attr("name")+" button pressed");
		$("#Spinner_").show();
		$.ajax({
		    context: this,
		    type: "GET",
		    url: ".",
		    data: {id: $(this).attr("name"), _op_: "command"},
		    dataType: "script",
		    success: function (data, textStatus, XMLHttpRequest) {
			$("#Spinner_").hide();
			//alert("button: "+data);
		    },
		    error: function (xhr, status, error) {
			alert("ajax fail:"+status);
		    }
		});
	    });
	}]
    }

    method cbuttonJS {{what .cbutton}} {
	return [string map [list %B% [jQ S $what]] {
	    $('%B%').change(function () { 
		//alert($(this).attr("name")+" cbutton pressed");
		$("#Spinner_").show();
		var data = {id: $(this).attr("name"), val: 0, _op_: "cbutton"};
		var val = this.value;
		if($(this).is(":checked")) {
		    data['val'] = val;
		}

		$.ajax({
		    context: this,
		    type: "GET",
		    url: ".",
		    data: data,
		    dataType: "script",
		    success: function (data, textStatus, XMLHttpRequest) {
			$("#Spinner_").hide();
			//alert("cbutton: "+data);
		    },
		    error: function (xhr, status, error) {
			alert("ajax fail:"+status);
		    }
		});
	    });
	}]
    }

    method variableJS {{what .variable}} {
	return [string map [list %B% [jQ S $what]] {
	    $('%B%').change(function () {
		//alert($(this).attr("name")+" changed: " + $(this).val());
		$("#Spinner_").show();
		$.ajax({
		    context: this,
		    type: "GET",
		    url: ".",
		    data: {id: $(this).attr("name"), val: $(this).val(), _op_: "var"},
		    dataType: "script",
		    success: function (data, textStatus, XMLHttpRequest) {
			//alert("variable: "+data);
			$("#Spinner_").hide();
		    },
		    error: function (xhr, status, error) {
			alert("ajax fail:"+status);
		    }
		});
	    });
	}]
    }

    method update {changes} {
	Debug.wubtk {[self] UPDATE ($changes)}

	set result ""
	foreach {id html type} $changes {
	    Debug.wubtk {[namespace tail [self]] changed id: $id type: $type}
	    dict lappend classified $type $id
	    set html [string map {\n \\n} $html]
	    set jid #$id
	    append result [string map [list %ID% [jQ S $jid] %H% $html] {
		$('%ID%').replaceWith("%H%");
	    }]

	    # send js to track widget state
	    switch -- $type {
		button {
		    append result [my buttonJS $jid]
		}
		checkbutton {
		    append result [my cbuttonJS $jid]
		}
		entry -
		text {
		    append result [my variableJS $jid]
		}
	    }
	}

	Debug.wubtk {SCRIPT: ($result)}
	return $result
    }

    # strip any of the js donated by changed components
    # and plug it on the end of the generic stuff.
    method stripjs {r} {
	set content {}
	dict for {n v} [dict get? $r -script] {
	    if {[string match !* $n]} {
		#Debug.wubtk {stripjs: $n}
		set v [join [lrange [split $v \n] 1 end-1] \n]
		lappend content $v
	    }
	}
	return [join $content \n]
    }

    method rq {} {
	variable r
	return $r
    }

    method redirect {args} {
	variable redirect
	if {[llength $args]} {
	    set redirect $args
	} else {
	    return $redirect
	}
    }

    method do_redir {r {js 0}} {
	variable redirect
	set redir $redirect
	if {![llength $redir]} {
	    return $r
	}
	set redirect ""	;# forget redirection request now

	# we've been asked to redirect ... so do it
	if {$js} {
	    set redir [Url redir $r {*}$redir]
	    Debug.wubtk {do_redir javascript: $redir}
	    return [Http Ok $r "window.location='$redir';" application/javascript]
	} else {
	    Debug.wubtk {do_redir HTTP: $redirect}
	    return [Http Redirect $r $redirect]
	}
    }

    # support the cookie widget
    method cookie {op caller args} {
	variable cdict	;# current request's cookies
	if {[llength $args]%2} {
	    set args [lassign $args value]
	}

	set name [$caller widget]
	set opts {path domain port expires secure max-age}
	foreach n $opts {
	    set val [$caller cget? -$n] 
	    if {$val ne ""} {
		set $n $val
	    }
	}

	variable cookiepath;
	if {[info exists path]} {
	    if {[string first $cookiepath $path] != 0} {
		set path "$cookiepath/$path"
		$caller configure -path $path
	    }
	} else {
	    set path $cookiepath
	    $caller configure -path $path
	}

	set matcher {}
	set rest {}
	foreach n [list name {*}$opts] {
	    if {![info exists $n]} continue
	    if {$n in {name path domain}} {
		lappend matcher -$n [set $n]
	    } else {
		lappend rest -$n [set $n]
	    }
	}

	Debug.wubtk {cookie $op matcher:$matcher rest:$rest for $caller}

	switch -- $op {
	    get {
		return [dict get [Cookies fetch $cdict {*}$matcher] -value]
	    }

	    clear {
		set cdict [Cookies clear $cdict {*}$matcher]
	    }

	    set {
		set matches [Cookies match $cdict {*}$matcher]
		if {![llength $matches]} {
		    set cdict [Cookies add $cdict -value $value {*}$matcher {*}$rest]
		}
		set cdict [Cookies modify $cdict -value $value {*}$args {*}$matcher {*}$rest]
	    }

	    construct {
		set matches [Cookies match $cdict {*}$matcher]
		if {[llength $matches] > 1} {
		    error "Ambiguous cookie, matches '$matches'"
		} else {
		    Debug.wubtk {new cookie $matcher}
		}
		if {[llength $matches]} {
		    $caller configure {*}[Cookies fetch $cdict {*}$matcher]
		}
	    }
	}
    }

    method tl {op widget args} {
	variable toplevels
	switch -- $op {
	    add {
		dict set toplevels $widget visible
	    }
	    delete {
		dict set toplevels $widget delete
	    }
	    hide {
		dict set toplevels $widget hide
	    }
	}
    }

    method prep {r} {
	# re-render whole page
	set r [jQ jquery $r]
	set r [jQ scripts $r jquery.form.js]
	set r [jQ tabs $r .notebook]
	set r [jQ accordion $r .accordion]

	# send js to track widget state
	set r [jQ ready $r [my buttonJS]]
	set r [jQ ready $r [my cbuttonJS]]
	set r [jQ ready $r [my variableJS]]
    }

    method render {r} {
	variable redirect
	if {[llength $redirect]} {
	    # the widget command did a redirect
	    return [my do_redir $r]
	}

	set r [my prep $r]
	variable timeout

	if {$timeout > 0} {
	    Debug.wubtk {Comet push $timeout}
	    set r [jQ comet $r ./?_op_=refresh]
	} else {
	    Debug.wubtk {Comet no push}
	}

	# add some CSS
	variable theme
	set r [jQ theme $r $theme]

	set css [wm css .]
	if {$css ne ""} {
	    set content [<style> $css]
	} else {
	    set content ""
	}

	set style [wm stylesheet .]
	if {$style ne ""} {
	    set r [Html postscript $r [<stylesheet> {*}$style]]
	}

	try {
	    append content [grid render]
	    set r [grid js $r]
	    Debug.wubtk {RENDER JS: [my stripjs $r]}
	    append content [<div> id ErrDiv {}]
	    append content [<span> id STORE {}]

	    variable toplevels
	    set tljs {}
	    foreach {tl tv} $toplevels {
		set tlw [$tl widget]
		# render visible toplevels
		Debug.wubtk {TV $tl: $tv}
		switch -- $tv {
		    visible {
			# open the toplevel window/tab
			set title [$tl cget? -title]
			if {$title eq ""} {
			    set title $tlw
			}
			set opts [list '$tlw/' '$title']
			if {0} {
			    foreach opt {titlebar menubar toolbar
				location scrollbars status resizable} {
				if {[$tl cget $opt]} {
				    lappend opts $opt='yes'
				} else {
				    lappend opts $opt='no'
				}
			    }
			    foreach opt {width height left top} {
				if {[$tl cget? $opt] ne ""} {
				    lappend opts $opt=[$tl cget $opt]
				}
			    }
			}
			lappend tljs "\$('#STORE').data('$tlw', window.open([join $opts ,]))"
		    }
		    delete {
			# close the toplevel window/tab
			lappend tljs "\$('#STORE').data('$tlw').close()"
			dict unset toplevels $tl
		    }
		    hide {
			# close the toplevel window/tab
			lappend tljs "\$('#STORE').data('$tlw').close()"
		    }
		}
		if {[llength $tljs]} {
		    set tljs [join $tljs ";\n"]
		    set r [Html postscript $r $tljs]
		}
	    }

	    variable icons
	    variable spinner_size
	    variable spinner_style
	    append content [string map [list %SS% $spinner_style] [<img> id Spinner_ style {%SS%; display:none;} width $spinner_size src $icons/bigrotation.gif]]
	    Debug.wubtk {RENDERED: $content}

	    dict set r -title [wm title .]
	    dict lappend r -headers [wm header .]
	    
	    set r [Http Ok $r $content x-text/html-fragment]
	} on error {e eo} {
	    set r [Http ServerError $r $e $eo]
	}

	return $r
    }

    method do_refresh {r} {
	# client has asked us to push changes
	Debug.wubtk {[self] client has asked us to push changes}
	set changes [lassign [grid changes $r] r]
	set update [my update $changes]
	append update [my stripjs $r]

	if {$update eq ""} {
	    # no updates to send
	    if {[info exists _refresh]} {
		# we already have a pending _refresh
		# likely the connection has timed out
		Debug.wubtk {WubTk [info coroutine] - double refresh}
		set _refresh [Http Ok $_refresh {} application/javascript]
		Httpd Resume $_refresh
	    }
	    set _refresh $r	;# remember request
	    set r [Httpd Suspend $r]	;# suspend until changes
	    grid prod 1	;# register interest
	} else {
	    grid prod 0	;# no registered interest
	    set r [Http Ok $r $update application/javascript]
	}
	return $r
    }

    # process a browser event
    method event {r} {
	# clear out any old refresh - this response will satisfy it
	if {[info exists _refresh]} {
	    Debug.wubtk {satisfy old refresh}
	    set re [Http Ok $_refresh {} application/javascript]
	    unset _refresh
	    Httpd Resume $re
	} else {
	    grid prod 0	;# no registered interest
	}
	
	# client event has been received
	set Q [Query flatten [Query parse $r]]
	set widget .[dict Q.id]

	if {[llength [info commands [namespace current]::$widget]]} {
	    Debug.wubtk {event $widget ($Q)}
	    set e {$('#ErrDiv').html('');}
	    try {
		$widget [dict r.-op] [dict Q.val?]	;# run the widget op
	    } on error {e eo} {
		# widget op caused an error - report on it
		Debug.wubtk {event error on $widget: '$e' ($eo)}
		set e "cmd: [string map [list \" \\\" ' \\'] $e]"
		set e [string map [list %E% $e] {
		    $('#ErrDiv').html('<p>Error: %E% </p>');
		}]
	    } finally {
		variable redirect
		if {[llength $redirect]} {
		    # the widget command did a redirect
		    return [my do_redir $r 1]
		} else {
		    # reflect changes due to the widget command
		    set changes [lassign [grid changes $r] r]
		    if {[dict exists $r -repaint]} {
			# a repaint has been triggered through grid operation
			catch {dict unset -r -script}
			return [Http Ok $r {window.location='.';} application/javascript]
		    } else {
			# normal result - flush changes to client
			set content [my update $changes]
			append content [my stripjs $r]
			append content $e
			return [Http Ok $r $content application/javascript]
		    }
		}
	    }
	} else {
	    # widget doesn't exist - report that
	    Debug.wubtk {not found [namespace current]::$widget}
	    set content "\$('#ErrDiv').html('Widget "$widget" not found');"
	    return [Http Ok $r $content application/javascript]
	}
    }

    method do_image {r} {
	set cmd .$widget
	if {[llength [info commands [namespace current]::$cmd]]} {
	    Debug.wubtk {image $cmd}
	    set r [$cmd fetch $r]
	} else {
	    Debug.wubtk {not found image [namespace current]::$cmd}
	    set r [Http NotFound $r]
	}
	return $r
    }

    method do {req lambda} {
	Debug.wubtk {[info coroutine] PROCESS in namespace:[namespace current]}

	variable r $req	;# keep our current request around

	# run user code - return result
	variable cdict [dict get? $r -cookies]
	interp eval $lambda	;# install the user code
	set r [my render $r]
	Debug.wubtk {COOKIES: $cdict}
	dict set r -cookies $cdict	;# reflect cookies back to client
	
	# initial client direct request
	variable exit 0
	while {!$exit} {
	    lassign [::yieldm $r] what r
	    Debug.wubtk {[info coroutine] processing '$what'}
	    switch -- $what {
		prod {
		    # our grid has prodded us - there are changes
		    if {[info exists _refresh]} {
			Debug.wubtk {prodded with suspended refresh}
			# we've been prodded by grid with a pending refresh
			set changes [lassign [grid changes $_refresh] _refresh]
			set content [my update $changes]
			append content [my stripjs $_refresh]
			set re [Http Ok $_refresh $content application/javascript]
			unset _refresh
			Httpd Resume $re
		    } else {
			Debug.wubtk {prodded without suspended refresh}
			grid prod 0	;# no registered interest
		    }
		}

		terminate {
		    Debug.wubtk {requested termination}
		    break	;# we've been asked to terminate
		}

		client {
		    set cdict [dict get? $r -cookies]

		    # unpack query response
		    Debug.wubtk {[info coroutine] Event: [dict r.-op?]}
		    switch -glob -- [dict r.-op?] {
			command -
			cbutton -
			slider -
			var {
			    # process browser event
			    set r [my event $r]
			}

			upload {
			    # client event has been received
			    set Q [Query flatten [Query parse $r]]
			    set widget .[dict Q.id]

			    if {[llength [info commands [namespace current]::$widget]]} {
				Debug.wubtk {event $widget ($Q)}
				try {
				    $widget [dict r.-op] [dict Q.val?]
				} on error {e eo} {
				} finally {
				    set r [my render $r]
				}
			    } else {
				# widget doesn't exist - report that
				Debug.wubtk {not found [namespace current]::$widget}
				set content "\$('#ErrDiv').html('Widget "$widget" not found');"
				set r [Http Ok $r $content application/javascript]
			    }
			}

			refresh {
			    # process refresh event
			    set r [my do_refresh $r]
			}

			default {
			    # nothing else to be done ... repaint display
			    set widget [dict r.-widget]
			    if {$widget eq ""} {
				Debug.wubtk {render .}
				set r [my render $r]
			    } else {
				Debug.wubtk {fetch and render toplevel $widget}
				try {
				    set r [.$widget fetch $r]
				} on error {e eo} {
				    set r [Http ServerError $r $e $eo]
				} finally {
				}
			    }
			}
		    }
		    dict set r -cookies $cdict	;# reflect cookies back to client
		}
	    }
	}

	# fallen out of loop - time to go
	Debug.wubtk {[info coroutine] exiting}
	return $r
    }

    destructor {
	catch {interp destroy}
    }

    method destroyme {args} {
	[self] destroy
    }

    superclass FormClass
    constructor {args} {
	variable interp {}
	variable {*}$args
	variable redirect ""	;# no redirection, initially
	variable exit 0		;# do not exit, initially
	next? {*}$args		;# construct Form
	variable toplevels {}	;# keep track of toplevels

	Debug.wubtk {constructed WubTkI self-[self]  - ns-[namespace current] ($args)}

	# create an interpreter within which to evaluate user code
	# install its command within our namespace
	set interp [::interp create {*}$interp -- [namespace current]::interp]

	Debug.wubtk {[info coroutine] INTERP $interp}
	interp eval [list set ::auto_path $::auto_path]

	# create per-coro namespace commands
	namespace eval [namespace current] {
	    proc exit {value} {
		variable exit 1	;# flag the exit
		Debug.wubtk {exit $value}
		if {![string is integer -strict $value]} {
		    my redirect {*}$value
		}
	    }
	    
	    proc connection {args} {
		return [my {*}$args]
	    }

	    proc destroy {} {
		namespace delete [namespace current]
	    }
	    proc update {args} {}
	}

	WubWidgets gridC create [namespace current]::grid	;# per-coro grid instance
	WubWidgets wmC create [namespace current]::wm		;# per-coro wm instance

	if {[info exists css]
	    && $css ne ""
	} {
	    wm css . $css
	}

	if {[info exists stylesheet]
	    && $stylesheet ne ""
	} {
	    wm stylesheet . {*}$stylesheet
	}

	foreach n {grid wm connection destroy update exit} {
	    interp alias $n [namespace current]::$n
	}
	interp alias rq [self] rq

	# install aliases for Tk Widgets
	foreach n $::WubWidgets::tks {
	    proc [namespace current]::$n {w args} [string map [list %N% $n %C% [self]] {
		Debug.wubtk {SHIM: 'WubWidgets %N%C create [namespace current]::$w'}
		set obj [WubWidgets %N%C create [namespace current]::$w -interp [list [namespace current]::interp eval] -connection %C% {*}$args]
		interp alias [namespace tail $obj] $obj
		#Debug.wubtk {aliases: [interp aliases]}
		
		return [namespace tail $obj]
	    }]
	    interp alias $n [namespace current]::$n
	}

	# construct an image command
	proc image {args} {
	    Debug.wubtk {SHIM: 'WubWidgets image $args'}
	    set obj [WubWidgets image {*}$args]
	    Debug.wubtk {SHIMAGE: $obj}
	    interp alias [namespace tail $obj] $obj
	    return [namespace tail $obj]
	}

	interp alias image [namespace current]::image
	interp eval {package provide Tk 8.6}

	oo::objdefine [self] forward site ::Site
    }
}

class create ::WubTk {
    method getcookie {r} {
	variable cookie
	# try to find the application cookie
	set cl [Cookies Match $r -name $cookie]
	if {[llength $cl]} {
	    # we know they're human - they return cookies (?)
	    return [dict get [Cookies Fetch $r -name $cookie] -value]
	} else {
	    return ""
	}
    }

    method newcookie {r {cmd ""}} {
	if {$cmd eq ""} {
	    # create a new cookie
	    variable uniq; incr uniq
	    set cmd [::md5::md5 -hex $uniq[clock microseconds]]
	}

	# add a cookie to reply
	if {[dict exists $r -cookies]} {
	    set cdict [dict get $r -cookies]
	} else {
	    set cdict [dict create]
	}

	# include an optional expiry age
	variable expires
	if {$expires ne ""} {
	    if {[string is integer -strict $expires]} {
		# it's an age
		if {$expires != 0} {
		    set expiresC [Http Date [expr {[clock seconds] + $expires}]]
		    set expiresC [list -expires $expires]
		} else {
		    set expiresC {}
		}
	    } else {
		set expiresC [Http Date [clock scan $expires]]
		set expiresC [list -expires $expires]
	    }
	} else {
	    set expiresC {}
	}
	
	# add the cookie
	variable cookie; variable mount
	set cdict [Cookies add $cdict -path $mount -name $cookie -value $cmd {*}$expiresC]
	Debug.wubtk {created wubapp cookie $cdict}
	
	dict set r -cookies $cdict
	dict set r -wubapp $cmd
	return $r
    }

    method call {r cmd suffix extra} {
	# this is an existing coroutine - call it and return result
	Debug.wubtk {calling coroutine '$cmd' with extra '$extra'}
	if {[catch {
	    [namespace current]::Coros::$cmd client $r
	} result eo]} {
	    Debug.error {'$cmd' error: $result ($eo)}
	    return [Http ServerError $r $result $eo]
	}
	Debug.wubtk {'$cmd' yielded: ($result)}
	return $result
    }

    # process request helper
    method do {r} {
	variable mount
	# calculate the suffix of the URL relative to $mount
	lassign [Url urlsuffix $r $mount] result r suffix path
	if {!$result} {
	    return [Httpd NotFound $r]	;# the URL isn't in our domain
	}

	# decode stuff to the right of the mount URL
	set widget ""
	dict set r -extra [join [set extra [lassign [split $suffix /] widget]] /]
	dict set r -widget $widget

	# get op from query
	set Q [Query parse $r]; dict set r -Query $Q; set Q [Query flatten $Q]
	dict set r -op [set op [dict Q._op_?]]

	set wubapp [my getcookie $r]	;# get the wubapp cookie
	Debug.wubtk {process cookie: '$wubapp' widget:'$widget' op:'$op' extra:'$extra' suffix:'$suffix' over '$mount'}
	
	if {$wubapp ne ""
	    && [namespace which -command [namespace current]::Coros::$wubapp] ne ""
	} {
	    dict set r -wubapp $wubapp
	    return [my call $r $wubapp $suffix $extra]
	} else {
	    Debug.wubtk {coroutine gone: $wubapp -> $mount$widget}
	    if {$wubapp ne ""} {
		# they handed us a cookie relating to a defunct coro,
		# doesn't matter, the value is purely nominal,
		# go with it.
		if {$op ne ""} {
		    # this is an old invocation trying to get javascript
		    # make it redirect
		    set app "$mount/$widget/"
		    return [Http Ok $r "window.location='$app';" application/javascript]
		}
	    } else {
		set r [my newcookie $r]	;# brand new webapp
		set wubapp [dict get $r -wubapp]
	    }

	    # collect options to pass to coro
	    set options {}
	    foreach v {timeout icons theme spinner_style spinner_size css stylesheet cookiepath} {
		variable $v
		if {[info exists $v]} {
		    lappend options $v [set $v]
		}
	    }

	    # create the coroutine
	    try {
		variable lambda
		set o [::WubTkI create [namespace current]::Coros::O_$wubapp {*}$options]
		set r [::Coroutine [namespace current]::Coros::$wubapp $o do $r $lambda]
		trace add command [namespace current]::Coros::$wubapp delete [list $o destroyme]
	    } on error {e eo} {
		Debug.wubtk {[info coroutine] error '$e' ($eo)}
		catch {$o destroy}
		catch {rename [namespace current]::Coros::$wubapp {}}
		set r [Http ServerError $r $e $eo]
	    } finally {
		return $r
	    }
	}
    }

    constructor {args} {
	variable {*}[Site var? WubTk]	;# allow .ini file to modify defaults
	variable lambda ""
	variable expires ""
	variable css {
	    .slider { margin: 10px; }
	    fieldset {
		-moz-border-radius: 7px;
		-webkit-border-radius: 7px;
	    }
	    textarea {
		-moz-border-radius: 7px;
		-webkit-border-radius: 7px;
	    }
	    button {
		-moz-border-radius: 7px;
		-webkit-border-radius: 7px;
	    }
	    input {
		-moz-border-radius: 7px;
		-webkit-border-radius: 7px;
	    }
	}

	variable stylesheet ""
	variable timeout 0
	variable icons /icons/
	variable theme dark
	variable spinner_size 20
	variable spinner_style "position: fixed; top:10px; left: 10px;"
	variable {*}$args

	if {![info exists cookiepath] || $cookiepath eq ""} {
	    variable cookiepath $mount
	}

	if {[info exists file]} {
	    append lambda [fileutil::cat -- $file]
	} elseif {![info exists lambda] || $lambda eq ""} {
	    variable lambda [fileutil::cat -- [file join $::WubTk_dir test.tcl]]
	}

	if {![info exists cookie]} {
	    variable cookie [string map {/ _} $mount]
	}

	namespace eval [namespace current]::Coros {}

	next? {*}$args

    }
}
