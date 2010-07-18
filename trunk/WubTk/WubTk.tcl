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

class create ::WubTk {
    method buttonJS {{what .button}} {
	return [string map [list %B% [jQ S $what]] {
	    $('%B%').click(function () { 
		//alert($(this).attr("name")+" button pressed");
		$("#Spinner_").show();
		$.ajax({
		    context: this,
		    type: "GET",
		    url: "button",
		    data: {id: $(this).attr("name")},
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
		var data = {id: $(this).attr("name"), val: 0};
		var val = this.value;
		if($(this).is(":checked")) {
		    data['val'] = val;
		}

		$.ajax({
		    context: this,
		    type: "GET",
		    url: "cbutton",
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
		    url: "variable",
		    data: {id: $(this).attr("name"), val: $(this).val()},
		    dataType: "script",
		    success: function (data, textStatus, XMLHttpRequest) {
			//alert("button: "+data);
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
		Debug.wubtk {stripjs: $n}
		set v [join [lrange [split $v \n] 1 end-1] \n]
		lappend content $v
	    }
	}
	return [join $content \n]
    }

    method process {r lambda options} {
	Debug.wubtk {[info coroutine] PROCESS - ($options) in namespace:[namespace current]}
	dict with options {}

	# create an interpreter within which to evaluate user code
	# install its command within our namespace
	set interp [interp create {*}[dict options.interp?] -- [namespace current]::${cmd}_wkinterp]
	set ${ns}::interp $interp	;# copy the interp value to the coro's namespace
	Debug.wubtk {[info coroutine] INTERP $interp ([interp slaves])}

	# create per-coro namespace commands
	namespace eval [namespace current] {
	    WubWidgets gridC create grid	;# per-coro grid instance
	    WubWidgets wmC create wm	;# per-coro wm instance
	    
	    proc connection {args} {
		after 0 [list [info coroutine] prod]
	    }
	    proc destroy {} {
		namespace delete [namespace current]
	    }
	    proc update {args} {}

	    proc exit {args} {
		grid exit {*}$args
	    }
	}
	
	foreach n {grid wm connection destroy update exit} {
	    $interp alias $n [namespace current]::$n
	}
    
	# install aliases for Tk Widgets
	foreach n $::WubWidgets::tks {
	    proc $n {w args} [string map [list %N% $n %I% $interp] {
		Debug.wubtk {SHIM: 'WubWidgets %N%C create [namespace current]::$w'}
		set obj [WubWidgets %N%C create [namespace current]::$w -interp [list %I% eval] {*}$args]
		%I% alias [namespace tail $obj] $obj
		Debug.wubtk {aliases: [%I% aliases]}
		
		return [namespace tail $obj]
	    }]
	    $interp alias $n [namespace current]::$n
	}

	proc image {args} [string map [list %I% $interp] {
	    Debug.wubtk {SHIM: 'WubWidgets image $args'}
	    set obj [WubWidgets image {*}$args]
	    Debug.wubtk {SHIMAGE: $obj}
	    %I% alias [namespace tail $obj] $obj
	    return [namespace tail $obj]
	}]
	$interp alias image [namespace current]::image

	Debug.wubtk {aliases: [$interp aliases]}

	$interp eval {package provide Tk 8.6}
	if {[catch {
	    $interp eval $lambda	;# install the user code
	} e eo]} {
	    return [Http ServerError $r $e $eo]
	}

	# initial client direct request
	Debug.wubtk {processing [info coroutine]}
	set r {}	;# initial response is empty - let WubTk redirect
	while {1} {
	    set r [::yield $r]
	    if {[catch {dict size $r} sz]} {
		# this is not a dict... is it a prod?
		if {$r eq "prod"} {
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
		    continue
		} else {
		    Debug.error {WubTk [info coroutine] got bad call '$r'}
		    grid prod 0	;# no registered interest
		    error "WubTk [info coroutine] got bad call '$r'"
		}
	    } elseif {$sz == 0} {
		Debug.wubtk {requested termination}
		break	;# we've been asked to terminate
	    }

	    # unpack query response
	    set Q [Query parse $r]; dict set r -Query $Q; set Q [Query flatten $Q]
	    Debug.wubtk {[info coroutine] Event: [dict get? $r -extra] ($Q)}
	    set cmd ""; set op ""
	    set err [catch {
		set extra [dict get? $r -extra]
		switch -glob -- $extra {
		    refresh {
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
			continue	;# we've served this request
		    }

		    button {
			# client button has been pressed
			set cmd .[dict Q.id]
			if {[llength [info commands [namespace current]::$cmd]]} {
			    Debug.wubtk {button $cmd ($Q)}
			    $cmd command
			} else {
			    Debug.wubtk {not found button [namespace current]::$cmd}
			}
		    }

		    cbutton {
			# client checkbutton has been pressed
			set cmd .[dict Q.id]
			if {[llength [info commands [namespace current]::$cmd]]} {
			    Debug.wubtk {cbutton $cmd ($Q)}
			    $cmd cbutton [dict Q.val]
			} else {
			    Debug.wubtk {not found cbutton [namespace current]::$cmd}
			}
		    }

		    slider {
			# client variable has been set
			set cmd .[dict Q.id]
			if {[llength [info commands [namespace current]::$cmd]]} {
			    Debug.wubtk {scale $cmd ($Q)}
			    $cmd scale [dict Q.val]
			} else {
			    Debug.wubtk {not found scale [namespace current]::$cmd}
			}
		    }

		    variable {
			# client variable has been set
			set cmd .[dict Q.id]
			if {[llength [info commands [namespace current]::$cmd]]} {
			    Debug.wubtk {button $cmd ($Q)}
			    $cmd var [dict Q.val]
			} else {
			    Debug.wubtk {not found button [namespace current]::$cmd}
			}
		    }

		    image/* {
			set cmd .[lindex [split $extra /] end]
			if {[llength [info commands [namespace current]::$cmd]]} {
			    Debug.wubtk {image $cmd}
			    set r [$cmd fetch $r]
			} else {
			    Debug.wubtk {not found image [namespace current]::$cmd}
			    set r [Http NotFound $r]
			}
			continue
		    }

		    default {
			# re-render whole page
			set r [jQ jquery $r]

			# send js to track widget state
			set r [jQ ready $r [my buttonJS]]
			set r [jQ ready $r [my cbuttonJS]]
			set r [jQ ready $r [my variableJS]]
			if {$timeout > 0} {
			    set r [jQ comet $r refresh]
			}

			# add some CSS
			set r [jQ theme $r $theme]
			set content [<style> {
			    .slider { margin: 10px; }
			}]

			if {[catch {
			    append content [grid render]
			    set r [grid js $r]
			    Debug.wubtk {RENDER JS: [my stripjs $r]}
			} e eo]} {
			    set r [Http ServerError $r $e $eo]
			} else {
			    append content [<div> id ErrDiv {}]
			    append content [string map [list %SS% $spinner_style] [<img> id Spinner_ style {%SS%; display:none;} width $spinner_size src $icons/bigrotation.gif]]
			    Debug.wubtk {RENDERED: $content}
			    dict set r -title [wm title]
			    set r [Http Ok $r $content x-text/html-fragment]
			}
			continue
		    }
		}
	    } e eo]

	    switch -- $err {
		4 continue
		3 break
	    }

	    if {[grid exiting?]} {
		# exit's been called by the app - arrange for redirect/exit
		Debug.wubtk {exit redirect: [grid redirect]}
		set redirect [grid redirect]
		break
	    }

	    if {$err} {
		set e "$cmd: $e"
	    } else {
		set e ""
	    }

	    # clear out any old refresh - this response will satisfy it
	    if {[info exists _refresh]} {
		Debug.wubtk {satisfy old refresh}
		set re [Http Ok $_refresh {} application/javascript]
		unset _refresh
		Httpd Resume $re
	    } else {
		grid prod 0	;# no registered interest
	    }

	    Debug.wubtk {sending pending updates - $e}
	    set changes [lassign [grid changes $r] r]
	    set content [my update $changes]
	    append content [my stripjs $r]

	    if {$e ne ""} {
		set e [string map [list \" \\\" ' \\'] $e]
		append content [string map [list %E% $e] {
		    $('#ErrDiv').html('<p>Error: %E% </p>');
		}]
	    } elseif {$content ne ""} {
		append content {
		    $('#ErrDiv').html('');
		}
	    }

	    Debug.wubtk {update SCRIPT: $content}
	    set r [Http Ok $r $content application/javascript]
	}

	# fallen out of loop - time to go
	destroy	;# destroy all resources
	if {[info exists redirect] && $redirect ne ""} {
	    set redirect [Url redir $r $redirect]
	    Debug.wubtk {[info coroutine] redirecting to '$redirect'}
	    return [Http Ok $r "window.location='$redirect';" application/javascript]
	}
	Debug.wubtk {[info coroutine] exiting}
    }

    # process request helper
    method do {r} {
	variable mount
	# calculate the suffix of the URL relative to $mount
	lassign [Url urlsuffix $r $mount] result r suffix path
	if {!$result} {
	    return [Httpd NotFound $r]	;# the URL isn't in our domain
	}

	set extra [lassign [split $suffix /] cmd]
	dict set r -extra [join $extra /]

	Debug.wubtk {process '$suffix' over '$mount' extra: '$extra'}
	
	if {$suffix eq "/" || $suffix eq ""} {
	    # this is a new call - create the coroutine
	    variable uniq; incr uniq
	    set cmd [::md5::md5 -hex $uniq[clock microseconds]]
	    dict set r -cmd $cmd

	    # construct a namespace for this command
	    namespace eval [namespace current]::Coros::$cmd [list namespace path [list ::WubWidgets [info object namespace [self]]]]	;# get WubWidgets and WubTk on the command path

	    # collect options to pass to coro
	    set options {}
	    foreach v {timeout icons theme spinner_style spinner_size} {
		variable $v
		lappend options $v [set $v]
	    }
	    lappend options cmd $cmd
	    lappend options ns [namespace current]::Coros::${cmd}

	    variable lambda
	    set result [coroutine [namespace current]::Coros::${cmd}::_do my process $r $lambda $options]

	    if {$result ne ""} {
		Debug.wubtk {coroutine initialised - ($r) reply}
		return $result	;# allow coroutine lambda to reply
	    } else {
		# otherwise simply redirect to coroutine lambda
		Debug.wubtk {coroutine initialised - redirect to ${mount}$cmd}
		return [Http Redirect $r [string trimright $mount /]/$cmd/]
	    }
	}
	
	if {[namespace which -command [namespace current]::Coros::${cmd}::_do] ne ""} {
	    # this is an existing coroutine - call it and return result
	    Debug.wubtk {calling coroutine '$cmd' with extra '$extra'}
	    if {[catch {
		[namespace current]::Coros::${cmd}::_do $r
	    } result eo]} {
		Debug.error {'$cmd' error: $result ($eo)}
		return [Http ServerError $r $result $eo]
	    }
	    Debug.wubtk {'$cmd' yielded: ($result)}
	    return $result
	} else {
	    Debug.wubtk {coroutine gone: $cmd}
	    variable tolerant
	    set app [string trimright $mount /]/
	    set extra [dict get? $r -extra]
	    if {$extra ne ""} {
		# this is an old invocation trying to get javascript
		return [Http Ok $r "window.location='$app';" application/javascript]
	    }

	    if {$tolerant} {
		return [Http Redirect $r $app]
	    } else {
		set msg [<p> "WubTk '$cmd' has terminated."]
		append msg [<p> [<a> href $app "Restart"]]
		return [Http Ok $r $msg]
	    }
	}
    }

    destructor {
	namespace delete Coros
    }

    superclass FormClass	;# allow Form to work nicely
    constructor {args} {
	variable tolerant 1
	variable {*}[Site var? WubTk]	;# allow .ini file to modify defaults
	variable lambda ""
	variable timeout 0
	variable icons /icons/
	variable theme dark
	variable spinner_size 20
	variable spinner_style "position: fixed; top:10px; left: 10px%;"
	variable {*}$args
	namespace eval [info object namespace [self]]::Coros {}
	if {[info exists file]} {
	    append lambda [fileutil::cat -- $file]
	} elseif {![info exists lambda] || $lambda eq ""} {
	    variable lambda [fileutil::cat -- [file join $::WubTk_dir test.tcl]]
	}

	next {*}$args
    }
}
