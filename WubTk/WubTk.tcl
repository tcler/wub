# WubTk.tcl - a domain built around coroutines from tcl8.6

package require Debug
Debug define wubtk 10
package require Http
package require md5

package require WubWidgets

package provide WubTk 1.0

set ::API(Domains/WubTk) {
    {WubTk - a Web emulation of Tk}
}

class create ::WubTk {
    method buttonJS {{what {$('.button')}}} {
	return [string map [list %B% $what] {
	    %B%.click(function () { 
		//alert($(this).attr("name")+" button pressed");
		$.ajax({
		    context: this,
		    type: "GET",
		    url: "button",
		    data: {id: $(this).attr("name")},
		    dataType: "script",
		    success: function (data, textStatus, XMLHttpRequest) {
			//alert("button: "+data);
		    }
		});
	    });
	}]
    }

    method variableJS {{what {$('.variable')}}} {
	return [string map [list %B% $what] {
	    %B%.change(function () {
		//alert($(this).attr("name")+" changed: " + $(this).val());
		$.ajax({
		    context: this,
		    type: "GET",
		    url: "variable",
		    data: {id: $(this).attr("name"), val: $(this).val()},
		    dataType: "script",
		    success: function (data, textStatus, XMLHttpRequest) {
			//alert("button: "+data);
		    }
		});
	    });
	}]
    }
    
    method commandJS {{what {$('.command')}}} {
	return [string map [list %B% $what] {
	    %B%.change(function callback(eventObject) {
		//alert($(this).attr("name")+" command invoked");
		$.ajax({
		    context: this,
		    type: "GET",
		    url: "command",
		    data: {id: $(this).attr("name")},
		    dataType: "script",
		    success: function (data, textStatus, XMLHttpRequest) {
			//alert("command: "+data);
		    }
		});
	    });
	}]
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
	    namespace eval [namespace current]::Coros::$cmd {
		gridC create grid	;# make per-coro single instance of grid
		wmC create wm		;# make per-coro single instance of wm
	    }

	    # install the user code in the coro's namespace
	    variable lambda
	    Debug.wubtk {coroutine initialising - ($r) reply}
	    
	    set result [coroutine [namespace current]::Coros::${cmd}::_do ::apply [list {r lambda} {
		eval $lambda	;# install the user code

		# initial client direct request
		Debug.wubtk {processing [info coroutine]}

		set r {}
		while {[dict size [set r [::yield $r]]]} {
		    # unpack query response
		    set Q [Query parse $r]; dict set r -Query $Q; set Q [Query flatten $Q]
		    Debug.wubtk {[info coroutine] Event: [dict get? $r -extra] ($Q)}
		    set err [catch {
			switch -- [dict get? $r -extra] {
			    button {
				set cmd .[dict Q.id]
				if {[llength [info commands [namespace current]::$cmd]]} {
				    Debug.wubtk {button $cmd}
				    $cmd command
				} else {
				    Debug.wubtk {not found button [namespace current]::$cmd}
				}
			    }
			    variable {
				set cmd .[dict Q.id]
				if {[llength [info commands [namespace current]::$cmd]]} {
				    Debug.wubtk {button $cmd}
				    $cmd var [dict Q.val]
				} else {
				    Debug.wubtk {not found button [namespace current]::$cmd}
				}
			    }
			    command {
			    }
			    default {
				# re-render whole page
				set r [jQ jquery $r]

				# send js to track widget state
				set r [jQ ready $r [my buttonJS]]
				set r [jQ ready $r [my variableJS]]
				set r [jQ ready $r [my commandJS]]

				set content [<div> id ErrDiv {}]
				append content [grid render [namespace tail [info coroutine]]]
				Debug.wubtk {render: $content}
				dict set r -title [wm title]
				set r [Http Ok $r $content x-text/html-fragment]
				continue
			    }
			}
		    } e eo]
		    if {$err == 4} continue

		    set result ""
		    foreach {id html type} [grid changes] {
			Debug.wubtk {changed id: $id type: $type}
			dict lappend classified $type $id
			append result [string map [list %ID% $id %H% $html] {
			    $('#%ID%').replaceWith("%H%");
			}]

			# send js to track widget state
			set jid "\$('#$id')"
			switch -- $type {
			    button {
				append result [my buttonJS $jid]
			    }
			    entry {
				append result [my variableJS $jid]
			    }
			}
		    }

		    if {$err} {
			set e [string map [list \" \\\" ' \\'] $e]
			append result [string map [list %C% $cmd %OP% [dict get? $r -extra] %E% $e] {
			    $('#ErrDiv').html('<p>%C% %OP% Error: %E% </p>');
			}]
		    } else {
			append result {
			    $('#ErrDiv').html('');
			}
		    }
		    
		    Debug.wubtk {SCRIPT: ($result)}
		    set r [Http Ok $r $result text/javascript]
		}
	    } [namespace current]::Coros::$cmd] $r $lambda]
	    
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
	    if {$tolerant} {
		return [Http Redirect $r [string trimright $mount /]/]
	    } else {
		return [Http NotFound $r [<p> "WubTk '$cmd' has terminated."]]
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
	variable {*}$args
	namespace eval [info object namespace [self]]::Coros {}
	next {*}$args
    }
}
