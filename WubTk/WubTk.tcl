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
		    },
		    error: function (xhr, status, error) {
			alert("ajax fail:"+status);
		    }
		});
	    });
	}]
    }

    method cbuttonJS {{what {$('.cbutton')}}} {
	return [string map [list %B% $what] {
	    %B%.click(function () { 
		//alert($(this).attr("name")+" cbutton pressed");
		$.ajax({
		    context: this,
		    type: "GET",
		    url: "cbutton",
		    data: {id: $(this).attr("name"), val: $(this).val()},
		    dataType: "script",
		    success: function (data, textStatus, XMLHttpRequest) {
			//alert("cbutton: "+data);
		    },
		    error: function (xhr, status, error) {
			alert("ajax fail:"+status);
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
		    },
		    error: function (xhr, status, error) {
			alert("ajax fail:"+status);
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
		    },
		    error: function (xhr, status, error) {
			alert("ajax fail:"+status);
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
		if {[catch {
		    eval $lambda	;# install the user code
		} e eo]} {
		    return [Http ServerError $r $e $eo]
		}

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
			    cbutton {
				set cmd .[dict Q.id]
				if {[llength [info commands [namespace current]::$cmd]]} {
				    Debug.wubtk {button $cmd}
				    $cmd cbutton [dict Q.val]
				} else {
				    Debug.wubtk {not found cbutton [namespace current]::$cmd}
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
				set r [jQ ready $r [my cbuttonJS]]
				set r [jQ ready $r [my variableJS]]
				set r [jQ ready $r [my commandJS]]

				set content [grid render [namespace tail [info coroutine]]]
				append content [<div> id ErrDiv {}]
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
			set html [string map {\n \\n} $html]
			append result [string map [list %ID% $id %H% $html] {
			    $('#%ID%').replaceWith("%H%");
			}]

			# send js to track widget state
			set jid "\$('#$id')"
			switch -- $type {
			    button {
				append result [my buttonJS $jid]
			    }
			    checkbutton {
				append result [my buttonJS $jid]
			    }
			    entry -
			    text {
				append result [my variableJS $jid]
			    }
			}
		    }

		    if {$err} {
			set e [string map [list \" \\\" ' \\'] $e]
			append result [string map [list %C% $cmd %OP% [dict get? $r -extra] %E% $e] {
			    $('#ErrDiv').html('<p>%OP% %C% Error: %E% </p>');
			}]
			Debug.wubtk {Error: $e ($eo)}
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
	variable lambda ""
	variable {*}$args
	namespace eval [info object namespace [self]]::Coros {}

	if {[info exists file]} {
	    append lambda [fileutil::cat -- $file]
	}

	next {*}$args
    }
}

if {0} {
    # Here's a demo, put it in local.tcl

    lappend ::auto_path [file join [file dirname [file dirname [file normalize [info script]]]] WubTk]

    package require WubTk
    Debug on wubwidgets 10
    Debug on wubtk 10

    Nub domain /tk/ WubTk lambda {
	proc buttonA args {
	    set bText [.b cget -text]
	    set aText [.a cget -text]
	    set bBg [.b cget -background]
	    set aBg [.a cget -background]
	    set bFg [.b cget -foreground]
	    set aFg [.a cget -foreground]
	    .a configure -text $bText -foreground $bFg -background $bBg
	    .b configure -text $aText -foreground $aFg -background $aBg
	}

	proc buttonC args {
	    global joe
	    set joe "${joe}A"
	    if {$joe != "bob" && [string first Mod [wm title .]] == -1} {
		wm title . "[wm title .] - Text Modified"
	    }
	}

	proc buttonH {sobj dobj} {
	    set text [$sobj get 0.0 end]
	    set output ""
	    foreach word [split $text] {
		for {set i 0} {$i<[string length $word]} {incr i} {
		    set char [string index $word $i]
		    append output "$char[string tolower $char$char]"
		}
		append output " "
	    }
	    $dobj configure -text $output
	    wm title . "Done it!"
	}

	proc showCode {} {
	    .g delete 0.0 end
	    
	    set err [catch {set fh [open test1.tk RDONLY]}]
	    if {!$err} {
		set code [read $fh]
		close $fh
		.g insert end $code
		.i configure -text "Cut n paste the above code into wish to compare!" -foreground orange -background black
	    } else {
		.i configure -text "Source code not in current directory!" -foreground orange -background black
	    }
	}

	wm title . "Demo #1"
	button .a -text "Press Me" -command buttonA -background lightgreen -foreground purple
	button .b -text "Don't Press Me" -command buttonA -background pink -foreground blue
	button .c -text "Modify" -command buttonC
	entry .d -textvariable joe
	label .e -text "Text:"
	button .f -text Logout -command exit
	text .g -background lightyellow
	label .i -text ""
	#button .h -text "Do it" -command "buttonH .g .i" -background violet
	button .h -text "Clear" -command {.g delete 0.0 end; .i configure -text ""} -background violet
	button .j -text "Show Code" -command "showCode" -background lightblue

	set joe bob

	grid configure .a -column 0 -row 0
	grid configure .b -column 1 -row 0
	grid configure .c -column 0 -row 1
	grid configure .e -column 0 -row 2
	grid configure .d -column 1 -row 2
	grid configure .f -column 2 -row 0 -columnspan 1
	grid configure .g -column 0 -row 4 -columnspan 3
	grid configure .i -column 0 -row 5 -columnspan 3
	grid configure .h -column 0 -row 6 -columnspan 1
	grid configure .j -column 2 -row 6 -columnspan 1
    }
}
