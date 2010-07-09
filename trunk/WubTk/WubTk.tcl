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
    # process request helper
    method do {r} {
	variable mount
	# calculate the suffix of the URL relative to $mount
	lassign [Url urlsuffix $r $mount] result r suffix path
	if {!$result} {
	    return [Httpd NotFound $r]	;# the URL isn't in our domain
	}

	Debug.wubtk {process '$suffix' over $mount}
	
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
	    namespace eval [namespace current]::Coros::$cmd $lambda	;# install the user code

	    Debug.wubtk {coroutine initialising - ($r) reply}
	    
	    set result [coroutine [namespace current]::Coros::${cmd}::_do ::apply [list {r} {
		set r [::yield]
		while {1} {
		    # decode response from client
		    Debug.wubtk {processing [info coroutine]}
		    set r [jQ jquery $r]
		    set r [jQ ready $r {
			$(".command").change(function callback(eventObject) {
			    .ajax({
				context: this,
				type: "GET",
				url: "command",
				data: {id: $(this).attr("id")},
				dataType: "script",
				success: function (data, textStatus, XMLHttpRequest) {
				    alert("moop: "+data);
				}
			    });
			});
		    }]

		    set content [grid render [namespace tail [info coroutine]]]
		    Debug.wubtk {render: $content}
		    dict set r -title [wm title]
		    set r [Http Ok $r $content x-text/html-fragment]

		    set r [::yield $r]	;# generate page
		}
	    } [namespace current]::Coros::$cmd] $r]

	    if {$result ne ""} {
		Debug.wubtk {coroutine initialised - ($r) reply}
		return $result	;# allow coroutine lambda to reply
	    } else {
		# otherwise simply redirect to coroutine lambda
		Debug.wubtk {coroutine initialised - redirect to ${mount}$cmd}
		return [Http Redirect $r [string trimright $mount /]/$cmd/]
	    }
	}

	set extra [lassign [split $suffix /] cmd]
	dict set r -extra [join $extra /]

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
	    return [Http NotFound $r [<p> "WubTk '$cmd' has terminated."]]
	}
    }

    destructor {
	namespace delete Coros
    }

    superclass FormClass	;# allow Form to work nicely
    constructor {args} {
	variable hint 1
	variable {*}[Site var? WubTk]	;# allow .ini file to modify defaults
	variable {*}$args
	namespace eval [info object namespace [self]]::Coros {}
	next {*}$args
    }
}
