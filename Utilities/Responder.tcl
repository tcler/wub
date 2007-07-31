package provide Responder 1.0

namespace eval Responder {
    # process - evaluate a script in the context of a request
    proc process {req args} {
	set code [catch {{*}$args} r eo]
	switch -- $code {
	    1 {
		return [Http ServerError $req $r $eo]
	    }
	    default {
		set req $r
		if {$code == 0} {
		    set code 200
		}
		if {![dict exists $req -code]} {
		    dict set req -code $code
		}
		Debug.wikit {Response code: $code / [dict get $req -code]}
		return $req
	    }
	}
    }

    proc dispatch {req args} {
	catch {
	    uplevel 1 switch $args
	} r eo

	switch [dict get $eo -code] {
	    0 -
	    2 { # ok - return
		if {![dict exists $r -code]} {
		    set r [Http Ok $r]
		}
		return $r
	    }
	    
	    1 { # error
		return [Http ServerError $req $r $eo]
	    }

	    3 -
	    4 { # break & continue
		return -options $eo
	    }
	}
    }

    variable working 0
    variable pre
    variable post

    proc init {args} {
	variable {*}$args
    }

    proc Incoming {new args} {
	inQ put $new	;# add the incoming request to the inQ

	# while idle and there are new requests to procecss
	variable working
	variable pre
	variable post
	while {!$working && ![catch {inQ get} req eo]} {
	    set working 1
	    dict set req -cookies [Cookies parse4server [Dict get? $req cookie]]

	    # get a plausible prefix/suffix split
	    set response [uplevel 1 [list dispatch $req {*}$args]]
	    set rsp [process $rsp convert do $rsp]
	    Send $rsp ;# send response

	    set working 0	;# go idle
	}
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
