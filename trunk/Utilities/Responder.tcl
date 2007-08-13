package provide Responder 1.0

namespace eval Responder {
    # process - evaluate a script in the context of a request
    # generate errors as ServerError responses
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

    # dispatch - wrap a switch command so errors are handled as 500 responses
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

	    3 { # break
		return -code break
	    }

	    4 { # continue
		return -code continue
	    }
	}
    }

    # post - process outgoing response request
    proc post {req} {
	#return [process $rsp convert do $rsp]
	return $req
    }

    proc Incoming {new args} {
	inQ put $new	;# add the incoming request to the inQ

	# while idle and there are new requests to procecss
	variable working	;# set while we're working
	while {!$working && ![catch {inQ get} req eo]} {
	    set working 1
	    catch {uplevel 1 [list switch {*}$args]} r eo
	    Debug.socket {Dispatcher: $eo}
	    switch [dict get $eo -code] {
		0 -
		2 { # ok - return
		    if {![dict exists $r -code]} {
			set rsp [Http Ok $r]
		    } else {
			set rsp $r
		    }
		}
		
		1 { # error
		    set rsp [Http ServerError $req $r $eo]
		}
		
		3 { # break
		    set working 0
		    break
		}
		    
		4 { # continue
		    set working 0
		    continue
		}
	    }

	    if {[catch {post $rsp} r eo]} { ;# postprocess response
		set rsp [Http ServerError $rsp $r $eo]
	    } else {
		set rsp $r
	    }

	    #puts stderr "RESPONSE: $rsp"
	    Send $rsp 		;# send response
	    set working 0	;# go idle
	}
    }

    variable working 0	;# set while we're working
    ::struct::queue inQ	;# create a queue of pending work

    # configure - configure namespace
    proc configure {args} {
	variable {*}$args
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
