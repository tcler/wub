#! /usr/bin/env tclsh

# Single Threaded Minimal File Server Site
lappend auto_path [pwd]	;# path to the Site.tcl file
namespace eval Site {
    variable varnish {}
    variable home [file dirname [info script]]
}

package require Site

# This is a File domain which serves simple files
# from the eponymous directory.  Each file served has an expiry
# date as indicated, which allows caching.

# File domain is the most minimal, but Mason is better
# File docroot -root $::Site::docroot
Mason wub -url / -root $docroot -auth .before -wrapper .after

# [Responder Incoming] provides a safe wrapper and switch-like
# dispatcher for incoming requests.
proc Responder::do {req} {
    switch -glob -- [dict get $req -path] {
	/ -
	/* {
	    # redirect / to /wub
	    return [docroot do $req]
	}
    }
}

#### Incoming - handle of incoming request
#
# This proc is called with a fully parsed incoming request.
# It uses Responder to wrap a tcl's [switch] dispatch command,
# and wrap errors in an appropriate HTML response.

proc Incoming {req} {
    return [Responder Process $req]
}

# Start Site Server(s)
Site start
