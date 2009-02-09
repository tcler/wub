#! /usr/bin/env tclsh

# Single Threaded Simplistic Site
lappend auto_path [pwd]	;# path to the Site.tcl file
namespace eval Site {
    variable home [file dirname [info script]]
}

package require Site

# Start Site Server(s)
Site start
