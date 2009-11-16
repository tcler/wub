#! /usr/bin/env tclsh

if {0} {
    lappend auto_path [file dirname [pwd]]/Utilities/
    package require Package	;# start the cooption of [package]
}
lappend auto_path [pwd]
package require Site

# Initialize Site
Site init home [file normalize [file dirname [info script]]] ini site.ini debug 10

# Start Site Server(s)
Site start 
