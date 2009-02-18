#! /usr/bin/env tclsh
lappend auto_path [pwd]
package require Site

# Initialize Site
Site init home [file normalize [file dirname [info script]]] ini site.ini debug 10

# Start Site Server(s)
Site start 
