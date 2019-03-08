#! /usr/bin/env tclsh
lappend auto_path [file dirname [info script]]
package require Site
package provide Wub 5.0

# Initialize Site
set siteconf site.config
set conffile [lindex $argv 0]
if {[file exists $conffile]} {
	set siteconf $conffile
	set argv [lrange $argv 1 end]
}
Site init home [file normalize [file dirname [info script]]] config $siteconf debug 10 {*}$argv

Site start	;# start server(s)

# vim: ts=8:sw=4:noet
