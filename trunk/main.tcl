# main.tcl - the Wub starkit startup
package require starkit
starkit::startup

set user_dir [pwd]	;# this is the dir from which the user ran the kit

# start up Wub
package require Site

if {![info exists $argv application]} {
    # Initialize Site
    Site init ini site.ini

    # Start Site Server(s)
    Site start 
}
