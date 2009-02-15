# main.tcl - the Wub starkit startup
package require starkit
starkit::startup

set userdir [pwd]	;# this is the dir from which the user ran the kit

# start up Wub
package require Site

if {![dict exists $argv application]} {
    # Initialize Site
    Site init ini site.ini userdir $userdir

    # Start Site Server(s)
    Site start
}
