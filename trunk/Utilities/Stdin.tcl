package provide Stdin 1.0

proc Stdin {{prompt "% "}} {
    global Stdin

    if {[eof stdin]} {
	fileevent stdin readable {}
	return
    }

    append Stdin(command) [gets stdin]
    if {[info complete $Stdin(command)]} {
	if {[catch {uplevel \#0  $Stdin(command)} result eo]} {
	    puts -nonewline "$result ($eo)\n$prompt"
	} else {
	    puts -nonewline "$result\n$prompt"
	}
	flush stdout
	set Stdin(command) ""
    } else {
	append Stdin(command) \n
    }
}
fileevent stdin readable Stdin
