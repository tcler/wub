# Prefix - handle a domain by applying a prefix to the req

package provide Prefix 1.0

package require snit

::snit::type Prefix {
    option -prefix ""

    method Dispatch {req} {
	Debug.dispatch {Dispatch}
	{*}$options(-prefix) $req
	Debug.dispatch {Dispatched}
    }

    # constructor --
    #
    #	Initialise prefix domain handling
    #
    # Arguments:
    #
    #	args	parameter names and value
    #
    # Results:
    # Side-Effects:
    #
    
    constructor {args} {
	if {[catch {
	    puts stderr "Prefix constructor: $args"
	    $self configurelist $args
	} result eo]} {
	    puts stderr "Prefix constructor failed: $result ($eo)"
	}
    }
}

if {0 && [info exists argv0] && ($argv0 eq [info script])} {
    lappend auto_path [pwd]

    set sd [Prefix %AUTO% -prefix]
}
