# Pool - snit wrapper around ::struct::pool
# adds dynamic object construction
#
# Dispatch method is also provided,
# so Pool functions as a domain dispatcher
#
# A pool of interps may be created by:
#	-constructor {interp create}
#	-destroy {eval exit}
#	-prefix {interp eval}
#	-infix Dispatch
#
# A pool of threads may be created by:

package provide Pool 1.0
package require snit
package require struct::pool

snit::type Pool {
    component pool -inherit true -public pool
    option -maxsize -default 30 \
	-cgetmethod getmax \
	-configuremethod setmax

    method getmax {option} {
	return [$pool maxsize]
    }
    method setmax {option value} {
	set options($option) $value
	$pool maxsize $value
    }

    # contained type and constructor args
    option -constructor {}
    option -prefix {}	;# prefix to make resource into an evaluable expression
    option -infix Dispatch	;# command to invoke within resource
    option -destroy destroy	;# destructor for items

    # variables responsible for tying resources to -http object
    variable tied -array {}	;# set of 'tied' resources
    option -tie 0		;# whether to tie resources by default

    # Respond --
    #
    #	handle response dispatch
    #
    # Arguments:
    #
    #	code	error code of the dispatch
    #	rsp	HTTP response from dispatch
    #
    # Results:
    # Side-Effects:
    #

    method Respond {code rsp} {
	set x [dict get $rsp -local$self]	;# get resource
	catch {
	    dict unset rsp -local$self	;# clean up local state
	}

	# tie/release the resource
	if {[dict exists $rsp -release]} {
	    set http [dict get $req -http]

	    # forget tied association between objects
	    catch {unset tied($http)}
	    catch {unset tied($x)}

	    {*}$http tie $x	;# forget the tie
	    $self release $x	;# release the pooled resource
	} elseif {[dict exists $rsp -tie]} {
	    # a response with 'tie' will associate this element to the server
	    set http [dict get $req -http]

	    # remember tied association between objects
	    set tied($http) $x
	    set tied($x) $http

	    {*}$http tie $x $self release $x	;# record a release callback
	}

	# rethrow the response
	return -code $code -response $rsp
    }

    method Dispatch {req} {
	# get a tied resource, or grab one from the pool
	if {[info exists tied([set http [dict get $req -http]])]} {
	    set x $tied($http)	;# use the previously tied resource
	} else {
	    set x [$self get]	;# get a pooled resource
	}

	dict set req -local$self $x	;# remember the resource for request

	# do we want to tie resources by default?
	if {$options(-tie)} {
	    # fill the request with options to tie it to http.  It will be tied
	    # unless it subsequently elects to be released
	    dict set req -tie 1
	    catch {dict unset req -release}
	} else {
	    # remove options to tie resoutece to http.  It will not be tied
	    # unless it subsequently elects to be tied
	    catch {dict unset req -tie}
	}

	# dispatch to pooled resource, calling back Request on completion
	do dispatch $req {*}$options(-prefix) {*}$x {*}$options(-infix)
    }

    method get {} {
	while {![info exists item]} {
	    if {![$pool request item]} {
		if {[$pool maxsize] > [$pool info cursize]} {
		    # create a new object on demand and put it into pool
		    set s [{*}$options(-constructor)]
		    #puts stderr "Constructed Pool $s: $options(-constructor)"
		    $pool add $s
		} else {
		    # we have exceeded per-listener pool size
		    error "Exhaustion [$pool maxsize] [$pool info cursize]"
		}
	    }
	}
	return $item
    }

    # discard --
    #
    #	destroy an unallocated item from the Pool

    method Discard {item} {
	$pool remove $item	;# will error if item is allocated
	$item $options(-destroy)
    }

    destructor {
	# first try to destroy each object
	foreach {item id} [$pool info allocstate] {
	    if {$id != -1} {
		$pool release $item
		$pool remove $item
	    }
	    catch {$item $options(-destroy)}
	}

	if {[catch {$pool destroy}]} {
	    $pool destroy -force
	}
    }

    constructor {args} {
	#puts stderr "Pool $self: $args [array get options]"
	if {[catch {
	    install pool using ::struct::pool [namespace tail ${self}]Pool
	    $self configurelist $args
	    $pool maxsize $options(-maxsize)
	} result eo]} {
	    puts stderr "Pool constructor error $self: $result ($eo)"
	} else {
	    #puts stderr "Pool $self: [array get options]"
	}
    }
}
