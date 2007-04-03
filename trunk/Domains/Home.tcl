# Home -
#

package provide Home 1.0

package require Mason

::snit::type Home {
    component mason -inherit yes
    option -pubdir "public_html"

    method clean {rsp ursp} {
	# clean up the response dict - filter out any bad changes 
	# the user script made to the response dict
	set http [dict filter $ursp key {[A-Za-z]*}]
	set meta [dict filter $ursp script {k v} {
	    expr {
		  $k in {-fd -content -depends
		      -rtype -code
		      -cookies -dynamic}
	      }
	}]
	set rsp [dict merge $rsp $http $meta]

	#Debug.home {Home clean: $meta -> $rsp}
	return $rsp
    }

    method Respond {code rsp} {
	Debug.dispatch {HOME Respond $code [dumpMsg $rsp]}
	lassign [$mason wrap $code $rsp] code ursp
	$mason Respond $code [$self clean $rsp	$ursp]
    }

    method Dispatch {req} {
	Debug.dispatch {Dispatch}
	dict set req -domain $self
	dict lappend req -continuations [list $self Respond]

	dict set req -root [$mason cget -root]
	set req [$self auth $req]	;# run the mason domain auth

	# get user's home html dir
	set root [file split [dict get $req -suffix]]
	set suffix [file join {*}[lrange $root 1 end]]
	set root ~[lindex $root 0]
	set root [file join [file normalize $root] $options(-pubdir)]

	dict set req -root $root
	dict set req -suffix $suffix
	dict set ::response -safe 1

	# run the user's domain auth
	set req [$self auth $req]

	dict set req -root $root
	dict set req -suffix $suffix
	dict set ::response -safe 1
	puts stderr "Home $root - $suffix"
	$mason mason $req
	Debug.dispatch {Dispatched}
    }

    constructor {args} {
	install mason using Mason %AUTO% \
	    {*}$args -safe 1

	$self configurelist $args
    }
}
