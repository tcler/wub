# Wub test domain - some nice tests

package require snit
package provide WubTest 1.0

::snit::type WubTest {

    # test that system text is converted properly to html
    method /systext {req} {
	do respond Ok $req "title: System Text Test
	<h1>Test system text conversion</h1>
 	<p>The server should convert this to legal html</p>	" \
	    "text/x-system"
    }

    method /error {req} {
	set provoke_error = [expr {1/0}]
    }

    method Dispatch {req} {
	set http [dict get $req -http]
	set suffix [dict get $req -suffix]
	#puts stderr "WubTest $suffix"
	$self /$suffix $req
	return
    }

    constructor {args} {
	#$self configurelist $args
    }
}
