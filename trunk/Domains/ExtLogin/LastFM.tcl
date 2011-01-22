# LastFM.tcl - a plugin for authentication against Last.FM

package require TclOO
namespace import oo::*

package require Debug

package require Cookies
package require HTTP
package require md5
package provide ExtLogin::LastFM 1.0

class create ::ExtLogin::LastFM {

    method sign_api_call args {
	if {[llength $args] == 1} {
	    lassign $args args
	}

	set sorted {}
	foreach key [lsort [dict keys $args]] {
	    lappend sorted $key [dict get $args $key]
	}

	set base [join $sorted ""]$secret

	return [::md5::md5 -hex $base]
    }

    method login {r args} {
	foreach {k v} $args {
	    set $k $v
	}
	set queryd [Query flatten [Query parse $r]]
	set token [dict get $queryd token]

	dict set request method auth.getSession
	dict set request token $token
	dict set request api_key $api_key
	dict set request api_sig [my sign_api_call $request]

	set urld [Url parse $api_url]
	dict set urld -query [Query encodeL $request]

	set V [HTTP new $api_url [lambda {r mount v} {
	    set result [dict get $v -content]
	    set key ""
	    regexp {<name>([^<]+)<} $result - username
	    regexp {<key>([^<]+)<} $result - key
	    # here we probably need to deXMLize username, and urlencode it below --- todo
	    dict set r set-cookie "token=lastfm.$key; domain=[dict get $r -host]; path=/"
	    return [Httpd Resume [Http Redirect $r [string trimright $mount /]/providers/LastFM/getinfo?username=$username Redirect]]
	} $r $mount] get [list [Url http $urld]]]

	return [Httpd Suspend $r 100000]
    }

    method getinfo {r args} {
	foreach {k v} $args {
	    set $k $v
	}
	set queryd [Query flatten [Query parse $r]]
	set username [dict get $queryd username]

	set urld [Url parse $api_url]
	dict set urld -query [Query encodeL [list api_key $api_key method user.getInfo user $username]]
	set V [HTTP new $api_url [lambda {r callback username cookie v} {
		set result [dict get $v -content]
		set img {http://cdn.last.fm/flatness/catalogue/noimage/2/default_user_small.png}
		regexp {<image>([^<]+)<} $result - img
		return [Httpd Resume [{*}$callback $r login cookie $cookie domain last.fm provider LastFM username $username image $img]]
	} $r $callback $username $cookie] get [list [Url http $urld]]]

	return [Httpd Suspend $r 100000]
    }

    method authenticate {r args} {
	return [Http Redirect $r http://www.last.fm/api/auth/?api_key=$api_key&cb=[dict get $r -scheme]://[dict get $r -host][string trimright $mount /]/providers/LastFM/login Redirect]
    }

    method info {} {
	return $info
    }

    variable mount api_url api_key secret callback info

    constructor {args} {
	dict set info name LastFM
	dict set info domain last.fm
	dict set info image-url http://www.last.fm/favicon.ico
	set api_url http://ws.audioscrobbler.com/2.0/

	foreach {n v} $args {
	    set $n $v
	}
    }
}


