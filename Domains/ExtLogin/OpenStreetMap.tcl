# OpenStreetMap.tcl - a plugin for authentication against OpenStreetMap

package require TclOO
namespace import oo::*

package require Debug

package require Cookies
package require HTTP
package provide ExtLogin::OpenStreetMap 1.0

class create ::ExtLogin::OpenStreetMap {

    method login {r args} {
	foreach {k v} $args {
	    set $k $v
	}
	lassign [string map {&secret= { }} $cookie] token secret
	set reqmethod GET
	set url http://api.openstreetmap.org/api/0.6/user/details
	set query {}
	set d [$oauth sign_request reqmethod $reqmethod url $url query $query provider_name $oauth_provider token $token token_secret $secret authtype header]
	foreach {k v} $d {
	    set $k $v
	}
	set V [HTTP new $url [lambda {r d cookie callback v} {
	    set result [dict get $v -content]
	    set username anonymous
	    set img http://www.openstreetmap.org/images/anon_large.png
	    regexp {display_name="([^\x22]+)"} $result - username
	    regexp {img href="([^\x22]+)"} $result - img
	    # here we probably need to deXMLize both username and img --- todo
	    return [Httpd Resume [{*}$callback $r login cookie $cookie domain openstreetmap.org provider OpenStreetMap username $username image $img]]
	} $r $d $cookie $callback] [string tolower $reqmethod] [list [Url http $urld] $entity {*}$headers]]

	return [Httpd Suspend $r 100000]
    }

    method authenticate {r args} {
	return [Http Redirect $r [string trimright $oauth_mount /]/?provider=$oauth_provider&return=[string trimright $mount /]/providers/OpenStreetMap/login Redirect]
    }

    method info {} {
	return $info
    }

    variable mount oauth_provider oauth oauth_mount callback info

    constructor {args} {
	dict set info name OpenStreetMap
	dict set info domain openstreetmap.org
	dict set info image-url http://openstreetmap.org/favicon.ico

	foreach {n v} $args {
	    set $n $v
	}
    }
}


