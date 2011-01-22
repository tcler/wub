package require tdom
package require Url
package require Debug
Debug define oid_resolve 10
Debug define openid 10

package provide Openid 1.0

# OIdResolver - HTTP client which chases down redirects
oo::class create OIdResolver {
    method yadis {document} {
	Debug.oid_resolve {yadis $document}
	set want "http://specs.openid.net/auth/2.0/signon"
	set result {}
	set doc  [dom parse $document]
	set root [$doc documentElement]
	set node [[$root selectNodes /xrds:XRDS] firstChild]
	if {$node ne ""} {
	    foreach service [$node child all] {
		if {[$service nodeName] eq "Service"} {
		    set record {}
		    foreach el [$service child all] {
			dict set record [$el nodeName] [[$el firstChild] nodeValue]
		    }
		    if {[dict exists $record Type]
			&& [dict get $record Type] eq $want
		    } {
			dict set record priority [$service getAttribute priority 100000]
			dict unset record Type
			lappend result $record
		    }
		}
	    }
	}

	$doc delete

	Debug.oid_resolve {yadis -> $result}
	return $result
    }

    method respond {args} {
	variable consumer
	Debug.oid_resolve {respond: $consumer $args}
	after 0 [list {*}$consumer {*}$args]
	my destroy
    }

    method get {url {ctype application/xrds+xml}} {
	variable servers
	set parsed [Url parse $url]
	catch {dict unset parsed -fragment}
	set url [Url uri $parsed]
	set host [dict get $parsed -host]
	set accept [list accept $ctype]

	if {[dict exists servers $host]} {
	    Debug.oid_resolve {get: [dict get $servers $host] get $url}
	    [dict get $servers $host] get $url $accept
	} else {
	    dict set servers $host [HTTP new $url [list [self] got] get $accept]
	    Debug.oid_resolve {get new: [dict get $servers $host] get $url}
	}
    }

    method got {r} {
	variable consumer
	variable depth; incr depth
	variable maxdepth
	variable mode

	switch -glob -- [dict get? $r -code] {
	    3* {
		Debug.oid_resolve {got redirect: [dict get? $r -code]}
		if {$depth > $maxdepth} {
		    # redirection exceeded maximum depth
		    my respond resolve_error "$depth exceeded"
		} else {
		    # redirection - keep chasing
		    my get $url
		}
	    }

	    2* {
		# got content
		switch -- [dict get? $r content-type] {
		    application/xrds+xml {
			# the content is the YADIS document - parse it
			my respond resolve_xrds [my yadis [dict get $r -content]]
		    }

		    text/html {
			# parse the damn HTML
			set doc  [dom parse -html $html]
			set root [$doc documentElement]

			if {$mode eq "yadis"} {
			    Debug.oid_resolve {got HTML yardis}
			    # An HTML document with a <head> element
			    # that includes a <meta> element
			    # with http-equiv attribute X-XRDS-Location

			    set node [$root selectNodes {//meta[@http-equiv="X-XRDS-Location"]}]
			    if {$node ne ""} {
				set url [$node getAttribute content JUNK]
				if {$url ne "JUNK"} {
				    # chase down the XRDS document
				    $doc delete
				    my get $url
				    return	;# try yadis again
				}
			    }

			    Debug.oid_resolve {yardis failed}
			    set mode "html"	;# yadis failed
			}

			# yadis negotiation failed, do HTML-Based Discovery
			# Within the HEAD element of the document:
			# A LINK element MUST be included with attributes
			# "rel" set to "openid2.provider"
			# and "href" set to an OP Endpoint URL.
			set node [$root selectNodes {//link[@rel="openid2.provider"]}]
			if {$node eq ""} {
			    my respond resolve_error "HTML-Based Discovery - no LINK openid2.provider" $r
			    return
			} else {
			    set result [list op [$node getAttribute href]]
			}

			# A LINK element MAY be included with attributes
			# "rel" set to "openid2.local_id"
			# and "href" set to the end user's
			# OP-Local Identifier.
			set node [$root selectNodes {//link[@rel="openid2.local_id"]}]
			if {$node ne ""} {
			    lappend result op-local [$node getAttribute href]
			}

			# deliver the response, clean up
			my respond resolve_html $result
		    }

		    default {
			if {[dict exists $r x-xrds-location]} {
			    # http://yadis.org/wiki/Yadis_1.0_(HTML)
			    # 6.2.5 Response
			    # The response MUST be one of:
			    # HTTP response-headers that include
			    # an X-XRDS-Location response-header,
			    # together with a document
			    if {$depth > $maxdepth} {
				# redirection exceeded maximum depth
				my respond resolve_error "$depth exceeded"
			    } else {
				Debug.oid_resolve {x-xrds-location}
				my get [dict get $r x-xrds-location]
				return	;# try again
			    }
			} else {
			    my respond resolve_error "Unexpected response" $r
			}
		    }
		}
	    }

	    4* -
	    5* -
	    default {
		# error
		my respond resolve_error "HTTP failed" $r
	    }
	}
    }

    destructor {
	Debug.oid_resolve {destroying [self]}
	variable servers
	dict for {n v} $servers {
	    catch {$v destroy}
	}
    }

    constructor {_claimedID _consumer {_maxdepth 10}} {
	variable claimedID $_claimedID
	variable consumer $_consumer
	variable depth 0
	variable maxdepth $_maxdepth
	variable mode yadis	;# initially in yadis mode
	variable servers {}

	set parsed [Url parse $claimedID]
	catch {dict unset parsed -fragment}
	set claimedID [Url uri $parsed]

	my get $claimedID
    }
}

oo::class create OpenId {
    method yield {args} {
	my {*}[::yieldm $args]
    }

    method resolve {input} {
	if {[string match xri::// [string tolower $input]]} {
	    # If the user's input starts with the "xri://" prefix,
	    # it MUST be stripped off, so that XRIs are used
	    # in the canonical form.
	    set input [string range $input 6 end]
	}

	if {[string index $input 0] in {"=" "@" "+" "$" "!" "("}} {
	    # If the first character of the resulting string
	    # is an XRI Global Context Symbol 
	    # then the input SHOULD be treated as an XRI.

	    # If the identifier is an XRI, [XRI_Resolution_2.0] will yield
	    # an XRDS document that contains the necessary information.
	    # It should also be noted that Relying Parties can take advantage
	    # of XRI Proxy Resolvers, such as the one provided by XDI.org
	    # at http://www.xri.net.
	    # This will remove the need for the RPs to perform
	    # XRI Resolution locally.

	    # query http://xri.net/$input - that will give us the URL
	    variable xri_proxy
	    set input ${xri_proxy}$input

	    return [HTTP new $input [list [self] proxy]]
	} else {
	    # Otherwise, the input SHOULD be treated as an http URL;
	    # if it does not include a "http" or "https" scheme,
	    # the Identifier MUST be prefixed with the string "http://".
	    if {![string match http [string tolower $input]]
		&& ![string match https [string tolower $input]]} {
		set input http://$input
	    }

	    # If the URL contains a fragment part, it MUST be stripped off
	    # together with the fragment delimiter character "#"
	    set input [lindex [split $input "#"] 0]

	    # URL Identifiers MUST then be further normalized
	    # by both following redirects when retrieving their content
	    # and finally applying the rules in Section 6 of [RFC3986]
	    # to the final destination URL.
	    # This final URL MUST be noted by the Relying Party as
	    # the Claimed Identifier and be used when requesting authentication.

	    return [OIdResolver new $input [list [self] response]]
	    # the resultant URL, when followed,
	    # should result in a URL
	}
    }

    method resolve_error {args} {
	#
    }

    method resolve_xrds {xrds} {
    }

    method resolve_html {id} {
    }

    method reader {args} {
	variable claimedID
	variable resolver [my resolve $claimedID]
    }

    destructor {
	variable resolver
	catch {$resolver destroy}
    }

    constructor {args} {
	variable xri_proxy http://xri.net/
	variable {*}$args

	# create coros inside our ns
	set ns [info object namespace [self]]

	# create protocol coroutine
	variable reader ${ns}::${socket}R
	coroutine $reader [self] reader
	trace add command $reader delete [list [self] destroy]
    }
}
