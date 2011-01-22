# OpenId - openid client and server
#
# useful for authentication

package require sha1
package require HTTP

package provide OpenId 1.0

namespace eval OpenId {

    # protocol messages are mappings of plain-text keys to plain-text values in UTF-8
    # Messages MUST NOT contain multiple parameters with the same name.

    # A message in Key-Value form is a sequence of lines.
    # Each line begins with a key, followed by a colon, and the value associated with the key.
    # The line is terminated by a single newline (UCS codepoint 10, "\n").
    # A key or value MUST NOT contain a newline and a key also MUST NOT contain a colon.
    # Additional characters, including whitespace, MUST NOT be added before or after the colon
    # or newline.
    # The message MUST be encoded in UTF-8 to produce a byte string.
    # Key-Value Form encoding is used for signature calculation and for direct responses
    # to Relying Parties.
    proc decode_KV {text} {
	set dict {}
	foreach line [split $text \n] {
	    set value [join [lassign [split $line :] key] :]
	    dict set dict $key $value
	}
	return $dict
    }

    proc encode_KV {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	set result {}
	dict for {k v} $args {
	    lappend result "$k:$v"
	}
	return [join $result \n]
    }

    proc encode_HTTP {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	set dict {}
	dict for {k v} $args {
	    if {[string match -* $k]} continue
	    if {![string match openid.* $k]} {
		dict set dict openid.$k $v
	    } else {
		dict set dict $k $v
	    }
	}
	dict set dict openid.ns {http://specs.openid.net/auth/2.0}
	if {![dict exists $args openid.mode]} {
	    error "Must specify openid.mode"
	}

	return [Query encodeL $dict]
    }

    # Arbitrary precision integers encoded as big-endian signed two's complement binary strings.
    # "btwoc" is a function that takes an arbitrary precision integer and returns its shortest
    # big-endian two's complement representation.
    # All integers that are used with Diffie-Hellman Key Exchange are positive.
    # This means that the left-most bit of the two's complement representation MUST be zero.
    # If it is not, implementations MUST add a zero byte at the front of the string. 
    proc btwoc_encode {int} {
	return [string trimleft [binary format W* $int] \0]
    }

    proc btwoc_decode {string} {
	binary scan W* $string result
	return $result
    }

    # The end user's input identifier MUST be normalized into an Identifier, as follows:

    # 1. If the user's input starts with the "xri://" prefix, it MUST be stripped off,
    # so that XRIs are used in the canonical form.

    # 2. If the first character of the resulting string is an XRI Global Context Symbol
    # ("=", "@", "+", "$", "!") or "(", then the input SHOULD be treated as an XRI.

    # 3. Otherwise, the input SHOULD be treated as an http URL;
    # if it does not include a "http" or "https" scheme, the Identifier MUST be prefixed with
    # the string "http://".
    # If the URL contains a fragment part, it MUST be stripped off

    # 4. URL Identifiers MUST then be further normalized by both following redirects
    # when retrieving their content and finally applying the rules
    # in Section 6 of [RFC3986] (Berners-Lee, T., “Uniform Resource Identifiers (URI):
    # to the final destination URL.
    # This final URL MUST be noted by the Relying Party as the Claimed Identifier and be
    # used when requesting authentication.

    proc normalize {id} {
	if {[string match xri:* $id]} {
	    set id [string range $id 4 end]
	}
	if {[string match $id {[=@+$!\(]*} $id]} {
	    # it's an XRI
	} else {
	    set idd [Url parse $id]
	    if {[dict exists $idd -fragment]} {
		dict unset idd -fragment
	    }
	    set ci [Url uri $idd]
	}
    }

    proc query {r} {
	corovars query
	set query [Query flatten [Query parse $r]]
    }

    proc expect {args} {
	corovars query
	set mode [dict get? $query mode]
	if {$mode in $args} {
	    return $mode
	} else {
	    error "OpenId expected one of ($args) got '[dict get? $query mode]'"
	}
    }

    proc pack {args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	set result {}
	dict for {n v} $args {
	    lappend result openid.$n:$v
	}
	return "[join $result]\n"
    }

    # yield wrapper with command dispatcher
    proc yield {{retval ""}} {
	corovars connection
	while {1} {
	    set yield [::yield $retval]
	    Debug.coco {yield ($retval) -> ($yield)}
	    lassign $yield cmd args
	    switch -- $cmd {
		kill {
		    return -level [expr {[info level] - 1}] $args	;# return to the top coro level
		}

		EOF -
		TIMEOUT {
		    lassign $args conn
		    if {[info exists connection($conn)]} {
			unset connection($conn)	;# just peacefully forget about connection
		    }
		}

		call -
		default {
		    return $args
		}
	    }
	}
    }

    # fetch a URL
    proc fetch {$r url op args} {
	corovars connection
	set purl [Url parse $url]
	dict set urls $url 1

	# default content-type
	if {[dict exists $args -entity] && ![dict exists $args content-type]} {
	    dict set args content-type text/plain
	}

	# fetch the openid_url
	set writer [HttpC connect [info coroutine] $url -direct 1]
	set connection($writer) $writer
	set retries 0
	set retval [Http Suspend $r]	;# suspend the client connection
	while {1} {
	    $writer [list $op [list $url {*}$args]]	;# send to the writer
	    lassign [::yield $retval] cmd args; set retval ""
	    switch -- $cmd {
		TIMEOUT -
		EOF {
		    set conn [lindex $args 0]
		    catch {unset connection($conn)}
		    if {$conn eq $writer} {
			if {[incr retries] < $::OpenId::retries} {
			    set writer [HttpC connect [info coroutine] $url -direct 1]
			} else {
			    error "Run out of retries connecting to $url"
			}
		    } else {
			# ignore it
		    }
		}

		INCOMING {
		    set conn [lindex $args 0]
		    if {$conn ne $writer} {
			error "Got INCOMING from $conn, expected it from $writer"
		    }
		    set r $args
		    switch -glob -- [dict get $r -code] {
			1* {
			    continue	;# just ignore these
			}
			2* {
			    return $r	;# we've got the content we wanted
			}
			3* {
			    # redirects - follow them down
			    set nurl [dict get $r location]
			    set nup [Url parse $url]
			    if {![Url samehost $nup $purl]} {
				catch {$writer kill}
				set writer [HttpC connect [info coroutine] $nurl -direct 1]
			    }

			    # keep track of redirect cycles
			    dict incr urls $nurl 1
			    if {[dict get $urls $nurl] > 1} {
				error "Cycle around ([dict keys $urls])"
			    }

			    set url $nurl
			}

			4* -
			5* {
			    error "Got code [dict get $r -code] ([dict get $r -message]) while fetching $url. ($r)"
			}
		    }
		}
		
		call -
		default {
		    error "Got $cmd $args when expecting response from $openid_url"
		}
	    }
	}
    }

    proc scrape {page} {
	# find <link rel="openid.server" href="SERVER"> in the content
	# find <link rel="openid.delegate" href="DELEGATE"> in the content
	set dict {}
	set tree [::struct::tree "tree[info coroutine]"]
	::htmlparse::2tree $page $tree
	foreach node [$tree children [lambda {tree node} {
	    return [expr {[$tree get $node type] eq "link"}]
	}]] {
	    set attrs [Html parseAttr [$tree get $node data]]
	    if {[dict exists $attrs rel]} {
		set rel [dict get $attrs rel]
		if {$rel in {server delegate}} {
		    dict set dict [lindex [split $rel .] 1] [dict get $attrs href]
		}
	    }
	}
	$tree destroy
	return $dict
    }

    variable association	;# associations between OP and us

    # Establish a shared secret between Consumer and Identity Provider. 
    proc associate {r args} {
	lappend dict mode "associate"
	lappend dict assoc_type "HMAC-SHA1"
	lappend dict session_type ""
	lappend dict dh_modulus [base64 [btwoc $p]]
	lappend dict dh_gen [base64 [btwoc $g]]	;# Default: g = 2
	return [pack $dict]
    }

    # Ask an Identity Provider if a End User owns the Claimed Identifier, willing to wait for the reply.
    # The Consumer will pass the User-Agent to the Identity Provider for a short period
    # of time which will return either a "yes" or "cancel" answer. 
    proc checkid_setup {r} {
	lappend dict mode "checkid_setup"
	lappend dict identity [dict get $args identity]
	lappend dict return_to [Url redir $r [file join $mount logged_in] nonce [nonce $r]]
	return $dict
    }

    proc checkid_immediate {r args} {
	dict set args mode "checkid_immediate"
    }

    proc check_authentication {r args} {
	lappend dict mode "check_authentication"
	foreach v {assoc_handle sig signed} {
	    lappend dict $v [dict get $args openid.$v]
	}
	foreach v [split [dict get $args signed] ,] {
	    if {$v eq "mode"} continue
	    lappend dict $v [dict get $args $v]
	}
	return [pack $dict]
    }

    proc unpack {a args} {
	set dict {}
	foreach id $args {
	    if {[dict exists $a openid.$id]} {
		dict set dict $id [dict get $a openid.$id]
	    }
	}
	return $dict
    }

    proc +associate {r args} {
	# unpack request
	set dict [unpack $args assoc_type session_type dh_modulus dh_gen dh_consumer_public]
	dict with $dict {}

	# generate the association
	lassign [generate_association $r $assoc_type] assoc_handle expires_in secret

	lappend result assoc_type $assoc_type
	lappend result assoc_handle $assoc_handle
	lappend result expires_in $expires_in
	lappend result mac_key [base64 $secret]

	return [Http NoCache [Http Ok $r [pack $result] text/plain]]

	# openid.assoc_type: "HMAC-SHA1"
	# openid.session_type: Blank or "DH-SHA1" Default: Blank. (cleartext)
	# openid.dh_modulus: base64(btwoc(p))
	# openid.dh_gen: base64(btwoc(g)) Default: g = 2
	#   Only if using DH-SHA1 session_type. Should be specified if openid.dh_modulus is specified.
	# openid.dh_consumer_public: base64(btwoc(g ^ x mod p)) REQUIRED if using DH-SHA1 session_type.

	# Response format: Key-Value Pairs
	# assoc_type: The association type for the returned handle.
	# 	The only current mode is HMAC-SHA1, and all Consumers MUST support it.
	#	When caching, the Consumer MUST map an assoc_handle to both its secret and its assoc_type.
	# assoc_handle: The association handle to be provided in future transactions.
	# 	Consumers MUST NOT reuse this association handle after the corresponding expires_in value.
	# expires_in: The number of seconds this association handle is good for in base10 ASCII.
	# session_type: The encryption mode that the Provider chose. MAY be blank, absent, or "DH-SHA1".
	# dh_server_public: base64(btwoc(g ^ y mod p))
	#	The Provider's Diffie-Hellman public key
	# enc_mac_key: base64(SHA1(btwoc(g ^ (xy) mod p)) XOR secret(assoc_handle))
	# 	The encrypted shared secret, if using DH-SHA1.
	# mac_key: base64(secret(assoc_handle))
	#	The plaintext shared secret, if not using DH-SHA1.
    }

    # Asked if an EndUser owns the Identifier
    # return either a "yes" or "cancel" answer.
    proc +checkid_setup {r args} {
	# unpack request
	set dict [unpack $args identity assoc_handle return_to trust_root]
	dict with dict {}

	# go through some kind of authentication process
	# ...

	# process request
	variable ours
	if {$cancelled} {
	    lappend result mode cancel
	} else {
	    lappend result mode id_res
	    lappend result identity $identity
	    lappend result assoc_handle $assoc_handle
	    lappend result return_to $return_to
	    lappend result signed [join $signed ,]
	    
	    foreach sign $signed {
		lappend token $sign [set $sign]
	    }
	    lappend result sig [base64 [HMAC [dict get $ours $assoc_handle secret] [pack $token]]]
	}

	# ensure the association isn't stale
	if {[info exists assoc_handle] && ![dict exists $ours $assoc_handle]} {
	    lappend result invalidate_handle $assoc_handle
	}

	# this sends the client back to the consumer with some data
	return [Http Redir $r $return_to "" "" {*}$result]

	# openid.identity: Claimed Identifier
	# openid.assoc_handle: The assoc_handle from the associate request.
	#	Optional; Consumer MUST use check_authentication if an association handle
	#	isn't provided or the Identity Provider feels it is invalid.
	# openid.return_to: URL where the Provider SHOULD return the User-Agent back to.
	# openid.trust_root: URL the Provider SHALL ask the End User to trust.
	# 	Default: return_to URL
	#	Optional: the URL which the End User SHALL actually see to approve.

	# Response format: query string arguments
	# Always Sent
	# openid.mode: "id_res" or "cancel"

	# Sent on Positive Assertion
	# openid.identity: Verified Identifier
	# openid.assoc_handle: Opaque association handle being used to fine the HMAC key for the signature.
	# openid.return_to: Verbatim copy of the return_to URL parameter sent in the request
	#	before the Provider modified it.
	# openid.signed: Comma-seperated list of signed fields.
	#	Fields (without the "openid." prefix) that the signature covers.
	# openid.sig: base64(HMAC(secret(assoc_handle), token_contents)
	#	Where token_contents is a key-value format string of all the signed keys and values
	#	in this response. They MUST be in the same order as listed in the openid.signed field.
	#	Consumer SHALL recreate the token_contents string prior to checking the signature.
	# openid.invalidate_handle: (Optional) association handle sent in the request
	#	if the Provider did not accept or recognize it.
    }

    # Asked if EndUser owns the ClaimedIdentifier, give back an immediate "yes" or "can't say" answer.
    # Flow: Consumer -> User-Agent -> IdP -> User-Agent -> Consumer
    proc +checkid_immediate {r args} {
	foreach id {identity assoc_handle return_to trust_root} {
	    if {[dict exists $args openid.$id]} {
		set $id [dict get $args openid.$id]
	    }
	}

	variable ours	;# our associations
	if {[info exists $assoc_handle]} {
	    #	Consumer MUST use check_authentication if an association handle
	    #	isn't provided or the Identity Provider feels it is invalid.
	    if {![info exists ours($assoc_handle)]} {
		lappend result openid.mode id_res 
		lappend result openid.invalidate_handle $assoc_handle
		return [Http NoCache [Http Ok $r [pack $result] text/plain]]
	    }
	}

	# openid.identity: Claimed Identifier
	# openid.assoc_handle: The assoc_handle from the associate request.
	#	Optional Consumer MUST use check_authentication if an association handle
	#	isn't provided or the Identity Provider feels it is invalid.
	# openid.return_to: URL where the Provider SHOULD return the User-Agent back to.
	# openid.trust_root: URL the Provider SHALL ask the End User to trust.
	#	Default: return_to URL

	# openid.mode: "id_res"
	# openid.user_setup_url: URL to redirect User-Agent to so the End User can do whatever's necessary to fulfill the assertion. on failed assertion

	# openid.identity: Verified Identifier
	# openid.assoc_handle: Opaque association handle being used to find the HMAC key for the signature.
	# openid.return_to: Verbatim copy of the return_to URL parameter sent in the request, before the Provider modified it.
	# openid.signed: Comma-seperated list of signed fields.
	# openid.sig: base64(HMAC(secret(assoc_handle), token_contents)
	# openid.invalidate_handle: Optional; The association handle sent in the request if the Provider did not accept or recognize it.
    }

    proc +check_authentication {r args} {
	foreach id {assoc_handle sig signed invalidate_handle} {
	    if {[dict exists $args openid.$id]} {
		set $id [dict get $args openid.$id]
	    }
	}

	lappend result openid.mode id_res
	lappend result is_valid [expr {$valid?"true":"false"}]
	# invalidate_handle: opaque association handle
	# 	If present, the Consumer SHOULD uncache the returned association handle.

	return [Http NoCache [Http Ok $r [pack $result] text/plain]]

	# openid.assoc_handle: association handle from checkid_setup or checkid_immediate response.
	# openid.sig: signature to verify.
	# openid.signed: The list of signed fields to verify the signature of.
	# openid.*: response parameters from the openid.signed list
	# openid.invalidate_handle: (Optional) association handle returned via invalidate_handle.

	# Generate Response
	# openid.mode:"id_res"
	# is_valid: "true" or "false"
	# invalidate_handle: opaque association handle
	# 	If present, the Consumer SHOULD uncache the returned association handle.
    }


    variable mount
    variable logo

    # +login - we've got an openid from the Client
    proc +login {r args} {
	# clean up the openid_url
	set openid_url [dict get $args openid_url]
	set url [normalize $openid_url]
	set rsp [fetch $r $url get]	;# try to get the URL given as identity
	if {[dict get $rsp content-type] ne "text/html"} {
	    error "unexpected content-type [dict get $rsp content-type] fetching $url"
	}

	dict with [scrape [dict get $rsp -entity]] {}

	if {![info exists delegate]} {
	    set delegate $url
	}
	if {![info exists server]} {
	    error "entity at $url doesn't contain a server"
	}
	set serverP [Url parse $server]
	if {![dict exists $serverP -host]} {
	    error "server $server for id $url does not have a host"
	}

	proc entity {text} {
	    set result {}
	    foreach line [split $text \n] {
		set val [join [lassign [split $line :] var] :]
		dict set result $var $val
	    }
	    return $result
	}

	# get an association for the server (if possible)
	variable association association
	if {![info exists association($server)]} {
	    if {![catch {
		# try to get an association with this server
		fetch $r $url post -entity [associate $r]
	    } rsp eo]} {
		# we have an association
		set association($server) [entity [dict get $rsp -entity]]1
	    } else {
		# no association.  Ok, let's proceed by being dumb.
		variable mount
		set dict {openid.mode checkid_setup}
		lappend dict openid.identity $openid_url
		set rd [file join $mount logged_in]
		lappend dict openid.return_to [Url redir $r $rd nonce [nonce $r]]
		return [Http Redir $r $server "" "" {*}$dict]
	    }
	}

	# redirect the client to the delegate with its url
    }

    proc loginform {} {
	variable mount
	variable logo
        return [<form> oilogin action [file join $mount login] {
	    [<hidden> openid.mode login]
	    [<submit> submit src [file join $mount $logo]] [<text> openid_url title "OpenId Url"]
	}]
    }

    proc init {args} {
	variable {*}$args
	variable mount
	Coco init $mount {r {
	    while {1} {
		query $r
		set mode [dict get? $query openid.mode]
		if {[info commands ::OpenId::+$mode]} {
		    ::OpenId::yield [::OpenId::+$mode $r {*}$query]
		} else {
		    # emit login form, expect login
		    set r [yield [Httpd Ok [OpenId loginform] x-text/html-fragment]]
		}
	    }
	}}
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
