# cache for multithreading
package require Debug
package provide Cache 2.0
namespace eval Cache {

    proc unmodified? {req cached} {
	# perform cache freshness check
	if {![dict exists $req if-modified-since]} {
	    Debug.cache {unmodified? 0 - no if-modified-since}
	    return 0
	}

	# cache check freshness against request's modification time
	set since [Http DateInSeconds [dict get $req if-modified-since]]
	set result [expr {$since < [dict get $cached -when]}]
	Debug.cache {unmodified? $since < [dict get $cached -when] -> $result}
	return $result
    }

    variable keys	;# keys into cache
    array set keys {}
    variable cache	;# array of refcounted dicts
    array set cache {}
    variable unique 0

    proc exists? {key} {
	variable keys
	set key [string trim $key \"]
	Debug.cache {exists: $key - [info exists keys($key)]}
	return [info exists keys($key)]
    }

    proc invalidate {key} {
	Debug.cache {invalidate: $key}
	variable keys
	variable cache
	Debug.cache {pre-invalidate: [array names keys]} 4

	set key [string trim $key \"]
	if {[exists? $key]} {
	    set ckey $keys($key)
	    if {[info exists cache($ckey)]} {
		dict incr cache($ckey) -refcount -1
		if {[dict get $cache($ckey) -refcount] <= 0} {
		    unset cache($ckey)
		}
	    }
	    unset keys($key)
	    Debug.cache {invalidated '$key'.}
	} else {
	    Debug.cache {invalidate - no such element '$key'.}
	}
	Debug.cache {post-invalidate: [array names keys]} 4
    }

    proc delete {key} {
	variable keys
	Debug.cache {delete $key} 4
	if {[exists? $key]} {
	    set ckey $keys($key)
	    if {[info exists cache($ckey)]} {
		Debug.cache {found cache: etag:[dict get? $cache($ckey) etag] url:[dict get? $cache($ckey) -url]}
		set etag [dict get? $cache($ckey) etag]
		if {$etag ne ""} {
		    invalidate $etag
		}

		set url [dict get? $cache($ckey) -url]
		if {$url ne ""} {
		    invalidate $url
		}
	    }
	    invalidate $key
	}
    }

    # clear the whole cache
    proc clear {} {
	variable keys
	foreach key [array get keys http:*] {
	    delete $key
	}
    }

    proc put {req} {
	Debug.cache {put: ([dumpMsg $req])}
	if {[exists? [dict get $req -url]]} {
	    Debug.cache {putting url: [dict get $req -url]}
	    # invalidate anything which matches this URL
	    set cached [fetch $req]
	    invalidate [dict get $cached etag]
	    invalidate [dict get $cached -url]
	}

	if {[dict exists $req etag]} {
	    # invalidate anything which matches this etag
	    set etag [dict get $req etag]
	    if {[exists? $etag]} {
		invalidate $etag
	    }
	} else {
	    # generate an etag
	    variable unique
	    set etag "[pid].[clock microseconds].[incr unique]"
	    while {[exists? $etag]} {
		set etag "[pid].[clock microseconds].[incr unique]"
	    }
	    dict set req etag \"[string trim $etag \"]\"
	}

	# allow application to avoid caching by setting -dynamic
	if {[dict exists $req -dynamic]
	    && [dict get $req -dynamic]
	} {
	    return $req
	}

	set cached $req
	foreach f {set-cookie -cookies -listener
	    -sock -method -transaction -ipaddr -worker
	} {
	    catch {dict unset cached $f}	;# do not cache field
	}
	dict set cached -refcount 2
	dict set cached -when [clock seconds]

	set etag [string trim $etag \"]

	# now insert req into cache
	variable cache
	set cache($etag) $cached

	# insert keys into key array
	variable keys
	set keys($etag) $etag
	set keys([dict get $req -url]) $etag
	Debug.cache {new: $etag == [dict get $req -url]}

	return $req
    }

    proc keys {} {
	variable keys
	return [array names keys]
    }

    proc consistency {{fix 1}} {
	variable keys
	variable cache
	foreach {name val} [array get keys] {
	    if {$name eq $val} {
		# etag key
		if {![info exists cache($name)]} {
		    Debug.error {etag key no matching cache $name}
		    if {$fix} {
			unset keys($name)
		    }
		}
	    } else {
		# url key
		if {![info exists cache($val)]} {
		    Debug.error {url key $name no matching cache $val}
		    if {$fix} {
			unset keys($val)
		    }
		}
	    }
	}

	foreach {name val} [array get cache] {
	    if {![info exists keys($name)]} {
		# no etag key for cache
		Debug.error {orphan cache $name / $cache($name)}
	    } else {
	    }
	}
    }

    proc fetch {req} {
	Debug.cache {fetch: ([dumpMsg $req])}
	variable keys
	variable cache
	if {[dict exists $req etag]
	    && [info exists keys([dict get $req etag])]
	} {
	    return $cache($keys([dict get $req etag]))
	} elseif {[info exists keys([dict get $req -url])]} {
	    return $cache($keys([dict get $req -url]))
	} else {
	    error "Cache Fetching $req, no match."
	}
    }

    proc check {req} {
	Debug.cache {check [dict get $req -url]: ([dumpMsg $req])}
	# first query cache to see if there's even a matching entry
	if {!([dict exists $req etag] && [exists? $etag])
	    && ![exists? [dict get $req -url]]} {
	    Debug.cache {no: neither etag and url are in cache}
	    return {}	;# we don't have a copy
	}

	# old style no-cache request
	if {[dict exists $req pragma]
	    && ("no-cache" in [split [dict get $req pragma] ,])
	} {
	    # ignore no-cache, because we're the server, and in the best
	    # position to judge the freshness of our content.
	    Debug.cache {no-cache requested - we're ignoring those!}
	    # return {}
	}

	# split any cache control into an array
	if {[dict exists $req -cache-control]} {
	    set cacheable [split [dict get $req -cache-control] ,]
	    foreach directive $cacheable {
		set body [string trim [join [lassign [split $directive =] d] =]]
		set d [string trim $d]
		set cc($d) $body
	    }
	    if {[info exists cc(no-cache)]
		|| ([info exists cc(max-age)] && ($cc(max-age)==0))} {
		Debug.cache {no-cache requested [array get cc] - we're ignoring these}
		#return {}	;# no cache.
	    }
	    if {[info exists cc(max-age)]} {
		# we ignore max_age
		#set max_age [Http DateInSeconds $cc(max-age)]
	    }
	}

	# we may respond from cache, we *do* have a cached copy
	set cached [fetch $req]
	if {[info exists max_age]
	    && (([dict $cached -when] - [clock seconds]) > $max_age)
	} {
	    # invalidate the cache - this client wants newness
	    Debug.cache {older than max-age $max_age}
	    return {}
	}

	# re-state the expiry of this cache entry
	if {[dict exists $cached expires]} {
	    dict set req expires [dict get $cached expires]
	}

	# see if we can respond 304
	if {[dict exists $req if-none-match]} {
	    if {([dict get $cached etag] in [list {*}[split [dict get $req if-none-match] ","]])} {
		# rfc2616 14.26 If-None-Match
		# If any of the entity tags match the entity tag of the entity that
		# would have been returned in the response to a similar GET request
		# (without the If-None-Match header) on that resource, or if "*" is
		# given and any current entity exists for that resource, then the
		# server MUST NOT perform the requested method, unless required to do
		# so because the resource's modification date fails to match that
		# supplied in an If-Modified-Since header field in the request.
		# Instead, if the request method was GET or HEAD, the server SHOULD
		# respond with a 304 (Not Modified) response, including the cache-
		# related header fields (particularly ETag) of one of the entities that
		# matched. For all other request methods, the server MUST respond with
		# a status of 412 (Precondition Failed).
		if {[dict get $req -method] ni {"GET" "HEAD"}} {
		    return [Http PreconditionFailed $req]
		}
	    }
	}

	if {[unmodified? $req $cached]} {
	    Debug.cache {unmodified}
	    return [Http NotModified $req]
	} else {
	    # deliver cached content in lieue of processing
	    Debug.cache {cached content}
	    if {![dict exists $req last-modified]} {
		dict set req last-modified [Http Date [dict get $cached -when]]
	    }

	    set req [dict merge $req $cached]
	    set req [Http CacheableContent $req [dict get $cached -when]]
	    if {[dict get $cached -code] eq 404} {
		dict set req -code 404
	    }
	    return $req
	}

	Debug.cache {no cached version}
	return {}	;# no cache available
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
