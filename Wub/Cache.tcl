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
	set result [expr {$since >= [dict get $cached -modified]}]
	Debug.cache {unmodified? $since >= [dict get $cached -modified] -> $result}
	if {$result} {
	    counter $cached -ifmod
	}
	return $result
    }

    variable keys	;# keys into cache
    array set keys {}
    variable cache	;# array of refcounted dicts
    array set cache {}
    variable unique 0
    variable maxsize 0	;# maximum size of object to cache

    proc exists? {key} {
	if {$key eq ""} {return 0}	;# special case - no key

	variable keys
	set key [string trim $key \"]	;# remove ridiculous quotes
	Debug.cache {exists: $key - [info exists keys($key)]}
	return [info exists keys($key)]
    }

    proc invalidate {key} {
	if {$key eq ""} return	;# special case - no key

	Debug.cache {invalidate: $key}
	variable keys
	variable cache

	set key [string trim $key \"]	;# remove ridiculous quotes
	if {[exists? $key]} {
	    Debug.cache {invalidating $key in '[array names keys]'} 4
	    set ckey $keys($key)
	    if {[info exists cache($ckey)]} {
		dict incr cache($ckey) -refcount -1
		if {[dict get $cache($ckey) -refcount] <= 0} {
		    unset cache($ckey)
		}
	    }
	    unset keys($key)
	    Debug.cache {invalidated '$key'.}
	    Debug.cache {post-invalidate: [array names keys]} 8
	} else {
	    Debug.cache {invalidate - no such element '$key'.}
	}
    }

    proc delete {key} {
	Debug.cache {delete $key} 4
	if {[exists? $key]} {
	    variable keys
	    set key [string trim $key \"]	;# remove ridiculous quotes
	    set ckey $keys($key) ;# key under which the cached value is stored
	    variable cache
	    if {[info exists cache($ckey)]} {
		Debug.cache {found cache: etag:[Dict get? $cache($ckey) etag] url:[Dict get? $cache($ckey) -url]}
		set cached $cache($ckey)
		invalidate [Dict get? $cached etag]
		invalidate [Dict get? $cached -url]
	    }
	    invalidate $key	;# remove offered key
	}
    }

    # clear the whole cache
    proc clear {} {
	variable keys
	foreach key [array get keys http:*] {
	    delete $key
	}
    }

    # cache effectiveness stats
    variable hits 0
    variable attempts 0

    # fetch - try to find an entry matching req
    proc fetch {req} {
	Debug.cache {fetch: ([dumpMsg $req])}

	variable keys
	variable cache
	if {[exists? [Dict get? $req etag]]} {
	    set key $keys([string trim [dict get $req etag] \"])
	} elseif {[exists? [dict get $req -url]]} {
	    set key $keys([dict get $req -url])
	} else {
	    error "Cache Fetching $req, no match."
	}

	# maintain some stats for cache management
	variable hits; incr hits	;# count cache hits

	return $cache($key)
    }

    # high and low water mark for cache occupancy
    variable high 100
    variable low 90
    variable weight_age 0.02
    variable weight_hits -2.0

    proc staleness {n} {
	variable cache;
	variable weight_age; variable weight_hits

	set c $cache($n);
	set hits [expr {[dict get $c -hits] + [dict get $c -unmod]}]
	set age [expr {[dict get $c -when] - [clock seconds]}]
	set weight [expr {($hits * $weight_hits) + ($age * $weight_age)}]
	return $weight
    }

    # stale_sort - return objects in staleness order
    # staleness is a measure of #hits and age of entry
    proc stale_sort {a b} {
	variable cache;
	variable weight_age; variable weight_hits

	set weight_a [staleness $a]
	set weight_b [staleness $b]

	return [expr {int(100 * ($weight_b - $weight_a))}]
    }

    # put - insert request into cache
    proc put {req} {
	Debug.cache {put: ([dumpMsg $req])}

	# whatever the eventual cache status, must remove old matches
	invalidate [dict get $req -url]	;# invalidate by -url
	invalidate [Dict get? $req etag] ;# invalidate by etag

	# we only cache 200s
	if {[dict get $req -code] != 200} {
	    return $req
	}

	# allow application to avoid caching by setting -dynamic
	if {[dict exists $req -dynamic]
	    && [dict get $req -dynamic]
	} {
	    return $req
	}

	variable maxsize
	if {($maxsize > 0)
	    && ($maxsize < [string length [dict get $req -content]])} {
	    # we can't store enormous entities in the cache
	    return $req
	}

	set ctype [dict get $req content-type]
	if {[string match x-*/* $ctype]
	    || [string match */x-* $ctype]} {
	    return $req
	}

	if {[dict exists $req etag]} {
	    set etag [string trim [dict get $req etag] \"]
	    dict unset req etag	;# we don't want to store old etag - why?
	} else {
	    # generate an etag
	    variable unique
	    set etag "[pid].[clock microseconds].[incr unique]"
	    while {[exists? $etag]} {
		set etag "[pid].[clock microseconds].[incr unique]"
	    }
	}

	# subset the cacheable request with just those fields needed
	set cached [Dict subset $req {
	    -content -gzip -code -url -charset -chconverted -modified
	    -expiry
	    content-language content-location content-md5 content-type
	    expires last-modified cache-control}]
	set cached [dict merge $cached [Dict subset $req $::Http::rs_headers]]

	# add new fields for server cache control
	dict set cached -refcount 2
	dict set cached -when [clock seconds]
	dict set cached etag \"$etag\"	;# store with ridiculous quotes
	dict set cached -key $etag	;# remember the actual etag
	dict set cached -hits 0
	dict set cached -unmod 0
	dict set cached -ifmod 0

	Debug.cache {cache entry: [set x $cached; dict set x -gzip <ELIDED>; dict set x -content <ELIDED>; return $x]} 4

	variable cache; variable high; variable low
	# ensure cache size is bounded
	set cachesize [array size cache]
	if {$cachesize > $high} {
	    set ordered [lsort -command ::Cache::stale_sort [array names cache]]
	    while {$cachesize > $low} {
		# pick a cache entry to remove by weight
		set c [lindex $ordered 0]
		set ordered [lrange $ordered 1 end]

		# remove the selected entry
		catch { # invalidate by -url
		    invalidate [dict get $cache($c) -url]
		}
		catch { # invalidate by etag
		    invalidate [Dict get? $cache($c) etag]
		}

		incr cachesize -1
	    }
	}

	# insert cacheable request into cache under modified etag
	set cache($etag) $cached

	# insert keys into key array - match by -url or etag
	variable keys
	set keys($etag) $etag
	set keys([dict get $req -url]) $etag

	Debug.cache {new: $etag == [dict get $req -url]}

	return $req
    }

    # keys - return keys matching filter (default all)
    proc keys {{filter {}}} {
	variable keys
	return [array names keys {*}$filter]
    }

    # consistency - check or ensure cache consistency
    proc consistency {{fix 1}} {
	variable keys
	variable cache
	set check 1
	while {$check} {
	    set check 0
	    foreach {name val} [array get keys] {
		if {$name eq $val} {
		    # etag key
		    if {![info exists cache($name)]} {
			Debug.error {etag key no matching cache $name}
			if {$fix} {
			    unset keys($name)
			    incr check
			}
		    }
		} else {
		    # url key
		    if {![info exists cache($val)]} {
			Debug.error {url key $name no matching cache $val}
			if {$fix} {
			    catch {unset keys($val)}
			    incr check
			}
		    }
		}
	    }

	    foreach {name val} [array get cache] {
		if {[catch {
		    if {![exists? keys($name)]} {
			# no etag key for cache
			error {orphan cache by name '$name' / $cache($name)}
		    }
		    if {![exists? [dict get $val -url]]} {
			error {orphan cache by url '[Dict get? $val -url]' / $name - '$cache($name)'}
		    }
		    if {![exists? [dict get $val etag]]} {
			error {orphan cache by etag '[Dict get? $val etag]' / $name - '$cache($name)'}
		    }
		    if {[string trim [dict get $val etag] \"] ne $name} {
			error {etag and cache name mismatch}
		    }
		} r eo]} {
		    Debug.error {cache consistency: $eo}
		    if {$fix} {
			unset cache($name)
			incr check
		    }
		}
	    }
	}
    }

    variable CC 0	;# do we bother to parse cache-control?
    variable obey_CC 0	;# do we act on cache-control? (Not Implemented)

    # 2dict - convert cache to dict
    proc 2dict {} {
	variable cache
	set result {}
	foreach {n v} [array get cache] {
	    if {[dict exists $v -content]} {
		dict set v -size [string length [dict get $v -content]]
	    } else {
		dict set v -size 0
	    }
	    catch {dict unset v -content}
	    catch {dict unset v -gzip}
	    dict set v -stale [staleness $n]
	    dict set result $n $v
	}
	return $result
    }

    proc counter {cached field} {
	variable cache
	dict incr cache([dict get $cached -key]) $field
    }

    proc any-match {req cached} {
	if {![dict exists $req if-none-match]} {
	    return 0
	}

	set result [expr {[dict get $cached etag] in [split [dict get $req if-none-match] ", "]}]
	Debug.cache {any-match: $result - [dict get $cached etag] - [dict get $req if-none-match]}
	return $result
    }

    # check - can request be satisfied from cache?
    # if so, return it.
    proc check {req} {
	Debug.cache {check [dict get $req -url]: ([dumpMsg $req])}
	variable attempts; incr attempts	;# count cache attempts

	# first query cache to see if there's even a matching entry
	set etag [Dict get? $req etag]
	set url [Dict get? $req -url]
	if {$etag ne "" && ![exists? $etag]} {
	    Debug.cache {etag '$etag' given, but not in cache}
	    return {}	;# we don't have a copy matching etag
	}
	if {$url ne "" && ![exists? $url]} {
	    Debug.cache {url '$url' not in cache}
	    return {}	;# we don't have a copy matching -url either
	}
	dict set req etag $etag

	# old style no-cache request
	variable obey_CC
	variable CC
	if {$CC
	    && "no-cache" in [split [Dict get? $req pragma] ,]
	} {
	    # ignore no-cache, because we're the server, and in the best
	    # position to judge the freshness of our content.
	    Debug.cache {no-cache requested - we're ignoring those!}
	    if {$obey_CC} {return {}}
	}

	# split any cache control into an array
	if {$CC
	    && [dict exists $req -cache-control]
	} {
	    foreach directive [split [dict get $req -cache-control] ,] {
		set body [string trim [join [lassign [split $directive =] d] =]]
		set d [string trim $d]
		set cc($d) $body
	    }
	    Debug.cache {no-cache requested [array get cc]}

	    if {[info exists cc(no-cache)]
		|| ([info exists cc(max-age)] && ($cc(max-age)==0))} {
		if {$obey_CC} {return {}}	;# no cache.
	    }

	    if {$obey_CC && [info exists cc(max-age)]} {
		# we ignore max_age
		#set max_age [Http DateInSeconds $cc(max-age)]
	    }
	}

	# we may respond from cache, we *do* have a cached copy
	if {[catch {
	    fetch $req
	} cached eo]} {
	    # it's gotta be there!
	    Debug.error {cache inconsistency - can't fetch existing entry for url:'$url' etag:'$etag' because '$cached' ($eo)}
	    return {}
	}

	if {[info exists max_age]
	    && ([dict get $cached -when] - [clock seconds]) > $max_age
	} {
	    # ignore the cache - this client wants newness
	    Debug.cache {older than max-age $max_age}
	    return {}
	}

	# re-state the expiry of this cache entry
	if {[dict exists $cached expires]} {
	    dict set req expires [dict get $cached expires]
	}

	# see if we can respond 304
	if {[any-match $req $cached]} {
	    # rfc2616 14.26 If-None-Match
	    # If any of the entity tags match the entity tag of the entity
	    # that would have been returned in the response to a similar 
	    # GET request (without the If-None-Match header) on that 
	    # resource, or if "*" is given and any current entity exists 
	    # for that resource, then the server MUST NOT perform the 
	    # requested method, unless required to do so because the 
	    # resource's modification date fails to match that
	    # supplied in an If-Modified-Since header field in the request.
	    if {[dict get $req -method] in {"GET" "HEAD"}} {
		# if the request method was GET or HEAD, the server 
		# SHOULD respond with a 304 (Not Modified) response, including
		# the cache-related header fields (particularly ETag) of one 
		# of the entities that matched.
		Debug.cache {unmodified $url}
		counter $cached -unmod	;# count unmod hits
		return [Http NotModified $req]
		# NB: the expires field is set in $req
	    } else {
		# For all other request methods, the server MUST respond with
		# a status of 412 (Precondition Failed).
		return [Http PreconditionFailed $req]
	    }
	}

	if {[unmodified? $req $cached]} {
	    Debug.cache {unmodified $url}
	    counter $cached -unmod	;# count unmod hits
	    return [Http NotModified $req]
	    # NB: the expires field is set in $req
	} else {
	    # deliver cached content in lieue of processing
	    #dict set req last-modified [dict get $cached last-modified]
	    counter $cached -hits	;# count individual entry hits
	    set req [dict merge $req $cached]
	    set req [Http CacheableContent $req [dict get $cached -modified]]
	    Debug.cache {cached content for $url ([set xx $req; dict set xx -entity <ELIDED>; dict set xx -content <ELIDED>; dict set xx -gzip <ELIDED>; return $xx])}
	    return $req
	}

	Debug.cache {no cached version}
	return {}	;# no cache available
    }

    # initialise the state of Cache
    proc init {args} {
	variable {*}$args
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

Debug off cache 10	;# debug cache access decisions
