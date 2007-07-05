# Query - handle URL/HTTP query strings
if {[info exists argv0] && ([info script] eq $argv0)} {
    lappend auto_path [file dirname [file normalize [info script]]]
}

package require Debug
package provide Query 2.0

Debug off query

namespace eval Query {
    variable utf8 [expr {[catch {package require utf8}] == 0}]

    # Support for x-www-urlencoded character mapping
    # The spec says: "non-alphanumeric characters are replaced by '%HH'"
    proc init_map {} {
	variable map
	variable dmap

	set dmap {+ " " %0D%0A \n %0d%0a \n %% %}
	set map {% %%}

	# set up non-alpha map
	for {set i 1} {$i <= 256} {incr i} {
	    set c [format %c $i]
	    if {![string match {[a-zA-Z0-9]} $c]} {
		lappend map $c %[format %.2X $i]
		lappend dmap %[format %.2X $i] [binary format c $i]
	    }
	}

	# These are handled specially
	lappend map " " + \n %0D%0A
    }

    variable map
    variable dmap
    init_map

    proc 2hex {str} {
	binary scan $str H* hex
	return $hex
    }

    # decode
    #
    #	This decodes data in www-url-encoded format.
    #
    # Arguments:
    #	An encoded value
    #
    # Results:
    #	The decoded value
    
    proc decode {str} {
	Debug.query {decode '$str' [2hex $str]} 10
	variable dmap
	set str [string map $dmap $str]
	Debug.query {decode dmap '$str' [2hex $str]} 10

	return $str
    }

    # encode
    #
    #	This encodes data in www-url-encoded format.
    #
    # Arguments:
    #	A string
    #
    # Results:
    #	The encoded value

    proc encode {string} {
	variable map
	# map non-ascii characters away - note: % must be first
	Debug.query {encode '$string'}
	set string [string map $map $string]
	Debug.query {encode post '$string'}
	return $string
    }

    # qparse -- internal parser support
    #
    #	decodes a query string
    #	
    # Arguments:
    #	qstring	a string containing a query
    #	ct	the content type
    #
    # Results:
    #	A dictionary of names and values from the query
    #	The form of the dict is {name {{value {metadata}}  ... }}
    #
    # Side Effects:

    proc qparse {qstring count {ct "NONE"}} {
	switch -glob -- $ct {
	    text/xml -
	    application/x-www-form-urlencoded -
	    application/x-www-urlencoded -
	    NONE {
		set query [dict create]
		foreach {x} [split [string trim $qstring] &] {
		    # Turns out you might not get an = sign,
		    # especially with <isindex> forms.
		    set z [split $x =]
		    if {[llength $z] == 1} {
			# var present without assignment
			set var [decode [lindex $z 0]]
			set val ""
			set meta [list -unassigned 1 -count [incr count]]
		    } else {
			# var present with assignment
			set var [decode [lindex $z 0]]
			set val [decode [string trim [join [lrange $z 1 end] =] "\"'"]]
			set meta [list -count [incr count]]
		    }
		    
		    dict lappend query $var $val $meta
		}
	    }
	    
	    multipart/* {
		lassign [multipart $ct $qstring $count] query count
	    }
	    
	    default {
		error "Unknown Content-Type: $ct"
	    }
	}
	
	return [list $query $count]
    }

    # cconvert - convert charset to appropriate encoding
    # - try to ensure the correctness of utf-8 input
    proc cconvert {query charset} {
	if {$charset eq ""} {
	    set charset utf-8
	} else {
	    set charset [string tolower $charset]
	}
	Debug.query {cconvert $charset} 6
	if {$charset in [encoding names]} {
	    # tcl knows of this encoding - so make the conversion
	    variable utf8
	    dict for {k v} $query {
		set vals {}
		foreach {val meta} $v {
		    if {$utf8 && $charset eq "utf-8"} {
			# check the content for utf8 correctness
			set point [::utf8::findbad $v]
			if {$point < [string length $v] - 1} {
			    if {$point >= 0} {
				incr point
				lappend meta -bad $point
			    }
			    lappend vals $val $meta
			    continue
			}
		    }
		    
		    set val [encoding convertfrom $charset $val]
		    lappend vals $val $meta
		}
		Debug.query {cconvert $k ($vals)} 10
		dict set query $k $vals
	    }
	}

	return $query
    }

    # charset - handle '_charset_' hack
    # see https://bugzilla.mozilla.org/show_bug.cgi?id=18643
    proc charset {query} {
	if {![exists $query _charset_]} {
	    # no conversion necessary
	    return $query
	}
	set query [cconvert $query [value $query _charset_]]
	dict unset query _charset_
	
	return $query
    }

    # parse -- parse an http dict's queries
    #
    #	decodes the -query and -entity elements of an httpd dict
    #	
    # Arguments:
    #	http	dict containing an HTTP request
    #
    # Results:
    #	A dictionary of names associated with a list of 
    #	values from the request's query part and entity body
    
    proc parse {http} {
	if {[dict exists $http -Query]} {
	    return [dict get $http -Query]
	}

	if {[dict exists $http -query]} {
	    lassign [qparse [dict get $http -query] 0] query count
	    set query [charset $query]
	} elseif {![dict exists $http -entity]} {
	    set query [dict create]
	    set count -1
	    dict set http -entity {}
	} else {
	    set query {}
	    set count 0
	}

	if {[dict exists $http content-type]} {
	    set ct [dict get $http content-type]
	    set entity [Dict get? $http -entity]
	    lassign [qparse $entity $count $ct] query1 count
	    set query1 [charset $query1]
	    Debug.query {qparsed $query1}
	    dict for {n v} $query1 {
		while {$v ne {}} {
		    set v [lassign $v val meta]
		    Debug.query {meta: $n $val $meta - $query}
		    dict lappend query $n $val $meta
		}
	    }
	    Debug.query {meta $query}
	}

	return $query
    }

    # numvalues -- how many values does a named element have?
    #
    # Arguments:
    #	query	query dict generated by parse
    #	el	element name
    #
    # Results:
    #	number of values associated with query
    
    proc numvalues {query el} {
	return [expr {[llength [dict get $query $el]] / 2}]
    }

    # value -- return the nth value associated with a name in a query
    #
    # Arguments:
    #	query	query dict generated by parse
    #	el	name of element
    #	num	index of value
    #
    # Results:
    #	num'th value associated with el by query
    
    proc value {query el {num 0}} {
	return [lindex [dict get $query $el] [expr {$num * 2}]]
    }

    # add - add a simulated query element
    # query - query dict generated by parse
    # el - name of element
    # val - value of element
    # metadata - metadata
    proc add {query el val {metadata {}}} {
	dict lappend query $el $val $metadata
	return $query
    }

    # metadata -- return the nth metadata associated with a name in a query
    #
    # Arguments:
    #	query	query dict generated by parse
    #	el	name of element
    #	num	index of value
    #
    # Results:
    #	num'th metadata associated with el by query
    
    proc metadata {query el {num 0}} {
	return [lindex [dict get $query $el] [expr {$num * 2 + 1}]]
    }

    # exists -- does a value with the given name exist
    #
    # Arguments:
    #	query	query dict generated by parse
    #	el	name of element
    #	num	number of element's values
    #
    # Results:
    #	true if el is in query
    
    proc exists {query el {num 0}} {
	if {$num == 0} {
	    return [dict exists $query $el]
	} else {
	    return [expr {
			  [dict exists $query $el]
			  && ([llength [dict get $query $el]] > ($num*3))
		      }]
	}
    }

    # return a name, value, meta list from the query dict
    proc nvmlist {query} {
	set result {}
	dict for {n v} $query {
	    foreach {val meta} $v {
		lappend result $n $val $meta
	    }
	}
	return $result
    }
    
    # values -- return the list of values associated with a name in a query
    #
    # Arguments:
    #	query	query dict generated by parse
    #	el	name of element
    #
    # Results:
    #	list of values associated with el by query
    
    proc values {query el} {
	set result {}
	foreach {v m} [dict get $query $el] {
	    lappend result $v
	}
	return $result
    }

    # vars -- the list of names in the query
    #
    # Arguments:
    #	query	query dict generated by parse
    #	el	name of element
    #
    # Results:
    #	list of values associated with el by query
    
    proc vars {query} {
	return [dict keys $query]
    }

    # flatten -- flatten query ignoring multiple values and metadata
    #
    #	construct a list able to be flattened into an array
    #
    # Arguments:
    #	query	query dict generated by parse
    #
    # Results:
    #	list of values associated with el by query
    #	multiple values are stored with ,$index as a name suffix
    
    proc flatten {query} {
	set result {}
	dict for {n v} $query {
	    set count 0
	    foreach {val meta} $v {
		if {$count} {
		    lappend result $n,$count $val
		} else {
		    lappend result $n $val
		}
		incr count
	    }
	}
	return $result
    }

    # for compatibility with ncgi.
    proc nvlist {query} {
	return [flatten $query]
    }

    # parseMimeValue
    #
    #	Parse a MIME header value, which has the form
    #	value; param=value; param2="value2"; param3='value3'
    #
    # Arguments:
    #	value	The mime header value.  This does not include the mime
    #		header field name, but everything after it.
    #
    # Results:
    #	A two-element list, the first is the primary value,
    #	the second is in turn a name-value list corresponding to the
    #	parameters.  Given the above example, the return value is
    #	{
    #		value
    #		{param value param2 value param3 value3}
    #	}
    
    proc parseMimeValue {value} {
	set parts [split $value \;]
	set results [list [string trim [lindex $parts 0]]]
	
	set paramList {}
	foreach sub [lrange $parts 1 end] {
	    if {[regexp -- {([^=]+)=(.+)} $sub match key val]} {
		set key [string trim [string tolower $key]]
		set val [string trim $val]
		
		# Allow single as well as double quotes
		if {[regexp -- {^[\"']} $val quote]} {
		    # need a quote for balance
		    if {[regexp -- ^${quote}(\[^$quote\]*)$quote $val x val2]} {
			# Trim quotes and any extra crap after close quote
			set val $val2
		    }
		}
		lappend paramList $key $val
	    }
	}
	
	if {[llength $paramList]} {
	    lappend results $paramList
	}
	
	return $results
    }

    # multipart
    #
    #	This parses multipart form data.
    #	Based on work by Steve Ball for TclHttpd
    #
    # Arguments:
    #	type	The Content-Type, because we need boundary options
    #	query	The raw multipart query data
    #
    # Results:
    #	An alternating list of names and values
    #	In this case, the value is a two element list:
    #		content, which is the main value of the element
    #		headers, which in turn is a list names and values
    #	The header name/value pairs come primarily from the MIME headers
    #	like Content-Type that appear in each part.  However, the
    #	Content-Disposition header is handled specially.  It has several
    #	parameters like "name" and "filename" that are important, so they
    #	are promoted to to the same level as Content-Type.  Otherwise,
    #	if a header like Content-Type has parameters, they appear as a list
    #	after the primary value of the header.  For example, if the
    #	part has these two headers:
    #
    #	Content-Disposition: form-data; name="Foo"; filename="/a/b/C.txt"
    #	Content-Type: text/html; charset="iso-8859-1"; mumble='extra'
    #	
    #	Then the header list will have this structure:
    #	{
    #		content-disposition form-data
    #		name Foo
    #		filename /a/b/C.txt
    #		content-type {text/html {charset iso-8859-1 mumble extra}}
    #	}
    #	Note that the header names are mapped to all lowercase.  You can
    #	use "array set" on the header list to easily find things like the
    #	filename or content-type.  You should always use [lindex $value 0]
    #	to account for values that have parameters, like the content-type
    #	example above.  Finally, not that if the value has a second element,
    #	which are the parameters, you can "array set" that as well.
    
    proc multipart {type query {count -1}} {
	set parsedType [parseMimeValue $type]
	if {![string match multipart/* [lindex $parsedType 0]]} {
	    error "Not a multipart Content-Type: [lindex $parsedType 0]"
	}

	array set options [lindex $parsedType 1]
	if {![info exists options(boundary)]} {
	    error "No boundary given for multipart document"
	}
	set boundary $options(boundary)
	
	# The query data is typically read in binary mode, which preserves
	# the \r\n sequence from a Windows-based browser.
	# Also, binary data may contain \r\n sequences.
	
	if {[string match "*$boundary\r\n*" $query]} {
	    set lineDelim "\r\n"
	    # puts "DELIM"
	} else {
	    set lineDelim "\n"
	    # puts "NO"
	}
	
	# Iterate over the boundary string and chop into parts
	
	set len [string length $query]
	# [string length $lineDelim]+2 is for "$lineDelim--"
	set blen [expr {[string length $lineDelim] + 2 + \
			    [string length $boundary]}]
	set first 1
	set results [dict create]
	set offset 0
	
	# Ensuring the query data starts
	# with a newline makes the string first test simpler
	if {[string first $lineDelim $query 0] != 0} {
	    set query $lineDelim$query
	}
	
	while {[set offset [string first "$lineDelim--$boundary" $query $offset]] >= 0} {
	    # offset is the position of the next boundary string
	    # in $query after $offset
	    
	    if {$first} {
		set first 0	;# this was the opening delimiter
	    } else {
		# this was the delimiter bounding current element
		# generate a n,v element from parsed content
		dict lappend results \
		    $formName \
		    [string range $query $off2 [expr {$offset -1}]] \
		    $headers
	    }
	    incr offset $blen	;# skip boundary in stream
	    
	    # Check for the terminating entity boundary,
	    # which is signaled by --$boundary--
	    if {[string range $query $offset [expr {$offset + 1}]] eq "--"} {
		# end of parse
		break
	    }
	    
	    # We have a new element. Split headers out from content.
	    # The headers become a nested dict structure in result:
	    # {header-name { value { paramname paramvalue ... } } }
	    
	    # find off2, the offset of the delimiter which terminates
	    # the current element
	    set off2 [string first "$lineDelim$lineDelim" $query $offset]
	    
	    # generate a dict called headers with element's headers and values
	    set headers [dict create -count [incr count]]
	    set formName ""	;# any header 'name' becomes the element name
	    foreach line [split [string range $query $offset $off2] $lineDelim] {
		if {[regexp -- {([^:\t ]+):(.*)$} $line x hdrname value]} {
		    set hdrname [string tolower $hdrname]
		    set valueList [parseMimeValue $value]
		    if {$hdrname eq "content-disposition"} {
			
			# Promote Content-Disposition parameters up to headers,
			# and look for the "name" that identifies the form element
		    
			dict lappend headers $hdrname [lindex $valueList 0]
			foreach {n v} [lindex $valueList 1] {
			    lappend headers $n $v
			    if {$n eq "name"} {
				set formName $v	;# the name of the element
			    }
			}
		    } else {
			dict lappend headers $hdrname $valueList
		    }
		}
	    }
	    
	    if {$off2 > 0} {
		# +[string length "$lineDelim$lineDelim"] for the
		# $lineDelim$lineDelim
		incr off2 [string length "$lineDelim$lineDelim"]
		set offset $off2
	    } else {
		break
	    }
	}
	
	set q [dict create]
	dict for {n v} $results {
	    dict lappend q $n $v
	}
	return [list $q $count]
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

if {[info exists argv0] && ([info script] eq $argv0)} {
    foreach test {
	"error=User%20Doesn?error=User%20Doesn't%20Exist"
	"error=Passwords%20don't%20match"
	"error=first&error=second&error=third"
    } {
	lassign [Query qparse $test 0] query count
	puts stderr "'$test' -> ($query)"
	puts stderr "find error '[Query value $query error]'"
	
	set query [Query parse [dict create -query $test]]
	puts stderr "'$test' -> ($query)"
	puts stderr "find error '[Query value $query error]'"
	puts stderr "flattened: [Query flatten $query]"
    }

    # here's something I caught in the wild
    set q {N {8942 {}} cancel {Cancel {-count 4}} C {{This is a Work-in-progress translation (to Swedish) of the eleven syntactic rules of Tcl. (see [Endekalogue] for other translations). [Category Documentation] |} {-count 1 -bad 163}} O {{1182004521 lars_h@81.231.37.27} {-count 2}}}
    set metadata [Query metadata $q C]
    puts stderr "metadata: $metadata"
}
