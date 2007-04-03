# Form.html
# this is an experiment in constructing forms.
#
# TODO: nested fieldsets

if {[info exists argv0] && ([info script] eq $argv0)} {
    lappend auto_path [file dirname [file normalize [info script]]]
}

package require Debug
Debug off form 1

package provide Form 1.0

namespace eval Form {
    variable inherit {
	-type -maxlength -size
	-disabled -readonly -template -class
	-onfocus -onblur -onselect -onchange
    }

    variable field_fields {
	-type -maxlength -name -size -value -note
	-legend -label -prolog -epilog -inline -tooltip -alt
	-checked -disabled -readonly -tabindex
	-src -onfocus -onblur -onselect -onchange
	-class -rows -cols
	-option
    }

    variable defaults {
	-method post
	-maxlength 64
	-size 30
	-inline 0
	-type text
    }

    proc parse_field {name f enclosing} {
	set field [dict create -label $name -name $name]

	# grab defaults from surrounding field
	variable inherit
	dict for {k v} $enclosing {
	    if {$k in $inherit} {
		dict set field $k $v
	    }
	}

	set fields {}
	foreach {key val} $f {
	    if {[string match -* $key]} {
		dict set field $key $val
	    } else {
		if {[lsearch $fields $key] < 0} {
		    lappend fields $key
		}
		dict lappend field $key [parse_field $key $val $field]
	    }
	}
	if {[llength $fields] > 0} {
	    dict set field -fields $fields
	}
	return $field
    }

    proc parse {text args} {
	puts stderr "PARSE: [info frame -2]"
	Debug.form {Parse: '$text'} 2
	variable defaults
	set form [dict merge $defaults $args]
	set fields {}
	foreach {key val} $text {
	    Debug.form {parsing: $key} 2
	    if {[string match -* $key]} {
		dict lappend form $key $val
	    } else {
		if {[lsearch $fields $key] < 0} {
		    lappend fields $key
		}
		dict set form $key [list [parse_field $key $val $form]]
	    }
	}
	if {[llength $fields] > 0} {
	    dict set form -fields $fields
	}
	Debug.form {parse: $form} 2
	return $form
    }

    proc label {text} {
	set result {}
	foreach word [split $text] {
	    if {$word eq ""} continue
	    if {[string length $word] > 3} {
		lappend result [string totitle $word]
	    } else {
		lappend result $word
	    }
	}
	return [join $result]
    }

    proc inline {f body} {
	set html ""
	Debug.form {inline: $f}

	if {![dict exists $f -inline] || ![dict get $f -inline]} {
	    Debug.form {<P>: $f}
	    append html <p>
	    set p 1
	} else {
	    Debug.form {inlining: $f}
	    set p 0
	}

	uplevel [list append html $html]

	uplevel $body

	set html ""

	if {[dict exists $f -text]} {
	    append html <p> [dict get $f -text] </p> \n
	}

	if {$p} {
	    append html </p> \n
	}

	uplevel [list append html $html]
    }

    proc labelit {f} {
	if {[dict exists $f -label]} {
	    append html <label>
	    if {[dict exists $f -tooltip]} {
		append html "<span class='label'>[label [dict get $f -label]]: "
		append html "<span class='tooltip' style='visibility:hidden'>[dict get $f -tooltip]</span>"
		append html "</span>"
	    } else {
		append html "[label [dict get $f -label]]: "
	    }
	    append html </label>
	}
	return $html
    }

    proc attrs {f what which} {
	set attrl {}
	lappend which class
	foreach x $which {
	    if {[dict exists $f -$x]} {
		if {[dict get $f -$x] eq ""} {
		    lappend attrl $x
		} else {
		    lappend attrl "$x='[dict get $f -$x]'"
		}
	    }
	}

	return "<$what [join $attrl]"
    }

    proc htmlfields {name fields args} {
	Debug.form {htmlfields name:$name fields:'$fields' args:'$args'}
	set html ""
	if {![dict exists $fields -fields]} {
	    return ""
	}

	foreach fn [dict get $fields -fields] {
	    foreach f [dict get $fields $fn] {
		Debug.form {fields for $name: $fn ($f)}
		    
		set fname [expr {[dict exists $f -name]?[dict get $f -name]:$fn}]
		if {[dict exists $args $fname]} {
		    dict set f -value [armour [dict get $args $fname]]
		}
		    
		set type [expr {[dict exists $f -type]?[dict get $f -type]:""}]
		switch -glob -- [dict exists $f -fields],$type {
		    *,select {
			inline $f {
			    append html [labelit $f]
			    append html [attrs $f select {
				name size multiple disabled 
				tabindex onfocus onblur onchange
			    }]> \n
			    
			    append html [htmlfields $fname $f {*}$args]
			    
			    append html </select> \n
			}
		    }
		    
		    1,* {
			append html [fieldset $fn $f {*}$args]
		    }
		    
		    0,option {
			append html [attrs $f option {
			    selected disabled label value
			}]>
			
			if {[dict exists $f -option]} {
			    append html [dict get $f -option]
			} elseif {[dict exists $f -label]} {
			    append html [dict get $f -label]
			}
			catch {dict unset f -label}
			
			append html </option> \n
		    }
		    
		    0,textarea {
			inline $f {
			    append html [labelit $f]
			    append html [attrs $f textarea {
				type name readonly rows cols
			    }]>
			    if {[dict exists $f -value]} {
				append html [dict get $f -value]
			    }
			    append html </textarea> \n
			}
		    }
		    
		    0,radio -
		    0,checkbox {
			inline $f {
			    append html [labelit $f]
			    append html [attrs $f input {
				type checked name alt readonly value
			    }]/>
			}
		    }
		    
		    0,hidden {
			append html [attrs $f input {
			    type name readonly value
			}]/>
			catch {dict unset f -label}
		    }
		    
		    default {
			inline $f {
			    append html [labelit $f]
			    append html [attrs $f input {
				type maxlength name size alt readonly value}]/>
			}
		    }
		}
	    }
	}

	return $html
    }

    proc fieldset {name fieldset args} {
	Debug.form {fieldset name:$name fields:$fieldset}
	set html ""

	if {[dict exists $fieldset -prolog]} {
	    append html <p> [dict get $fieldset -prolog] </p> \n
	}

	if {![dict exists $fieldset -inline] || ![dict get $fieldset -inline]} {
	    Debug.form {<P>: $name}
	    append html <p>
	    set p 1
	} else {
	    Debug.form {inlining: $name}
	    set p 0
	}
	    
	append html <fieldset> \n

	if {![dict exists $fieldset -legend]} {
	    dict set fieldset -legend $name
	}

	append html <legend> [label [dict get $fieldset -legend]] </legend> \n

	if {[dict exists $fieldset -text]} {
	    append html <p> [dict get $fieldset -text] </p> \n
	}

	append html [htmlfields $name $fieldset {*}$args] \n

	if {[dict exists $fieldset -note]} {
	    append html <p> [dict get $fieldset -note] </p> \n
	}

	append html </fieldset> \n
	
	if {$p} {
	    append html </p> \n
	}

	if {[dict exists $fieldset -epilog]} {
	    append html <p> [dict get $fieldset -epilog] </p> \n
	}

	return $html
    }

    proc html {form args} {
	set form [parse $form {*}$args]

	if {[dict exists $form -record]} {
	    set record [dict get $form -record]
	    Debug.form {form html record: $record}
	} else {
	    set record {}
	}

	if {[dict exists $form -class]} {
	    set class [dict get $form -class]
	} else {
	    set class Form
	}

	foreach p [dict get? $form -proc] {
	    lassign $p name args body
	    Debug.form {form defining proc: $name}
	    if {$name ne "" && [info procs $name] eq {}} {
		proc ::$name $args $body
	    }

	    if {[dict get? $form -fields] eq ""} {
		# if no fields are defined, then the -proc *is* the form
		set html [dict get? $form -prolog]
		append html [$::name]
		foreach include [dict get? $form -load] {
		    set loaded [load [file join [dict get? $form -path] $include]]
		    dict set loaded -record [dict get? $form -record]
		    append html [html $loaded {*}$args] \n
		}
		append html [dict get? $form -epilog]
		return $html
	    } else {
		if {[dict exists $form -domain]} {
		    set domain [dict get $form -domain]
		} else {
		    set domain /[string trim [string tolower [namespace qualifiers $name]] :]/
		}
		
		dict set form -action [list "${domain}[string trimleft [namespace tail $name] /]"]
	    }
	}

	if {![dict exists $form -method]} {
	    dict set form -method GET
	}

	return [subst {
	    [if {[dict exists $form -action]} {
		return "<form class='$class' action='[lindex [dict get $form -action] 0]' method='[lindex [dict get $form -method] 0]'>\n"
	    }]

	    [join [dict get? $form -prolog]]
	    [htmlfields form $form {*}$record]
	    [join [dict get? $form -epilog]]

	    [if {[dict exists $form -submit]} {
		return "<input type='submit' value='[lindex [dict get $form -submit] 0]'>"
	    }]

	    [if {[dict exists $form -action]} {
		return </form>
	    }]
	}]
    }

    proc load {path args} {
	set fd [open $path]
	set content [string map $args [read $fd]]
	close $fd
	lappend content -path $path
	return [html $content]
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}

if {[info exists argv0] && ($argv0 eq [info script])} {
    Debug on form 10

    puts "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"><html><head></head><body>"
    puts [Form html {
	-submit "Create New Account"
	-prolog "<p>This is a form to enter your account details</p>"
	-epilog "<p>When you create the account instructions will be emailed to you.  Make sure your email address is correct.</p>"
	details {
	    -legend "Account Details"
	    -inline 1

	    user {
		-inline 1
		-tooltip "Your preferred username (only letters, numbers and spaces)"
	    }
	    email {
		-inline 1
		-tooltip "Your email address"
		-value moop
	    }
	    hidden {
		-type hidden
	    }
	}
	
	passwords {
	    -text "Type in your preferred password, twice.  Leaving it blank will generate a random password for you."
	    -type password
	    -inline 1
	    -maxlength 16
	    -size 16
	    password {}
	    repeat {}
	}

	radio {
	    -legend "Personal illnesses"
	    -type radio
	    -inline 0
	    illness {
		-label none
		-checked ""
		-value 0
	    }
	    illness {
		-label lameness
		-value 1
	    }
	    illness {
		-label haltness
		-value 2
	    }
	    illness {
		-label blindness
		-value 2
	    }

	    select {
		-type select
		-name selname
		-inline 1
		-size 2
		option1 {
		    -type option
		    -value moop1
		    -label moop1
		    -option "option 1"
		}
		option2 {
		    -label moop2
		    -type option
		    -value moop2
		}
	    }
	}

	personal {
	    -label "Personal Information"
	    -inline 0

	    name {
		-name fullname
		-type text
		-tooltip "Full name to be used in email."
	    }
	    phone {
		-type text
		-tooltip "Phone number for official contact"
	    }
	}
    } -action http:moop.html]
    
    puts "</body>\n</html>"
}
