# Form.html
# this is an experiment in constructing forms.
#
# adds:
#
# Grouping and Labelling:
# <form> id {attrs} content
# <fieldset> id {attrs} content
# Group content into forms/fieldsets - content is evaluated
#
# <legend> {attrs} content
# provide a legend - useful in fieldsets

# Menus
# <select> name {attrs} content
# <option> {attrs} value
# <optgroup> {attrs} value
#
# select menus.  content and value are evaluated.

# Input Fields
# <password> name {attrs} value
# <text> name {attrs} value
# <hidden> name {attrs} value
# <file> name {attrs} value
# <image> name {attrs} src
# <textarea> name {attrs} content
# <button> name {attrs} content
# <reset> name {attrs} content
# <submit> name {attrs} content
# <radio> name {attrs} text
# <checkbox> name {attrs} text
#
# input boxes - content/value/text are evaluated

# Sets - radio and checkboxes in groups
# <radioset> name {attrs} content
# <checkset> name {attrs} content
# <selectset> name {attrs} content
#
# Sets group together radioboxes, checkboxes and selects into a single coherent unit.
# each content is assumed to be a list of name/value pairs

if {[info exists argv0] && ([info script] eq $argv0)} {
    lappend auto_path [file dirname [file normalize [info script]]] ../Utilities/ ../extensions/
}

package require textutil
package require Dict
package require Html
package require OO

package require Debug
Debug define form 10

package provide Form 2.0

set ::API(Utilities/Form) {
    {
	HTML Form generator
    }
}

class create ::FormClass {
    # default - set attribute defaults for a given tag
    method default {type args} {
	variable Fdefaults
	set d [dict get? $Fdefaults $type]
	dict set Fdefaults $type [dict merge $d $args]
    }

    method attr {T args} {
	if {[llength $args] == 1} {
	    set args [lindex $args 0]
	}
	set result $T
	foreach {n v} $args {
	    if {[string match -* $n]} continue
	    if {$n in {checked disabled selected}} {
		if {$v} {
		    lappend result $n
		}
	    } else {
		lappend result "[string trim $n]='[armour [string trim $v]]'"
	    }
	}
	return [join $result]
    }

    method fieldsetS {name args} {
	variable fieldsetA
	variable Fdefaults
	set config [dict merge [dict get? $Fdefaults fieldset] [lrange $args 0 end-1]]
	if {![dict exists $config id]} {
	    if {$name ne ""} {
		dict set config id $name
	    } else {
		variable uniqID
		dict set config id F[incr uniqID]
	    }
	}
	set content "<[my attr fieldset [dict in $config $fieldsetA]]>\n"
	if {[dict exists $config legend]} {
	    append content [<legend> [dict get $config legend]] \n
	}
	return $content
    }

    method formS {name args} {
	variable formA
	variable Fdefaults
	variable tabindex 0
	set config [dict merge [dict get? $Fdefaults form] [lrange $args 0 end-1]]
	if {![dict exists $config id]} {
	    if {$name ne ""} {
		dict set config id $name
	    } else {
		variable uniqID
		dict set config id F[incr uniqID]
	    }
	}
	set content "<[my attr form [dict in $config $formA]]>\n"
	if {[dict exists $config legend]} {
	    append content [<legend> [dict get $config legend]] \n
	}
	return $content
    }

    # <form> and <fieldset>
    foreach {type} {form fieldset} {
	if {$type eq "form"} {
	    set ti {
		variable tabindex 0
	    }
	} else {
	    set ti ""
	}
	eval [string map [list %TI% $ti %T% $type] {
	    method <%T%> {args} {
		Debug.form {[self] defining %T%}
		variable %T%A
		variable Fdefaults

		set body [lindex $args end]
		set args [lrange $args 0 end-1]

		# get form name from args, if present
		set name ""
		if {[llength $args]%2} {
		    set args [lassign $args name]
		}

		set config [dict merge [dict get? $Fdefaults %T%] $args]
		%TI%

		my metadata $name $config

		variable tabular
		set otab $tabular
		if {[dict exists $config tabular] && [dict get $config tabular]} {
		    set tabular 1
		} else {
		    set tabular 0
		}

		# ensure an id
		if {![dict exists $config id]} {
		    if {$name ne ""} {
			dict set config id $name
		    } else {
			variable uniqID
			dict set config id F[incr uniqID]
		    }
		}

		# evaluate form content
		set body [uplevel 1 [list subst $body]]
		set body [string trim $body " \t\n\r"]

		set content ""
		if {[dict exists $config legend]} {
		    append content [<legend> [dict get $config legend]] \n
		}
		if {$tabular} {
		    set body [<tr> [string map {\n </tr>\n<tr>} $body]]
		    append content \n [<table> $body] \n
		} elseif {[dict exists $config vertical]
			   && [dict get $config vertical]
		      } {
		    append content $body
		    set content [string map {\n <br>\n} $content]
		} else {
		    append content $body
		}
		set tabular $otab	;# restore old tabular value

		return "<[my attr %T% [dict in $config $%T%A]]>$content</%T%>"
	    }
	}]
    }

    # process or return metadata
    method metadata {{name ""} {config ""}} {
	variable metadata
	if {$name eq ""} {
	    return $metadata	;# want all metadata
	}

	if {$config eq ""} {
	    return [dict get? $metadata $name]	;# want metadata for field
	}

	set n $name; set i 0
	while {[dict exists $metadata $n]} {
	    # ensure metadata is unique by name
	    set n ${name}_[incr i]
	}

	Debug.form {[self] metadata '$n': $config}
	dict set metadata $n $config
    }

    foreach {type} {legend} {
	eval [string map [list %T% $type] {
	    method <%T%> {args} {
		variable %T%A
		variable Fdefaults
		set config [dict merge [dict get? $Fdefaults %T%] [lrange $args 0 end-1]]
		return "<[my attr %T% [dict in $config $%T%A]]>[uplevel 1 [list subst [lindex $args end]]]</%T%>"
	    }
	}]
    }

    method <select> {name args} {
	variable selectA
	variable Fdefaults

	set content [lindex $args end]
	set args [lrange $args 0 end-1]
	set config [dict merge [dict get? $Fdefaults select] $args [list name $name]]

	if {![dict exists $config id]} {
	    dict set config id $name
	}
	set id [dict get $config id]

	if {![dict exists $config tabindex]} {
	    variable tabindex
	    dict set config tabindex [incr tabindex]
	}

	set content [uplevel 1 [list subst $content]]

	set result "<[my attr select [dict in $config $selectA]]>$content</select>"
	my metadata $name $config	;# remember config for field

	if {[dict exists $config title]} {
	    set title [list title [dict get $config title]]
	} else {
	    set title {}
	}

	set label [dict get? $config label]
	variable tabular
	if {$tabular} {
	    if {$label eq ""} {
		set label $name
	    }
	    return "[<td> class label [<label> for $id $label]] [<td> class field $result]"
	} elseif {$label ne ""} {
	    return "[<label> for $id $label] $result"
	} elseif {[dict exists $config legend]} {
	    set legend [dict get $config legend]

	    # get sub-attributes of form "{subel attr} value"
	    set sattr {fieldset {} legend {}}
	    dict for {k v} $config {
		set k [split $k]
		if {[llength $k] > 1} {
		    dict set sattr [lindex $k 0] [lindex $k 1] $v
		}
	    }

	    return [<fieldset> "" {*}[dict get $sattr fieldset] {*}$title {
		[<legend> {*}[dict get $sattr legend] $legend]
		$result
	    }]
	} else {
	    return $result
	}
    }

    foreach {type} {option optgroup} {
	eval [string map [list %T% $type] {
	    method <%T%> {args} {
		variable %T%A
		variable Fdefaults

		set content [lindex $args end]
		set args [lrange $args 0 end-1]
		set config [dict merge [dict get? $Fdefaults %T%] $args]
		if {$content eq ""} {
		    set content [dict get $config value]
		} else {
		    set content [uplevel 1 [list subst $content]]
		}

		if {[dict exist $config select?]} {
		    if {[dict get $config select?] eq [dict get $config value]} {
			dict set config selected 1
		    }
		}

		return "<[my attr %T% [dict in $config $%T%A]]>$content</%T%>"
	    }
	}]
    }

    method <textarea> {name args} {
	variable textareaA
	variable Fdefaults
	if {[llength $args] % 2} {
	    set content [lindex $args end]
	    set args [lrange $args 0 end-1]
	} else {
	    set content ""
	}
	set config [dict merge [dict get? $Fdefaults textarea] $args [list name $name]]

	if {![dict exists $config tabindex]} {
	    variable tabindex
	    dict set config tabindex [incr tabindex]
	}

	# ensure an id
	if {![dict exists $config id]} {
	    dict set config id $name
	}
	set id [dict get $config id]

	if {[dict exists $config compact]} {
	    if {[dict get $config compact]} {
		# remove initial spaces from Form
		set content [::textutil::undent [::textutil::untabify $content]]
	    }
	    dict unset config compact
	}
	
	set title {}
	if {[dict exists $config title]
	    && ([dict exists $config label]
		|| [dict exists $config legend])
	} {
	    set title [list title $title]
	    dict unset config title
	}
	set result "<[my attr textarea [dict in $config $textareaA]]>$content</textarea>"
	my metadata $name $config	;# remember config for field

	set label [dict get? $config label]
	variable tabular
	if {$tabular} {
	    if {$label eq ""} {
		set label $name
	    }
	    return "[<td> class label [<label> for $id $label]] [<td> class field $result]"
	} elseif {$label ne ""} {
	    return "[<label> for $id $label] $result"
	} elseif {[dict exists $config legend]} {
	    set legend [dict get $config legend]

	    # get sub-attributes of form "{subel attr} value"
	    set sattr {fieldset {} legend {}}
	    dict for {k v} $config {
		set k [split $k]
		if {[llength $k] > 1} {
		    dict set sattr [lindex $k 0] [lindex $k 1] $v
		}
	    }

	    return [<fieldset> "" {*}[dict get $sattr fieldset] {*}$title {
		[<legend> {*}[dict get $sattr legend] $legend]
		$result
	    }]
	} else {
	    return $result
	}
    }

    # <reset> and <submit>
    foreach type {reset submit} {
	eval [string map [list %T% $type] {
	    method <%T%> {name args} {
		variable Fdefaults
		if {[llength $args] % 2} {
		    set content [lindex $args end]
		    set args [lrange $args 0 end-1]
		} else {
		    set content ""
		}
		set config [dict merge [dict get? $Fdefaults %T%] [list alt %T%] $args [list name $name type %T%]]
		
		if {![dict exists $config tabindex]} {
		    variable tabindex
		    dict set config tabindex [incr tabindex]
		}

		if {$content eq {}} {
		    if {[dict exists $config content]} {
			set content [dict get $config content]
		    } else {
			set content [string totitle %T%]
		    }
		} else {
		    set content [uplevel 1 subst [list $content]]
		}

		my metadata $name $config	;# remember config for field
		variable imageA
		return [<button> $name {*}$config $content]
	    }
	}]
    }

    method <button> {name args} {
	if {[llength $args]%2} {
	    set content [lindex $args end]
	    set args [lrange $args 0 end-1]
	} else {
	    set content ""
	}
	variable Fdefaults
	set config [dict merge [dict get? $Fdefaults button] $args [list name $name]]

	if {![dict exists $config tabindex]} {
	    variable tabindex
	    dict set config tabindex [incr tabindex]
	}

	my metadata $name $config	;# remember config for field
	variable buttonA
	return "<[my attr button [dict in $config $buttonA]]>$content</button>"
    }
    
    foreach {itype attrs field} {
	password text value
	text text value
	hidden text value
	file file value
	image image src
    } {
	eval [string map [list %T% $itype %A% $attrs %F% $field] {
	    method <%T%> {name args} {
		if {[llength $args]%2} {
		    set value [lindex $args end]
		    set args [lrange $args 0 end-1]
		} else {
		    set value ""
		}
		
		variable %A%A
		variable Fdefaults
		set config [dict merge [dict get? $Fdefaults %T%] $args [list name $name type %T% %F% [armour [uplevel 1 [list subst $value]]]]]

		if {![dict exists $config tabindex]} {
		    variable tabindex
		    dict set config tabindex [incr tabindex]
		}

		if {![dict exists $config id]} {
		    dict set config id $name
		}
		set id [dict get $config id]

		# get sub-attributes of form "{subel attr} value"
		set sattr {label {} legend {}}
		dict for {k v} $config {
		    set k [split $k]
		    if {[llength $k] > 1} {
			dict set sattr [lindex $k 0] [lindex $k 1] $v
		    }
		}

		set result "<[my attr input [dict in $config $%A%A]]>"
		my metadata $name $config	;# remember config for field

		set label [dict get? $config label]
		variable tabular
		if {$tabular} {
		    if {$label eq ""} {
			set label $name
		    }
		    return "[<td> class label [<label> for $id $label]] [<td> class field $result]"
		} elseif {$label ne ""} {
		    return "[<label> for $id $label] $result"
		} elseif {[dict exists $config legend]} {
		    set legend [dict get $config legend]
		    return "[<span> {*}[dict get $sattr legend] $legend] $result"
		} else {
		    return $result
		}
	    }
	}]
    }

    method <selectlist> {name args} {
	set result ""
	foreach line [lindex $args end] {
	    set line [string trim $line]
	    if {$line eq ""} continue
	    if {[string match +* $line]} {
		set term [list <option> selected 1 {*}[string trimleft $line +]]
	    } else {
		set term [list <option> {*}$line]
	    }
	    append result \[ $term \] \n
	}
	return [uplevel 1 [list <select> $name {*}[lrange $args 0 end-1] $result]]
    }
    
    method <selectset> {args} {
	return [uplevel 1 [list <selectlist> {*}$args]]
    }

    # <radioset> <checkset> <radio> <checkbox>
    foreach type {radio check} sub {"" box} {
	eval [string map [list %T% $type %S% $sub] {
	    method <%T%set> {name args} {
		variable Fdefaults
		set rsconfig [dict merge [dict get? $Fdefaults %T%] [lrange $args 0 end-1] [list name $name type %T%]]
		set boxes [lindex $args end]
		set result {}
		
		set accum ""
		foreach {content value} $boxes {
		    set config [dict merge [dict get? $Fdefaults %T%%S%] $rsconfig]
		    if {[string match +* $content]} {
			dict set config checked 1
			set content [string trim $content +]
		    } else {
			catch {dict unset config checked}
		    }
		    dict set config value $value
		    lappend result [uplevel 1 [list <%T%%S%> $name {*}$config $content]]
		    set accum ""
		}

		if {[dict exists $rsconfig vertical]
		    && [dict get $rsconfig vertical]} {
		    set joiner <br>
		} else {
		    set joiner \n
		}
		
		my metadata $name $config	;# remember config for field

		if {[dict exists $rsconfig legend]} {
		    set legend [dict get $rsconfig legend]

		    # get sub-attributes of form "{subel attr} value"
		    set sattr {fieldset {} legend {}}
		    dict for {k v} $config {
			set k [split $k]
			if {[llength $k] > 1} {
			    dict set sattr [lindex $k 0] [lindex $k 1] $v
			}
		    }

		    return [<fieldset> "" {*}[dict get $sattr fieldset] {
			[<legend> {*}[dict get $sattr legend] $legend]
			[join $result $joiner]
		    }]
		} else {
		    return [join $result $joiner]
		}
	    }
	}]
	
	eval [string map [list %T% $type$sub] {
	    method <%T%> {name args} {
		if {[llength $args] % 2} {
		    set content [lindex $args end]
		    set args [lrange $args 0 end-1]
		} else {
		    set content ""
		}
		variable boxA
		variable Fdefaults
		set config [dict merge $args [list name $name type %T%] [dict get? $Fdefaults %T%]]

		set content [uplevel 1 [list subst $content]]
		if {![dict exists $config label] && $content ne ""} {
		    dict set config label $content
		    set content ""
		}

		if {![dict exists $config tabindex]} {
		    variable tabindex
		    dict set config tabindex [incr tabindex]
		}

		if {![dict exists $config id]} {
		    variable uniqID
		    dict set config id F[incr uniqID]
		}
		set id [dict get $config id]
		set result "<[my attr input [dict in $config $boxA]]>$content"
		my metadata $name $config	;# remember config for field

		if {[dict exists $config label]
		    && [dict get $config label] ne ""
		} {
		    set label [dict get $config label]
		    if {[dict exists $config title]} {
			return "[<label> for $id title [dict get $config title] $label] $result"
		    } else {
			return "[<label> for $id $label] $result"
		    }
		} else {
		    return $result
		}
	    }
	}]
    }

    method layout_parser {fname args} {
	variable layoutcache
	if {[info exists layoutcache($fname-$args)]} {
	    return $layoutcache($fname-$args)
	}
	if {[llength $args]%2 != 1} {
	    error "syntax: layout name ?option_pairs? {content}"
	}

	set script [lindex $args end]
	set args [lrange $args 0 end-1]

	variable tags
	package require parsetcl
	set parse [::parsetcl simple_parse_script $script]

	set rendered {}
	set known {}
	set form {}
	set frag 0

	parsetcl walk_tree parse index Cd {
	    if {[llength $index] == 1} {
		# body
		set line [lindex $parse {*}$index]
		Debug.form {walk: '$line'}
		set cargs [lassign $line . . . left]
		set fc [lindex $left 2]
		if {[string match L* [lindex $left 0]] && $fc in $tags} {
		    switch -- $fc {
			form {
			    # use this command to inject args into eventual <form>
			    foreach ca $cargs {
				lappend args [parsetcl unparse $ca]
			    }
			}

			fieldset {
			    set name [lindex $cargs 0]
			    if {[dict exists $known $name]} {
				error "redeclaration of '$name' in '[parsetcl unparse $line]'"
			    }
			    
			    set content [lindex $cargs end]
			    set fsargs {}
			    foreach ca [lrange $cargs 1 end-1] {
				lappend fsargs [parsetcl unparse $ca]
			    }
			    set name [string trim [lindex $name 2] .]
			    
			    # a fieldset's content is itself a form - recursively parse
			    set content [lindex $content 2]
			    set fs [lindex [uplevel 1 [list [self] layout_parser $name \n$content\n]] 1]
			    set fs "\[[self] <fieldset> $name $fsargs [list \n$fs\n]\]"
			    Debug.form {fieldset: $name: '$content' -> ($fs)}
			    dict set known $name \n$fs
			}
			
			select {
			    set name [lindex $cargs 0]
			    if {[dict exists $known $name]} {
				error "redeclaration of '$name' in '[parsetcl unparse $line]'"
			    }
			    
			    set content [lindex $cargs end]
			    set fsargs {}
			    foreach ca [lrange $cargs 1 end-1] {
				lappend fsargs [parsetcl unparse $ca]
			    }
			    set fsargs [join $fsargs]
			    set name [string trim [lindex $name 2] .]
			    
			    # a select's content is itself a form - recursively parse
			    set content [lindex $content 2]
			    set fs [lindex [uplevel 1 [list [self] layout_parser $name \n$content\n]] 1]
			    set fs "\[[self] <select> $name $fsargs [list \n$fs\n]\]"
			    Debug.form {select: $name: '$content' -> ($fs)}
			    dict set known $name \n$fs
			}
			
			legend {
			    lset parse {*}$index 3 2 <legend>	;# make it a form command
			    set allargs {}
			    foreach a $cargs {				# body
				parsetcl walk_tree a li Cd {
				    set lleft [lindex $a {*}$li 3]
				    set sc [lindex $lleft 2]
				    if {[string match L* [lindex $lleft 0]]
					&& [string match <*> $sc]
					&& [string trim $sc <>] in $tags
				    } {
					Debug.form {legend subcommand: '$sc'}
					# this is a form <tag> command
					# rewrite it so it appears to be coming from [self]
					lset a {*}$li 3 2 [list [self] $sc]
				    }
				}
				set up [parsetcl unparse $a]
				Debug.form {legend: $up}
				lappend allargs $up
			    }
			    dict set known [incr frag] "\[[self] <$fc> [join $allargs]\]"
			}
			
			default {
			    lset parse {*}$index 3 2 <$fc>	;# make it a form command
			    set name [lindex $parse {*}$index 4 2]
			    if {![string match L* [lindex $parse {*}$index 4 0]]} {
				error "'[lindex $parse {*}$index 4 2]' is not a name in [parsetcl unparse $line]"
			    }
			    if {[dict exists $known $name]} {
				error "redeclaration of '$name' in '[parsetcl unparse $line]'"
			    }

			    set allargs {}
			    foreach a $cargs {
				lappend allargs [parsetcl unparse $a]
			    }
			    dict set known $name "\[[self] <$fc> [join $allargs]\]"
			}
		    }
		} else {
		    Debug.form {'$fc' is not a form tag}
		    dict set known [incr frag] \[[parsetcl unparse $line]\]
		}
	    } else {
		# this is a normal command within the body of a tag - leave it alone
	    }
	} C.* {
	    dict set known [incr frag] \[[parsetcl unparse $line]\]
	}

	set result "$fname $args [list [join [dict values $known] \n]\n]"
	set layoutcache($fname-$args) $result
	return $result
    }

    method layout {fname args} {
	set result [my layout_parser $fname {*}$args]
	Debug.form {[self] layout: ($result)}
	return [uplevel 1 [list [self] <form> {*}$result]]
    }

    constructor {args} {
	variable Fdefaults [dict create {*}{
	    textarea {compact 0}
	    form {method post tabular 0}
	    fieldset {vertical 0}
	    submit {alt Submit}
	    reset {alt Reset}
	    option {}
	}]
	variable metadata {}	;# remember field configs
	variable tabindex 0	;# taborder for fields
	variable tabular 0	;# tabular form elements
	variable uniqID 0	;# unique ID for fields
	variable scripting 1	;# permit scripting options

	if {$args ne {}} {
	    variable {*}$args
	}

	if {$scripting} {
	    variable coreA {id class style title}
	} else {
	    variable coreA {id class title}
	}
	variable i18nA {lang dir}

	set commonOn {
	    onclick ondblclick onmousedown onmouseup onmouseover
	    onmousemove onmouseout onkeypress onkeydown onkeyup
	}
	if {$scripting} {
	    variable eventA $commonOn
	} else {
	    variable eventA {}
	}

	foreach on [list {*}$commonOn onsubmit onfocus onblur onselect onchange] {
	    if {$scripting} {
		set $on $on
	    } else {
		set $on ""
	    }
	}

	variable allA [subst {
	    $coreA
	    $i18nA
	    $eventA
	}]

	variable fieldsetA $allA
	variable accessA [subst {accesskey $allA}]
	variable formA [subst {action method enctype accept-charset accept $onsubmit $allA}]
	variable fieldA [subst {name disabled size tabindex accesskey $onfocus $onblur value $allA}]
	variable textareaA [subst {name rows cols disabled readonly tabindex accesskey $onfocus $onblur $onselect $onchange $allA}]
	variable buttonA [subst {value checked $fieldA $onselect $onchange type}]
	variable boxA [subst {value checked $fieldA $onselect $onchange type}]
	variable textA [subst {$fieldA readonly maxlength $onselect $onchange alt type}]
	variable imageA [subst {src alt $fieldA $onselect $onchange type}]
	variable fileA [subst {$fieldA accept $onselect $onchange type}]
	variable selectA [subst {name size multiple disabled tabindex $onfocus $onblur $onchange $allA}]
	variable optgroupA [subst {disabled label $allA}]
	variable optionA [subst {selected disabled label value $allA}]
	variable legendA [subst {$allA}]

	variable tags {}
	foreach m [info class methods FormClass -all] {
	    if {![string match <* $m]} continue
	    lappend tags [string trim $m <>] {}
	}
    }
    set meth {}
    foreach m [info class methods FormClass -all -private] {
	if {![string match <* $m]} continue
	lappend meth $m
	interp alias {} $m {} Form $m
    }
    export {*}[info class methods FormClass -all] {*}$meth ;# ensure <form>s are visible
}
::FormClass create ::Form

if {[info exists argv0] && ($argv0 eq [info script])} {
    Form default textarea rows 8 cols 60

    if {0} {
	puts "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"><html><head></head><body>"
	puts [<form> xxx action http:moop.html {
	    [<p> "This is a form to enter your account details"]
	    [<fieldset> details vertical 1 title "Account Details" {
		[<legend> "Account Details"]
		[<text> user label "User name" title "Your preferred username (only letters, numbers and spaces)"]
		[<text> email label "Email Address" title "Your email address" moop]
		[<hidden> hidden moop]
	    }]
	    [<fieldset> passwords maxlength 16 size 16 {
		[<legend> "Passwords"]
		[<p> "Type in your preferred password, twice.  Leaving it blank will generate a random password for you."]
		[<password> password]
		[<password> repeat]
	    }]
	    [<radioset> illness legend "Personal illnesses" {
		+none 0
		lameness 1
		haltness 2
		blindness 2
	    }]
	    [<checkset> illness vertical 1 legend "Personal illnesses" {
		+none 0
		lameness 1
		haltness 2
		blindness 2
	    }]
	    [<select> selname legend "Shoe Size" title "Security dictates that we know your approximate shoe size" {
		[<option> value moop1 label moop1 value 1 "Petit"]
		[<option> label moop2 value moop2 value 2 "Massive"]
	    }]
	    [<fieldset> personal tabular 1 legend "Personal Information" {
		[<text> fullname label "full name" title "Full name to be used in email."] [<text> phone label phone title "Phone number for official contact"]
	    }]
	    [<p> "When you create the account instructions will be emailed to you.  Make sure your email address is correct."]
	    [<textarea> te compact 1 {
		This is some default text to be getting on with
		It's fairly cool.  Note how it's left aligned.
	    }]
	    <br>[<submit> submit "Create New Account"]
	    
	    [<br>]
	    [<fieldset> permissions -legend Permissions {
		[<fieldset> gpermF style "float:left" title "Group Permissions." {
		    [<legend> Group]
		    [<checkbox> gperms title "Can group members read this page?" value 1 checked 1 read]
		    [<checkbox> gperms title "Can group members modify this page?" value 2 checked 1 modify]
		    [<checkbox> gperms title "Can group members add to this page?" value 4 checked 1 add]
		    [<br>][<text> group title "Which group owns this page?" label "Group: "]
		}]
		[<fieldset> opermF style "float:left" title "Default Permissions." {
		    [<legend> Anyone]
		    [<checkbox> operms title "Can anyone read this page?" value 1 checked 1 read]
		    [<checkbox> operms title "Can anyone modify this page?" value 2 modify]
		    [<checkbox> operms title "Can anyone add to this page?" value 4 add]
		}]
	    }]
	    [<br>]
	    [<div> class buttons [subst {
		[<submit> class positive {
		    [<img> src /images/icons/tick.png alt ""] Save
		}]
		
		[<a> href /password/reset/ [subst {
		    [<img> src /images/icons/textfield_key.png alt ""] Change Password
		}]]
		
		[<a> href "#" class negative [subst {
		    [<img> src /images/icons/cross.png alt ""] Cancel
		}]]
	    }]]
	}]
	puts "<hr />"
	set body {
	    [<fieldset> fsearch {
		[<submit> submit "Search"]
		[<text> kw title "Search Text"]
		[<br> clear both]
		[<radioset> scope {fieldset style} "float:left" legend "Search scope" {
		    +site 0
		    section 1
		}]
		[<select> newer {fieldset style} "float:left" legend "Newer Than" {
		    [<option> week value "last week"]
		    [<option> fortnight value "last fortnight"]
		    [<option> month value "last month"]
		    [<option> year value "last year"]
		}]
		[<select> older {fieldset style} "float:left" legend "Older Than" {
		    [<option> week value "last week"]
		    [<option> fortnight value "last fortnight"]
		    [<option> month value "last month"]
		    [<option> year value "last year"]
		}]
		[<select> sort {fieldset style} "float:left" legend "Sort By" {
		    [<option> title value title]
		    [<option> author value author]
		}]
	    }]
	    [<fieldset> sr1 {
		[<radioset> scope1 label "Search scope" {
		    +site 0
		    section 1
		}]
		[<select> newer1 label "Newer Than" {
		    [<option> week value "last week"]
		    [<option> fortnight value "last fortnight"]
		    [<option> month value "last month"]
		    [<option> year value "last year"]
		}]
		[<select> older1 label "Older Than" {
		    [<option> week value "last week"]
		    [<option> fortnight value "last fortnight"]
		    [<option> month value "last month"]
		    [<option> year value "last year"]
		}]
		[<selectset> sort1 label "Sort By" {
		    title
		    author
		}]
	    }]
	}
	puts [<form> yyy action http:moop.html $body]
	puts "</body>\n</html>"
    }

    if {1} {
	Debug on form 10
	puts "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"><html><head></head><body>"
	puts [Form layout foo {
	    fieldset fsearch {
		submit submit "Search"
		text kw title "Search Text"
		<br> clear both
		radioset scope {fieldset style} "float:left" legend "Search scope" {
		    +site 0
		    section 1
		}
		select newer {fieldset style} "float:left" legend "Newer Than" {
		    option week value "last week"
		    option fortnight value "last fortnight"
		    option month value "last month"
		    option year value "last year"
		}
		select older {fieldset style} "float:left" legend "Older Than" {
		    option week value "last week"
		    option fortnight value "last fortnight"
 		    option month value "last month"
		    option year value "last year"
		}
		select sort {fieldset style} "float:left" legend "Sort By" {
		    option title value title
		    option author value author
		}
	    }
	    fieldset sr1 style moop {
		radioset scope1 label "Search scope" {
		    +site 0
		    section 1
		}
		select newer1 label "Newer Than" {
		    option week value "last week"
		    option fortnight value "last fortnight"
		    option month value "last month"
		    option year value "last year"
		}
		select older1 label "Older Than" {
		    option week value "last week"
		    option fortnight value "last fortnight"
		    option month value "last month"
		    option year value "last year"
		}
		selectset sort1 label "Sort By" {
		    title
		    author
		}
	    }
	    text fullname label "full name"
	    button foo
	    radioset rs ...
	    text text ...
	}]
	puts "</body>\n</html>"
    }
}
# vim: ts=8:sw=4:noet
