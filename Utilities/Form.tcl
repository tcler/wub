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

#package require Debug
Debug off form 10

package provide Form 2.0

namespace eval Form {
    variable Fdefaults [dict create {*}{
	textarea {-compact 0}
	form {method post}
	fieldset {vertical 0}
	submit {alt Submit}
	reset {alt Reset}
	option {}
    }]
    variable tabindex 0	;# taborder for fields

    # default - set attribute defaults for a given tag
    proc default {type args} {
	variable Fdefaults
	set d [Dict get? $Fdefaults $type]
	dict set Fdefaults $type [dict merge $d $args]
    }

    proc attr {T args} {
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

    proc fieldsetS {name args} {
	variable fieldsetA
	variable Fdefaults
	set config [dict merge [Dict get? $Fdefaults fieldset] [lrange $args 0 end-1]]
	if {$name ne "" && ![dict exists $config id]} {
	    dict set config id $name
	}
	set content "<[attr fieldset [Dict subset $config $fieldsetA]]>\n"
	if {[dict exists $config legend]} {
	    append content [<legend> [dict get $config legend]] \n
	}
	return $content
    }

    proc formS {name args} {
	variable formA
	variable Fdefaults
	set config [dict merge [Dict get? $Fdefaults form] [lrange $args 0 end-1]]
	if {$name ne "" && ![dict exists $config id]} {
	    dict set config id $name
	}
	set content "<[attr form [Dict subset $config $formA]]>\n"
	if {[dict exists $config legend]} {
	    append content [<legend> [dict get $config legend]] \n
	}
	return $content
    }

    # <form> and <fieldset>
    foreach {type} {form fieldset} {
	if {0 && $type eq "form"} {
	    set ti "variable tabindex 0"
	} else {
	    set ti ""
	}
	eval [string map [list @TI $ti @T $type] {
	    proc <@T> {args} {
		variable @TA
		variable Fdefaults
		set body [lindex $args end]
		set args [lrange $args 0 end-1]

		set name ""
		if {[llength $args]%2} {
		    set args [lassign $args name]
		} 

		set config [dict merge [Dict get? $Fdefaults @T] $args]
		if {$name ne "" && ![dict exists $config id]} {
		    dict set config id $name
		}

		if {[dict exists $config legend]} {
		    append content [<legend> [dict get $config legend]] \n
		}

		@TI
		append content [uplevel 1 [list subst $body]]
		set content [string trim $content " \t\n\r"]

		if {[dict exists $config vertical]
		    && [dict get $config vertical]
		} {
		    set content [string map {\n <br>\n} [string trim $content \n]]
		}

		return "<[attr @T [Dict subset $config $@TA]]>$content</@T>"
	    }
	}]
    }
    
    foreach {type} {legend} {
	eval [string map [list @T $type] {
	    proc <@T> {args} {
		variable @TA
		variable Fdefaults
		set config [dict merge [Dict get? $Fdefaults @T] [lrange $args 0 end-1]]
		return "<[attr @T [Dict subset $config $@TA]]>[uplevel 1 [list subst [lindex $args end]]]</@T>"
	    }
	}]
    }
    
    proc <select> {name args} {
	variable selectA
	variable Fdefaults
	set content [lindex $args end]
	set args [lrange $args 0 end-1]
	Debug.form {<select> $name ($args) content:'$content'}
	set config [dict merge [Dict get? $Fdefaults select] $args [list name $name]]

	if {![dict exists $config id]} {
	    if {[dict exists $config label]} {
		dict set config id $name
		set id $name
	    }
	} else {
	    set id [dict get $config id]
	}

	if {0 && ![dict exists $config tabindex]} {
	    variable tabindex
	    dict set config tabindex [incr tabindex]
	}

	set content [uplevel 1 [list subst $content]]

	set result "<[attr select [Dict subset $config $selectA]]>$content</select>"

	if {[dict exists $config title]} {
	    set title [list title [dict get $config title]]
	} else {
	    set title {}
	}

	if {[dict exists $config label]} {
	    set label [dict get $config label]
	    return [<label> for $id "$label $result"]
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
	eval [string map [list @T $type] {
	    proc <@T> {args} {
		variable @TA
		variable Fdefaults

		set content [lindex $args end]
		set args [lrange $args 0 end-1]
		set config [dict merge [Dict get? $Fdefaults @T] $args]
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

		return "<[attr @T [Dict subset $config $@TA]]>$content</@T>"
	    }
	}]
    }

    proc <textarea> {name args} {
	variable textareaA
	variable Fdefaults
	if {[llength $args] % 2} {
	    set content [lindex $args end]
	    set args [lrange $args 0 end-1]
	} else {
	    set content ""
	}
	set config [dict merge [Dict get? $Fdefaults textarea] $args [list name $name]]

	if {0 && ![dict exists $config tabindex]} {
	    variable tabindex
	    dict set config tabindex [incr tabindex]
	}

	if {$name ne "" && ![dict exists $config id]} {
	    dict set config id $name
	}

	if {![dict exists $config id]} {
	    if {[dict exists $config label]} {
		dict set config id $name
		set id $name
	    }
	} else {
	    set id [dict get $config id]
	}

	if {[dict exists $config -compact]
	    && [dict get $config -compact]
	} {
	    # remove initial spaces from Form
	    set content [::textutil::untabify2 [::textutil::undent [::textutil::tabify2 $content]]]
	}

	set title {}
	if {[dict exists $config title]
	    && ([dict exists $config label]
		|| [dict exists $config legend])
	} {
	    set title [list title $title]
	    dict unset config title
	}
	set result "<[attr textarea [Dict subset $config $textareaA]]>$content</textarea>"

	if {[dict exists $config label]} {
	    set label [dict get $config label]
	    return [<label> for $id "$label $result"]
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
    
    foreach type {reset submit} {
	eval [string map [list @T $type] {
	    proc <@T> {name args} {
		variable Fdefaults
		if {[llength $args] % 2} {
		    set content [lindex $args end]
		    set args [lrange $args 0 end-1]
		} else {
		    set content ""
		}
		set config [dict merge [Dict get? $Fdefaults @T] [list alt @T] $args [list name $name type @T]]
		
		if {0 && ![dict exists $config tabindex]} {
		    variable tabindex
		    dict set config tabindex [incr tabindex]
		}

		if {$content eq {}} {
		    if {[dict exists $config content]} {
			set content [dict get $config content]
		    } else {
			set content [string totitle @T]
		    }
		} else {
		    set content [uplevel 1 subst [list $content]]
		}

		variable imageA
		return [<button> $name {*}$config $content]
	    }
	}]
    }

    proc <button> {name args} {
	variable Fdefaults
	set config [dict merge [Dict get? $Fdefaults button] [lrange $args 0 end-1] [list name $name]]

	if {0 && ![dict exists $config tabindex]} {
	    variable tabindex
	    dict set config tabindex [incr tabindex]
	}

	variable buttonA
	return "<[attr button [Dict subset $config $buttonA]]>[uplevel 1 [list subst [lindex $args end]]]</button>"
    }
    
    foreach {itype attrs field} {
	password text value
	text text value
	hidden text value
	file file value
	image image src
    } {
	eval [string map [list @T $itype @A $attrs @F $field] {
	    proc <@T> {name args} {
		if {[llength $args] % 2} {
		    set value [lindex $args end]
		    set args [lrange $args 0 end-1]
		} else {
		    set value ""
		}
		
		variable @AA
		variable Fdefaults
		set config [dict merge [Dict get? $Fdefaults @T] $args [list name $name type @T @F [armour [uplevel 1 [list subst $value]]]]]

		if {0 && ![dict exists $config tabindex]} {
		    variable tabindex
		    dict set config tabindex [incr tabindex]
		}

		if {![dict exists $config id]} {
		    if {[dict exists $config label]} {
			dict set config id $name
			set id $name
		    }
		} else {
		    set id [dict get $config id]
		}

		# get sub-attributes of form "{subel attr} value"
		set sattr {label {} legend {}}
		dict for {k v} $config {
		    set k [split $k]
		    if {[llength $k] > 1} {
			dict set sattr [lindex $k 0] [lindex $k 1] $v
		    }
		}

		set result "<[attr input [Dict subset $config $@AA]]>"

		if {[dict exists $config label]} {
		    set label [dict get $config label]
		    return [<label> for $id {*}[dict get $sattr label] "$label $result"]
		} elseif {[dict exists $config legend]} {
		    set legend [dict get $config legend]
		    return "[<span> {*}[dict get $sattr legend] $legend]$result"
		} else {
		    return $result
		}
	    }
	}]
    }

    proc <selectlist> {name args} {
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
    
    proc <selectset> {args} {
	return [uplevel 1 [list <selectlist> {*}$args]
    }
    
    foreach type {radio check} sub {"" box} {
	eval [string map [list @T $type @S $sub] {
	    proc <@Tset> {name args} {
		variable Fdefaults
		set rsconfig [dict merge [Dict get? $Fdefaults @T] [lrange $args 0 end-1] [list name $name type @T]]
		set boxes [lindex $args end]
		set result {}
		
		set accum ""
		foreach {content value} $boxes {
		    set config [dict merge [Dict get? $Fdefaults @T@S] $rsconfig]
		    if {[string match +* $content]} {
			dict set config checked 1
			set content [string trim $content +]
		    } else {
			catch {dict unset config checked}
		    }
		    dict set config value $value
		    lappend result [uplevel 1 [list <@T@S> $name {*}$config $content]]
		    set accum ""
		}
		if {[dict exists $rsconfig vertical]
		    && [dict get $rsconfig vertical]} {
		    set joiner <br>
		} else {
		    set joiner \n
		}
		
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
	
	eval [string map [list @T $type$sub] {
	    proc <@T> {name args} {
		if {[llength $args] % 2} {
		    set content [lindex $args end]
		    set args [lrange $args 0 end-1]
		} else {
		    set content ""
		}
		variable boxA
		variable Fdefaults
		set config [dict merge $args [list name $name type @T] [Dict get? $Fdefaults @T]]

		set content [uplevel 1 [list subst $content]]
		if {![dict exists $config label] && $content ne ""} {
		    dict set config label $content
		    set content ""
		}

		if {0 && ![dict exists $config tabindex]} {
		    variable tabindex
		    dict set config tabindex [incr tabindex]
		}

		if {![dict exists $config id]} {
		    if {[dict exists $config label]} {
			dict set config id $name
			set id $name
		    }
		} else {
		    set id [dict get $config id]
		}
		set result "<[attr input [Dict subset $config $boxA]]>$content"
		if {[dict exists $config label]
		    && [dict get $config label] ne ""
		} {
		    set label [dict get $config label]
		    if {[dict exists $config title]} {
			return [<label> for $id title [dict get $config title] "$label $result"]
		    } else {
			return [<label> for $id "$label $result"]
		    }
		} else {
		    return $result
		}
	    }
	}]
    }

    variable scripting 1	;# permit scripting options
    proc init {args} {
	if {$args ne {}} {
	    variable {*}$args
	}
	variable scripting
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
    }

    namespace export -clear *
    namespace ensemble create -subcommands {}
}
Form init

namespace import ::Form::<*>

if {[info exists argv0] && ($argv0 eq [info script])} {
    #Debug on form 10
    Form default textarea rows 8 cols 60

    puts "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"><html><head></head><body>"
    puts [<form> xxx action http:moop.html {
	[<p> "This is a form to enter your account details"]
	[<fieldset> details vertical 1 title "Personal Details" {
	    [<legend> "Account Details"]
	    [<text> user legend "User name" title "Your preferred username (only letters, numbers and spaces)"]
	    [<text> email title "Your email address" moop]
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
	[<fieldset> personal {
	    [<label> "Personal Information"]
	    [<text> fullname title "Full name to be used in email."]
	    [<text> phone title "Phone number for official contact"]
	}]
	[<p> "When you create the account instructions will be emailed to you.  Make sure your email address is correct."]
	[<textarea> te compact 1 {
	    This is some default text to be getting on with
	    It's fairly cool.  Note how it's left aligned.
	}]
	<br>[<submit> submit "Create New Account"]

	[<br>]
	[<fieldset> permissions {
	    [<legend> Permissions]
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
    puts [<form> yyy action http:moop.html {
	[<fieldset> fsearch {
	    [<submit> submit "Search"]
	    [<text> kw title "Search Text"]
	    [<radioset> scope title "Search scope" {
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
	    [<select> sort style "float:left" title "Sort By" {
		[<option> title value title]
		[<option> author value author]
	    }]
	    [<selectset> sort1 style "float:left" title "Sort By" {
		title
		author
	    }]
	}]
    }]
    puts "</body>\n</html>"
}
