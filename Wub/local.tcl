lappend ::auto_path [file normalize ../../Awards/]

if {0} {
    source testDirectOO.tcl
    Nub domain /testD/ Direct object TestD ctype x-text/html-fragment
    Debug off direct 10
}

if {0} {
    Nub domain /awards/ Awards
}
if {0} {
    Nub domain /simplicio/ Simplicio
    Nub domain /sinorca/ Sinorca
}

Debug on nub 10
Debug on direct 10
#Debug on url 10
#Debug on cookies 10
Debug on Httpd
#Debug on HttpdLow
Debug on Watchdog
#Debug on chan
Debug on convert
#Debug on STX 10
Debug on mason 10
Debug on entity 10
Debug on image 10
Debug on cache 10
Debug on sinorca 10
Debug on RAM 10

if {0} {
    Nub code /noncookie/ {
	set r [Http NoCache $r]
	set cdict [Dict get? $r -cookies]
	if {[catch {Cookies fetch $cdict -name cookie} cl]} {
	    <p> "No Cookies for YOU"
	} else {
	    <p> "Got cookies!  $cl"
	}
    }

    Nub code /cookie/set {
	set r [Http NoCache $r]
	
	# add in the cookies
	set cdict [Dict get? $r -cookies]
	set cdict [Cookies add $cdict -path /cookie/ -name cookie -value cookie]
	set cdict [Cookies add $cdict -path /cookie/set/ -name cookie -value set]
	set cdict [Cookies add $cdict -path /cookie/test/ -name cookie -value test]
	set cdict [Cookies add $cdict -path /noncookie/ -name cookie -value noncookie]
	set cdict [Cookies add $cdict -name cookie -value nopath]
	dict set r -cookies $cdict
	
	<p> "Set you some Cookies"
    }
    
    Nub code /cookie/ {
	set cdict [Dict get? $r -cookies]
	
	set result [dict get $r -content]
	if {[catch {Cookies fetch $cdict -name cookie} cl]} {
	    append result \n [<p> "No Cookies for YOU"]
	} else {
	    append result \n [<p> "Got cookies!  $cl"]
	}
    }
    
    Nub code /cookie/test {
	set r [Http NoCache $r]
	set cdict [Dict get? $r -cookies]
	if {[catch {Cookies fetch $cdict -name cookie} cl]} {
	    <p> "No Cookies for YOU"
	} else {
	    <p> "Got cookies!  $cl"
	}
    }
}

if {0} {
    package require Login 
    Debug on login 100
    Nub domain /login/ Direct object {Login ::L account {db accountdb file account.db layout {user:S password:S args:S}} cpath /cookie/ permissive 0 jQ 1 autocommit 1} ctype x-text/html-fragment

    Nub code /login/test {
	set r [::L /form $r]
	set user [::L user $r]
	set cdict [Dict get? $r -cookies]
	
	set result [dict get $r -content]
	append result [<div> id message {}]
	append result [<p> "User: $user"]
    }
}

if {0} {
    set woof_install /home/colin/Desktop/packages/woof/trunk/
    set woof_root /tmp/woof

    if {![file exists $woof_root]} {
	file mkdir $woof_root

	# Create an initial config if required
	file mkdir [file join $woof_root config]
	if {![file exists [file join $woof_root config _woof.cfg]]} {
	    file copy [file join $woof_install config _woof.cfg] [file join $woof_root config _woof.cfg]
	    file copy [file join $woof_install config application.cfg] [file join $woof_root config application.cfg]
	}
	
	if {![file exists [file join $woof_root app]]} {
	    file copy [file join $woof_install app] [file join $woof_root app]
	}
	
	if {![file exists [file join $woof_root public]]} {
	    file mkdir [file join $woof_root public]
	    
	    if {![file exists [file join $woof_root public images]]} {
		file copy [file join $woof_install public images] [file join $woof_root public images]
	    }
	    
	    if {![file exists [file join $woof_root public stylesheets]]} {
		file copy [file join $woof_install public stylesheets] [file join $woof_root public stylesheets]
	    }
	}
    }
    
    package require Woof
    Debug off woof 10
    woofLoad [file normalize ~/Desktop/packages/woof/trunk/lib/woof/] /tmp/woof /woof
    # construct a nub for Woof
    Nub domain /woof/ Woof root /tmp/woof/
}

if {0} {
    # try Scout stuff under Dub
    Nub domain /scouts/ Dub file ../../Scouts/members.mk
}

if {0} {
    # experiment: clients don't evaluate the JS.
    Nub code /arrived {
	<p> "Arrived!"
    }

    Nub code /redir_content {
	set r [Http Redirect $r /arrived]
	<script> alert("yes!")\;
    }
}

lappend ::auto_path ../../Scouts/
package require Family 
Nub domain /family/ {Family ::F}

#package require ReCAPTCHA
Nub domain /rc/ ReCAPTCHA public 6LeAYQcAAAAAAP_Wup90AGlLsUF8pHyMFJso30vg private 6LeAYQcAAAAAAHruGNruXBMsJoOr55u3JjTNt_j6

Nub code /recap/ {
    puts stderr "/recap: [info class instances ::ReCAPTCHA]"
    set r [jQ form $r .autoform target '#result']
    set r [Http NoCache $r]
    <div> [subst {
	[[lindex [info class instances ::ReCAPTCHA] 0] form class autoform]
	[<div> id result {}]
    }]
}

Nub code /recap1/ {
    #set r [jQ form $r .autoform target '#result']
    set r [Http NoCache $r]

    # everything from here is content:
    <div> [subst {
	[[lindex [info class instances ::ReCAPTCHA] 0] form class autoform before [<text> field default] after [<submit> ok] pass {
	    set r [Http Ok $r "Passed ReCAPTCHA ($args)" text/plain]
	}]
	[<div> id result {}]
    }]
}

Nub code /dump/* {
    set meta ""; set fields ""
    set r [Http NoCache $r]
    puts stderr "DUMP: [dict get $r -header]"
    dict for {n v} $r {
	if {[string match -* $n]} {
	    append meta [<tr> "[<td> $n] [<td> [armour $v]]"] \n
	} else {
	    append fields [<tr> "[<td> $n] [<td> [armour $v]]"] \n
	}
    }
    append table [<tr> [<th> "Request"]] \n $fields \n [<tr> [<th> "Metadata"]] \n $meta
    <table> $table
}

Nub code /formtest/ {
    Debug on chan 10
    Debug on connections 10
    Debug on Httpd 10
    Debug on HttpdLow 10

    <form> action . enctype multipart/form-data method post [subst {
	[<file> moop {}]
	[<submit> submit Ok]
    }]
}

Nub code /dltest {
    dict set r content-disposition attachment
    <p> "this is a text file"
}

Nub domain /tiny/ {Tiny ::tiny} file tiny.mk

Nub code /tinyreftest {
    set r [::tiny genref $r]
    <div> id genref {}
}

Nub literal /mbcss {
    div {
	border: 1px solid;
	padding: 10px;
    }
    div.no {
	background: grey;
    }
    div.ne {
	background: orange;
    }
    div.n {
	background: brown;
    }
    div.o {
	background: green;
    }
    div.e {
	background: orange;
    }

    div.c {
	background: lightgreen;
    }
    div.content {
	background: cyan;
    }
    div.so { 
	background: green;
    }
    div.se {
	background: orange;
    }
    div.s {
	background: purple;
    }
    div.bl {
	background:blue;
    }
} text/css

Nub code /mbtest {
    dict set r -style /mbcss {}
    <div> class no no[subst {
	[<div> class ne ne[<img>][<div> class n style {float:left} "TITLE .n:first"][<div> style {float:right} buttonbar]]
	[<div> class o o[<div> class e e[<div> class c c[<div> class content "CONTENT .c:first"]]]]
	[<div> class bl bl[<div> class so so[<div> class se se[<div> class s {s.first}]]]]
    }]
} x-text/html-fragment

Nub literal /mbpagecss {
    body, p, ol, ul, td, th, input, textarea {font-family: verdana, arial, helvetica, sans-serif; font-weight: normal}
} text/css

Nub code /mbpage {
    dict set r -style /mbpagecss {}
    set r [jQ containers $r .containerPlus]
    set header [<img> height 16px src /icons/downb.gif]
    set header [<span> class mbheader $header]
    set r [jQ postscript $r {
	$("span.mbheader").hide("fast");

	$("div.n").bind("mouseenter",function(){
	    $("span.mbheader",this).fadeIn("fast");
	}).bind("mouseleave",function(){
	    $("span.mbheader",this).fadeOut("fast");
	});
	$("div.ne").bind("mouseenter",function(){
	    $("div.buttonBar",this).fadeIn("fast");
	}).bind("mouseleave",function(){
	    $("div.buttonBar",this).fadeOut("fast");
	});
    }]

    <div> class top style {width:80%} [jQ container title Title1 skin 'default' header $header draggable 0 resizable 1 buttons 'p,c' [subst {
	[<p> "some text at the top"]
	[jQ container title Nested buttons 'm,i' width '80%' skin 'default' header $header [<p> "Nested Content A"]]
	[jQ container title Nested buttons 'm,i' header $header skin 'default' [<p> "Nested Content B"]]
	[jQ container title Title2 draggable 0 buttons 'm,i,c' header $header [jQ container title Nested buttons 'm,i,c' header $header "Nested Content2"]]
	[<p> "some text at the bottom"]
    }]]
}

Nub code /mbpage1 {
    set r [jQ containers $r .containerPlus]
    set header [<img> height 16px src /icons/downb.gif]
    <div> class top style {width:80%} [jQ container title Title1 header $header draggable 0 resizable 1 buttons 'm,c' content '/mbpage2']
}

Nub code /mbpage2 {
    set r [jQ containers $r .containerPlus]
    subst {
	[<p> "some text at the top"]
	[jQ container title Nested buttons 'm,i' width '80%' [<p> "Nested Content A"][<br>]]
	[jQ container title Nested buttons 'm,i' [<p> "Nested Content B"][<br>]]
	[jQ container title Title2 draggable 0 buttons 'm,c,i' [jQ container title Nested buttons 'm,c,i' "Nested Content2"][<br>]]
	[<p> "some text at the bottom"]
    }
}

Nub code /object {
    # html { overflow: auto; } to remove scrollbar
    # html { border: none; } removed border
    subst {
	[<p> "this is an object"]
	[<object> style {width:80%;} data http://localhost:8080/wub/ {}]
	[<p> "end of object"]
    }
}

Nub domain /simplicio/ Simplicio

lappend ::auto_path ../../WebServices/
package require WS::Server

##
## Define the service
##
::WS::Server::Service -mode wub \
    -htmlhead {Wub Based Web Services} \
    -service wsEchoExample \
    -description  {Echo Example - Tcl Web Services}

##
## Define any special types
##
::WS::Utils::ServiceTypeDef Server wsEchoExample echoReply {
    echoBack     {type string}
    echoTS       {type dateTime}
}

##
## Define the operations available
##
::WS::Server::ServiceProc \
    wsEchoExample \
    {SimpleEcho {type string comment {Requested Echo}}} \
    {
        TestString      {type string comment {The text to echo back}}
    } \
    {Echo a string back} {

    return [list SimpleEchoResult $TestString]
}

::WS::Server::ServiceProc \
    wsEchoExample \
    {ComplexEcho {type echoReply comment {Requested Echo -- text and timestamp}}} \
    {
        TestString      {type string comment {The text to echo back}}
    } \
    {Echo a string and a timestamp back} {

    set timeStamp [clock format [clock seconds] -format {%Y-%m-%dT%H:%M:%SZ} -gmt yes]
    return [list ComplexEchoResult [list echoBack $TestString echoTS $timeStamp]  ]
}

package require WS::Wub
Nub domain /service/wsEchoExample Wsdl -service wsEchoExample

package require jQ
set ::jQ::min 0

lappend ::auto_path ../../Card/
package require Card
Debug on card
Nub domain /card/ {Card ::card} icons /icons/


# test Sql domain
set sqltestfile testsql.db
if {![file exists $sqltestfile]} {
    package require sqlite3
    sqlite3 db_test $sqltestfile
    db_test eval {CREATE TABLE sources(name text, size int, modify text, fgroup text, owner text, permissions int)}
    foreach fn [glob *.tcl] {
	foreach {n v} [file attributes $fn] {
	    set [string trim $n -] $v
	}
	set name [file tail [file normalize $fn]]
	set size [file size $fn]
	set modify [file mtime $fn]
	db_test eval {INSERT INTO sources VALUES($name, $size, $modify, $group, $owner, $permissions)} 
    }
    db_test close
}

package require Sql
Debug on Sql 10
Nub domain /sql/ Sql file $sqltestfile

#Nub domain CGI /blerk

#Debug heartbeat

return Ok
