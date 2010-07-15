# Steve Redler's WubTk demo

package require Tk

proc buttonA args {
    set bText [.b cget -text]
    set aText [.a cget -text]
    set bBg [.b cget -background]
    set aBg [.a cget -background]
    set bFg [.b cget -foreground]
    set aFg [.a cget -foreground]
    .a configure -text $bText -foreground $bFg -background $bBg
    .b configure -text $aText -foreground $aFg -background $aBg
    .g insert 2.4 "n1234567890\nsonia123"
}

proc buttonC args {
    global joe
    set joe "${joe}A"
    if {$joe != "bob" && [string first Mod [wm title .]] == -1} {
	wm title . "[wm title .] - Text Modified"
    }
    set joe [.g get 2.4 3.8]
    .g delete 2.4 3.8
}

proc buttonH {sobj dobj} {
    set text [$sobj get 0.0 end]
    set output ""
    foreach word [split $text] {
	for {set i 0} {$i<[string length $word]} {incr i} {
	    set char [string index $word $i]
	    append output "$char[string tolower $char$char]"
	}
	append output " "
    }
    $dobj configure -text $output
    wm title . "Done it!"
}

proc showCode {} {
    .g delete 1.0 end
    global lambda
    set a 0
    foreach line [split $lambda \n] {
	.g insert end "$line \n"
	incr a
	#update
    }
    .i configure -text "Cut n paste the above code into wish to compare!" -foreground orange -background black
}
proc readckrd {} {
    global joe k rb ckbt
    if {$ckbt} {
	.m configure -background lightgreen
    } else {
	.m configure -background pink
    }
}
wm title . "Demo #1"
button .a -text "Press Me" -command buttonA -background lightgreen -foreground purple
button .b -text "Don't Press Me" -command buttonA -background pink -foreground blue

button .c -text "Modify" -command buttonC
entry .d -textvariable joe -width 10 -type date
label .e -text "Text:" -justify right

button .f -text Logout -command "exit http://www.google.com"

text .g -background lightyellow 
.g insert end "steve1234567890\ncolin1234567890\nsonia1234567890\nsybil1234567890"
label .i -text ""
#button .h -text "Do it" -command "buttonH .g .i" -background violet
button .h -text "Clear" -command {.g delete 1.0 end; .i configure -text ""} -background violet
button .j -text "Show Code" -command "showCode" -background lightblue
#checkbutton .k -command  {global ckbt ; if {$ckbt} {set ckbt 0} else {set ckbt 1}}
checkbutton .k -text "check me out" -variable ckbt
entry .o -textvariable ckbt -width 4 -justify right
#radiobutton .l -text "option 1" -variable rb -value 1
#radiobutton .n -text "option 2" -variable rb -value 2
set k ""
set rb ""
button .m -text Read -command readckrd
scale .p -text "slide me" -variable scl -from 0 -to 100
entry .q -textvariable scl -width 4 -justify right
set joe bob
set ckbt 0
set rdbt 0
set scl 0
grid configure .a -column 0 -row 0
grid configure .b -column 1 -row 0
grid configure .c -column 0 -row 2
grid configure .e -column 1 -row 2
grid configure .d -column 2 -row 2
grid configure .f -column 2 -row 0 -columnspan 1
grid configure .g -column 0 -row 4 -columnspan 3
grid configure .i -column 0 -row 5 -columnspan 3
grid configure .h -column 0 -row 6 -columnspan 1
grid configure .j -column 2 -row 6 -columnspan 1
grid configure .k -column 0 -row 7 -columnspan 1
grid configure .o -column 1 -row 7 -columnspan 1
#grid configure .l -column 1 -row 7 -columnspan 1
grid configure .m -column 2 -row 7 -columnspan 1
#grid configure .n -column 2 -row 7 -columnspan 1
grid configure .p -column 0 -row 8 -columnspan 2
grid configure .q -column 2 -row 8 -columnspan 1
