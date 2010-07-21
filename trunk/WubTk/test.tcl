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
    .g insert end $lambda
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

image create photo .image -file ../Domains/Icons/images/chart_accept.gif
grid configure .image -column 0 -row 9

label .limage -image .image
grid configure .limage -column 1 -row 9

button .ibutton -image .image -command buttonC -text "image button" -valign middle -compound bottom
grid configure .ibutton -column 2 -row 9

frame .frame -text "A Frame"
image create photo .image2 -file ../Domains/Icons/images/clock.gif
button .frame.button -image .image2 -command buttonC -text "press me" -compound top
grid configure .frame.button -column 0 -row 0

grid configure .frame -column 3 -row 0 -rowspan 3

if {1} {
    # make a notebook
    notebook .nb
    grid configure .nb -column 0 -row 10 -columnspan 5
    
    frame .nb.tab1 -text "Tab 0"
    .nb add .nb.tab1
    label .nb.tab1.limage -text "This is Tab 0"
    grid configure .nb.tab1.limage -column 0 -row 0
    
    frame .nb.tab2 -text "Tab 1"
    .nb add .nb.tab2
    label .nb.tab2.limage -text "This is Tab 1"
    grid configure .nb.tab2.limage -column 0 -row 0
    
    frame .nb.tab3 -text "Tab 2"
    .nb add .nb.tab3
    label .nb.tab3.limage -text "This is Tab 2"
    grid configure .nb.tab3.limage -column 0 -row 0
}

if {1} {
    # make an accordion
    accordion .acc
    grid configure .acc -column 0 -row 11 -columnspan 5
    
    frame .acc.acc1 -text "Accordion 0"
    .acc add .acc.acc1
    label .acc.acc1.limage -text "This is Accordion 0"
    grid configure .acc.acc1.limage -column 0 -row 0
    
    frame .acc.acc2 -text "Accordion 1"
    .acc add .acc.acc2
    label .acc.acc2.limage -text "This is Accordion 1"
    grid configure .acc.acc2.limage -column 0 -row 0
    
    frame .acc.acc3 -text "Accordion 2"
    .acc add .acc.acc3
    label .acc.acc3.limage -text "This is Accordion 2"
    grid configure .acc.acc3.limage -column 0 -row 0
}

frame .test -text "Test multivar update" -grid {12 0}
label .test.l1 -textvariable tl1 -text [incr tl1] -grid {0 0}
button .test.b1 -text "Test Change" -command {after 10000 {global tl1; incr tl1}} -grid {0 1}

toplevel .tl -title "A New Toplevel"
label .tl.l1 -textvariable tl1 -text [incr tl1] -grid {0 0}
button .tl.b1 -text "Test Change" -command {after 10000 {global tl1; incr tl1}} -grid {0 1}

