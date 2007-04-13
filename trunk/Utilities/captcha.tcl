proc captcha {captcha} {
    if {![info exists ::captcha_lines]} {
	for {set i 7} {$i < 100} {incr i 7} {
	    lappend x "M 5,$i L 300,$i"
	}
	set ::captcha_lines [join $x]
    }
    exec convert -background lightblue -fill blue -font Bookman-DemiItalic -size 300x240 -gravity center label:$captcha -trim -fill yellow -draw [list path '$::captcha_lines'] -wave 10x70 -swirl 55 captcha.jpg
}
#exec convert -size 200x120 xc:lightblue -font Bookman-DemiItalic -pointsize 32 -fill blue -draw [list text 10,20 '$text'] -fill yellow -draw [list path 'M 5,5 L 140,5 M 5,10 L 140,10 M 5,15 L 140,15'] -trim -wave 6x70 -swirl 30 captcha.jpg

captcha Martin
