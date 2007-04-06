set text A1B2C3D
#exec convert -size 200x120 xc:lightblue -font Bookman-DemiItalic -pointsize 32 -fill blue -draw [list text 10,20 '$text'] -fill yellow -draw [list path 'M 5,5 L 140,5 M 5,10 L 140,10 M 5,15 L 140,15'] -trim -wave 6x70 -swirl 30 captcha.jpg
set h 240
set w 300
set incrs 5
exec convert -background lightblue -fill blue -font Bookman-DemiItalic -size 300x240 -gravity center label:Anthony -trim -fill yellow -draw [list path 'M $incrs,5 L 140,5 M 5,10 L 140,10 M 5,15 L 140,15'] captcha.jpg