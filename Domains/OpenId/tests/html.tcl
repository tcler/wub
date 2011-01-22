lappend ::auto_path /usr/lib/tcltk/
package require tdom

set html {
    <html>
    <head>
    <meta http-equiv="X-XRDS-Location" content="http://example.com/yadis.xml">
    </head>
    </html>
}

set doc  [dom parse -html $html]
set root [$doc documentElement]
set node [$root selectNodes {//meta[@http-equiv="X-XRDS-Location"]}]
puts stderr "$node [$node nodeName] [$node getAttribute content]"
