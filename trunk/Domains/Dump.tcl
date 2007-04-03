package provide Dump 1.0

proc DumpDispatch {req} {
    puts stderr "DumpDispatch: $req"

    set http [dict get $req -http]
    {*}$http Restart $req	;# assume it's not a POST

    # clean out some values
    dict unset req cache-control

    set c {
	<html>
	<head>
	<title>Test Page</title>
	</head>
	<body>
	<h1>Test Content</h1>
    }

    append c "<table border='1' width='80%'>" \n
    append c <tr> <th> metadata </th> </tr> \n
    dict for {n v} $req {
	if {[string match -* $n]} {
	    append c <tr> <td> $n </td> <td> $v </td> </tr> \n
	}
    }
    append c </table> \n

    append c "<table border='1' width='80%'>" \n
    append c <tr> <th> "HTTP field" </th> </tr> \n
    dict for {n v} $req {
	if {![string match -* $n]} {
	    append c <tr> <td> $n </td> <td> $v </td> </tr> \n
	}
    }
    append c </table> \n

    append c "<table border='1' width='80%'>" \n
    append c <tr> <th> "Query field" </th> </tr> \n
    dict for {n v} [Query flatten [Query parse $req]] {
	append c <tr> <td> $n </td> <td> $v </td> </tr> \n
    }
    append c </table> \n

    append c {
	</body>
	</html>
    }

    do respond Ok [dict replace $req warning "199 Moop 'For fun'"] $c text/html
}
