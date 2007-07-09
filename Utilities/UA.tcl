# UA.tcl - user agent detection.
# http://en.wikipedia.org/wiki/User_agent

package provide UA.tcl 1.0

# process the ua string into something enabling us to detect MSIE
proc ua {ua} {
    set rest [string trim [join [lassign [split $ua] mozver]]]
    if {![string match Mozilla/* $mozver]} {
	# this is not a standard kind of ua
	return [list id unknown ua $ua]
    }

    set result [dict create ua $ua]

    if {[regexp {([^(])*[(]([^)]*)[)](.*)} -> pre par addition]} {
	foreach v {pre par addition mozver} {
	    set $v [string trim [set $v]]
	}
	set addition [split $addition]
	lassign [split $mozver /] mver
	dict set result mozilla_version $mver
	set par [split $par {;}]

	if {[lindex $par 0] eq "compatible"} {
	    dict set result id MSIE
	    set fields {version provider platform}
	    dict set result extension [lassign $par -> {*}$fields]
	    set version [lindex [split $version] 1]
	    foreach f $fields {
		dict set result $f [set $f]
	    }
	} elseif {[string match Gecko/* [lindex $addition 0]]} {
	    dict set result id FF
	    set fields {platform security subplatform language version}
	    dict set extensions [lassign $par {*}$fields]
	    set version [lindex [split $version :] end]
	    foreach f $fields {
		dict set result $f [set $f]
	    }
	    dict set result rest [lassign [split $addition] gecko product]
	    foreach p {gecko product}
		dict set result product {*}[split $p /]
	    }
	} elseif {$addition eq ""} {
	    dict set result id NS
	    dict set result rest [lassign [split $pre] language provider]
	    set fields {platform security subplatform}
	    dict lappend result rest [lassign $par {*}$fields]
	    lappend fields language provider
	    foreach f $fields {
		dict set result $f [set $f]
	    }
	    
	}
    }
    return $result
}

Internet Explorer, and browsers cloaking as Internet Explorer

IE: Mozilla/MozVer (compatible; MSIE IEVer[; Provider]; Platform[; Extension]*) [Addition]
NS: Mozilla/Version[Gold] [[Language]][Provider] (Platform; Security[; SubPlatform][StandAlone])
FF: Mozilla/MozVer (Platform; Security; SubPlatform; Language; rv:Revision[; Extension]*) Gecko/GeckVer [Product/ProdVer]

Where:

    * MozVer: Netscape compatibility version
          o 1.22: Internet Explorer 1.5 and 2.0
          o 2.0: Internet Explorer 3.x for Windows and Internet Explorer 2.1 for Mac
          o 3.0: Internet Explorer 3.x for Mac
          o 4.0: Internet Explorer 4.x and higher
    * IEVer: Internet Explorer version number, e.g.: 1.5, 3.01, 5.0b1, 6.0
    * Provider: Access provider, e.g.:
          o AOL Version
          o America Online Browser Version; revRevision: AOL Explorer (note, this breaks the rule of no semicolons in Provider values)
          o CS 2000
          o MSN Version
          o This parameter is included but empty on some Opera distributions, resulting in "semicolon space semicolon":

; ;

    * Platform: Operating system, e.g.:
          o Windows 3.1 (including Windows NT 3.x)
          o Windows 95
          o Windows 98
          o Windows 98; Win 9x 4.90: Windows Millennium Edition (Windows Me)
          o Windows NT
          o Windows NT 4.0
          o Windows NT 5.0: Windows 2000
          o Windows 2000: Windows 2000 (used by Opera)
          o Windows NT 5.01: Windows 2000, Service Pack 1 (SP1)
          o Windows NT 5.1: Windows XP
          o Windows NT 5.2: Windows Server 2003
          o Windows NT 6.0: Windows Vista
          o Windows CE: Windows CE and Windows Mobile (used by Internet Explorer Mobile)
          o Win32: Seen with IE 7b1 on Windows XP
          o Mac_68000
          o Mac_PPC: Used up until IE 4.x
          o Mac_PowerPC: Used from IE 5.x and up
          o SunOS Version: SunOS
          o Symbian OS: Used by Opera on mobile phones
          o Nitro: Nintendo DS (used by Opera)
    * Extension: optional, a list of semicolon-separated extensions installed, e.g.:
          o .NET CLR Version: .NET Framework common language runtime installed (may appear multiple times, e.g. when both 1.1 and 2.0 are supported)
          o SV1: Internet Explorer 6 in Windows XP SP2 and Windows Server 2003 SP1 installed
          o Tablet PC Version: Tablet services are installed
          o Win64: 64-bit Windows
          o IA64: Intel Itanium processor
          o AMD64: x64 processor
          o x64: x64 processor
          o WOW64: 32-bit Internet Explorer is running on 64-bit Windows
          o Media Center PC Version: Windows MCE, where Version is:
                + 2.8: Media Center 2004
                + 3.0: Media Center 2005
                + 3.1: Media Center 2005 with update rollup 1
                + 4.0: Media Center 2005 with update rollup 2
                + 5.0: Windows Vista Home Premium or Ultimate edition.
          o MediaCenter Version: browsing from within Media Center interface
          o MSIECrawler: MSIE retrieving pages for Offline Content feature
          o X11: Internet Explorer on SunOS
          o PPC: Pocket PC (used by Internet Explorer Mobile)
          o Smartphone: Smartphone (used by Internet Explorer Mobile)
          o Motorola Type: Internet Explorer Mobile on Motorola mobile phone
          o Nokia Type/Version: Opera on Nokia mobile phone
          o Various 3rd party browser extensions, like: (R1 Version) (RealPlayer webbrowser), Alexa Toolbar, Maxthon, Crazy Browser Version, MyIE2, Avant Browser [avantbrowser.com], FDM
          o Language: some Opera versions seem to include the language code here (seen for Opera 8.5 on Windows XP)
          o WidthxHeight: some Internet Explorer Mobile browsers include the display resolution here
    * Addition
          o Netscape/Version: Netscape using MSHTML rendering engine
          o Opera Version [Language]: Opera cloaked as Internet Explorer

Example:

Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; .NET CLR 1.1.4322; .NET CLR 2.0.50727)

[edit] Internet Explorer 1.0

Internet Explorer 1.0 used an alternative user agent:

Microsoft Internet Explorer/Version (Platform)

Where:

    * Version: Windows version because IE was originally supposed to be included with Windows 95, e.g.: 4.0b1 for Internet Explorer 1.0 beta
    * Platform: operating system, e.g.: Windows 95

[edit] Netscape

This only applies to earlier versions of Netscape. Netscape 6.0 or higher based on the Gecko engine should see the Mozilla section below

Mozilla/Version[Gold] [[Language]][Provider] (Platform; Security[; SubPlatform][StandAlone])

Where:

    * Version: version number
    * Gold: includes HTML editor
    * Language: standardized two-letter language identifier, e.g.: en, fr, es (Netscape 2.x and 3.x: only for non-English versions, Netscape 4.x: not on Macintosh platforms?)
    * Provider, may contain variants of C-CCK-MCD (Client Customization Kit and Mission Control Desktop, for ISPs and OEMs)
    * Platform
          o Win16: Windows 3.x
          o Win95: Windows 95, where SubPlatform can be:
                + 16bit for 16-bit version of Netscape
          o Win98: Windows 98
          o WinNT: Windows NT 3.x
          o Windows NT 5.0: Windows 2000
          o Windows NT 5.1: Windows XP
          o X11, where SubPlatform is:
                + Distribution Version [Subversion], e.g.: Linux i586, SunOS 5.6 sun4u, IRIX 6.5 IP32
          o Macintosh, where SubPlatform is:
                + 68K
                + PPC
                + Intel
    * Security
          o U: strong security (USA)
          o I: weak security (International)
          o N: no security
    * StandAlone: standalone Navigator is indicated by ; Nav (X11 platforms), ;Nav (Windows, note missing space) or , Nav (on Macintosh), only for version 4.x

[edit] Mozilla

Mozilla/MozVer (Platform; Security; SubPlatform; Language; rv:Revision[; Extension]*) Gecko/GeckVer [Product/ProdVer]

Where:

    * MozVer: Netscape compatibility version
          o 5.0 for all known Mozilla browsers
    * Platform and SubPlatform
          o Windows
                + Win3.11: Windows 3.11
                + Win95: Windows 95
                + Win98: Windows 98
                + Win 9x 4.90: Windows Me
                + WinNT3.51: Windows NT 3.51
                + WinNT4.0: Windows NT 4.0
                + Windows NT 5.0: Windows 2000
                + Windows NT 5.1: Windows XP (except XP Professional x64 Edition)
                + Windows NT 5.2: Windows Server 2003 and Windows XP Professional x64 Edition
                + Windows NT 6.0: Windows Vista
                + Windows CE 4.21: Windows Mobile 2003
          o Macintosh
                + PPC Mac OS X
                + PPC Mac OS X Mach-O
                + Intel Mac OS X
          o X11
                + Linux [Version] Hardware
                + FreeBSD Hardware
                + NetBSD Hardware
                + SunOS Hardware
    * Security
          o U: strong security (USA)
          o I: weak security (International)
          o N: no security
    * Language: standardized language identifier, e.g.: en, en-US, en-GB
    * Revision: e.g. m18, 1.0rc3, 1.7.8, 1.8a2
    * Extension
          o MultiZilla Version
    * GeckVer: Gecko (layout engine) compilation date, in the format YYYYMMDD
    * Product
          o For the Mozilla Application Suite, there is no Product or ProdVer. The Revision is the product version.
          o Netscape, Netscape6
          o Phoenix, Firebird, Firefox, GranParadiso (Firefox 3 codename), Minefield (trunk build)
          o K-Meleon
          o Minimo
          o SeaMonkey
          o Camino
          o CS 2000 7.0
    * ProdVer: Product version

[edit] Encryption strength "U" / "I" / "N"