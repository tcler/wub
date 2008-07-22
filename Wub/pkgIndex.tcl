package ifneeded Listener 2.0 [list source [file join [list $dir] Listener.tcl]]

package ifneeded Httpd 3.0 [list source [file join [list $dir] Httpd.tcl]]
package ifneeded HttpdThread 1.0 [list source [file join [list $dir] HttpdThread.tcl]]
package ifneeded HttpdSingle 1.0 [list source [file join [list $dir] HttpdSingle.tcl]]

package ifneeded Cache 2.0 [list source [file join [list $dir] Cache.tcl]]
package ifneeded Varnish 1.0 [list source [file join [list $dir] Varnish.tcl]]
package ifneeded Access 1.0 [list source [file join [list $dir] Access.tcl]]
package ifneeded Activity 2.0 [list source [file join [list $dir] Activity.tcl]]
package ifneeded Block 2.0 [list source [file join [list $dir] Block.tcl]]

package ifneeded Backend 1.0 [list source [file join [list $dir] Backend.tcl]]
package ifneeded Site 1.0 [list source [file join [list $dir] Site.tcl]]