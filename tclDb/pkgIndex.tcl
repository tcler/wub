package ifneeded Db 1.0 [list source [file join $dir Db.tcl]]
package ifneeded db_tie 1.0 [list source [file join $dir db_tie.tcl]]
package ifneeded Mkvfs 2.0 [list source [file join $dir Mkvfs.tcl]]
package ifneeded snitvfs 1.0 [list source [file join $dir snitvfs.tcl]]
package ifneeded MkDomain 1.0 [list source [file join $dir MkDomain.tcl]]

package ifneeded mkdict_tie 1.0 [list source [file join $dir mkdict_tie.tcl]]
package ifneeded mkrecord 1.0 [list source [file join $dir mkrecord.tcl]]
package ifneeded nulluser 1.0 [list source [file join $dir nulluser.tcl]]
