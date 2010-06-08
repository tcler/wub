package provide Indent 1.0

namespace eval ::Indent {

	namespace ensemble create
	namespace export code

	# rules
	variable _comment		{^[ \t]*\#}

	
	variable buffer ""
	variable dmsg "" 
	variable ln
	variable -d

	# -- DebugMsg
	#
	proc DebugMsg { str } {
		variable dmsg
		variable -d
		if { ${-d} == 1 } {
			append dmsg "------${str}\n"
		}
	}

	# -- AppendBuffer
	#
	proc AppendBuffer { str } {
		variable buffer
		variable dmsg
		variable ln
		variable -d
		if { ${dmsg} ne "" } {
			append buffer ${dmsg}
			set dmsg ""
		}
		if { ${-d} == 1 } {
			append buffer "([format "%03s" ${ln}]) [string trimright ${str} "\ \t"]"
		} else {
			append buffer [string trimright ${str} "\ \t"]
		}
	}

	# -- GetBracetIndent
	#
	proc GetBracetIndent { str indent_value} {
		upvar ${indent_value} indent
		
		set lbraces [CountChars ${str} \{]
		set rbraces [CountChars ${str} \}]
		set net-braces [expr ${lbraces}-${rbraces}]
		set undent [string equal [string index ${str} 0] \}]
		set str_indent ${indent}
		if { ${net-braces} > 0 } {
			# increase undent for next str
			incr indent ${net-braces}
		} elseif { ${net-braces} < 0 } {
			# reduce indent starting with current str
			incr indent ${net-braces}
			set str_indent ${indent}
		} elseif { ${undent} } {
			set str_indent [expr ${indent}-1]
		}
		DebugMsg "lb: ${lbraces} rb: ${rbraces} nb: ${net-braces} u: ${undent} indent: $indent"
		return ${str_indent}
	}

	# -- code
	#
	# Reformat the text found in tclcode so it has a consistent
	# look and indentation.
	#
	# OPTIONS:
	# -d		debug: 1=on,0=off (default=0)
	# -o		output file name (default="")
	# -inset	initial indent level (default=0)
	# -ts		tab spacing (default=4)
	#
	proc code { tclcode args } {
		variable _comment
		variable buffer
		variable ln
		variable -d
		
		# get option setting
		set options {
			-d 0
			-o {}
			-inset 0
			-ts 4
		}
		foreach {opt val} ${options} {
			if { ${opt} in ${args} } {
				set val [lindex ${args} [lsearch ${args} ${opt}]+1]
			}
			set ${opt} ${val}
		}

		# split code on newlines
		set lines [split ${tclcode} \n]
		set maxln [llength ${lines}]

		set -ts [string repeat " " ${-ts}]
		for {set i 0} {${i}<20} {incr i} {
			lappend spaces [string repeat ${-ts} ${i}]
		}
		
		set buffer ""		;# output buffer
		set oddquotes 0		;# string quote flag (0 = balanced, 1 = not balanced)
		set quoted 0

		set indent ${-inset}
		set current_indent ${indent}

		for {set ln 0} {${ln}<${maxln}} {incr ln} {
			set raw [lindex ${lines} ${ln}]
			set line [string trim ${raw} "\ \t"]

			# blank line
			if { ${line} eq "" } {
				AppendBuffer \n
				continue
			}

			# full line comment
			if { [regexp ${_comment} ${line}] } {
				DebugMsg "# ...comment line"
				AppendBuffer [lindex ${spaces} ${indent}]${line}\n
				continue
			}
			
			# handle quote string
			set oddquotes [expr {[CountChars $line \"] % 2}]
			if { ${quoted} == 0 && ${oddquotes} == 1 } {
				# indent first line of quote
				set quoted 1
				DebugMsg "# ...quote start"
				# ajust indent based on text before first quote
				set current_indent ${indent}
				set str [string trimleft ${raw} "\ \t"]
				set i [string first \" ${str}]
				if { ${i} > 0 } {
					set current_indent [GetBracetIndent [string range ${str} 0 ${i}] indent]
				}
				AppendBuffer [lindex ${spaces} ${current_indent}]${str}\n
				continue
			} elseif { ${quoted} == 1 && ${oddquotes} == 1 } {
				set quote_start 0
				set quoted 0
				DebugMsg "# ...quote end"
				AppendBuffer ${raw}\n
				continue
			} elseif { ${quoted} == 1 } {
				set quote_start 0
				DebugMsg "# ...quote"
				AppendBuffer ${raw}\n
				continue
			}

			# quotes are balances so ajust the line indent
			set current_indent [GetBracetIndent ${line} indent]
			
			if { ${indent} < 0 } {
				DebugMsg "ERROR: unbalanced left braces - indent=${indent}"
				AppendBuffer ${raw}\n
				if { ${-o} ne "" } {
					set fid [open ${-o} w]
					puts ${fid} ${buffer}
					close ${fid}
				}
				
				error "${buffer}\nunbalanced left braces"
			}
			
			AppendBuffer [lindex ${spaces} ${current_indent}]${line}\n
		}

		# check to see if we returned to our initial indent level
		if { ${indent} > ${-inset} } {
			DebugMsg "ERROR: unbalanced right braces - indent=${indent} -inset=${-inset}"
			AppendBuffer \n
			if { ${-o} ne "" } {
				set fid [open ${-o} w]
				puts ${fid} ${buffer}
				close ${fid}
			}
			error "unbalanced right braces"
		}

		# write result to file if requested
		if { ${-o} ne "" } {
			set fid [open ${-o} w]
			puts ${fid} [string trimright ${buffer} "\n"]
			close ${fid}
		}
		
		return [string trimright ${buffer} "\n"]
	}

	# --
	#
	# Return the number of 'char' characters in string 'str'.
	#
	proc CountChars {str char} {
		set count 0
		while { [set idx [string first ${char} ${str}]] >= 0 } {
			set backslashes 0
			set nidx ${idx}
			while { [string equal [string index ${str} [incr nidx -1]] \\] } {
				incr backslashes
			}
			if {${backslashes} % 2 == 0} {
				incr count
			}
			set str [string range ${str} [incr idx] end]
		}
		return ${count}
	}

}
