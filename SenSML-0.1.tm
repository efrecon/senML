package require Tcl 8.6

namespace eval ::sensml {
  namespace eval vars {
    variable id 0;        # Identifier generator
    variable -log stderr; # Log stream, empty to switch off
    variable version [lindex [split [file rootname [file tail [info script]]] -] end]
  }

  # defaults for all new streams
  namespace eval sensml {
    variable -forbidden "*_"
    variable -callback  ""
  }

  namespace export {[a-z]*}
  # Create an alias for new as the name of the current namespace, this is the
  # only command that is really exposed.
  interp alias {} [namespace current] {} [namespace current]::new
}


proc ::sensml::new { args } {
  # Create new stream object
  set s [namespace current]::[incr vars::id]
  upvar \#0 $s S

  # Capture arguments and give good defaults into the stread object (a
  # dictionary). Create the command
  defaults S sensml {*}$args
  interp alias {} $s {} [namespace current]::Dispatch $s
}


# ::sensml::getopt -- Get options
#
#       From http://wiki.tcl.tk/17342
#
# Arguments:
#	_argv	"pointer" to incoming arguments
#	name	Name of option to extract
#	_var	Pointer to variable to set
#	default	Default value
#
# Results:
#       1 if the option was found, 0 otherwise
#
# Side Effects:
#       None.
proc ::sensml::getopt {_argv name {_var ""} {default ""} } {
  upvar 1 $_argv argv $_var var
  set pos [lsearch -regexp $argv ^$name]
  if {$pos>=0} {
    set to $pos
    if {$_var ne ""} {
      set var [lindex $argv [incr to]]
    }
    set argv [lreplace $argv $pos $to]
    return 1
  } else {
    if {[llength [info level 0]] == 5} {set var $default}
    return 0
  }
}


# ::sensml::defaults -- Init and option parsing based on namespace.
#
#       This procedure takes the dashled variables of a given (sub)namespace to
#       initialise a dictionary. These variables are considered as being the
#       canonical set of options for a command or object and contain good
#       defaults, and the procedure will capture these from the arguments.
#
# Arguments:
#	cx_	"Pointer" to dictionary to initialise and parse options in.
#	ns	Namespace (FQ or relative to caller) where to get options from
#	args	List of dashled options and arguments, must match content of namespace
#
# Results:
#       Return the list of options that were taken from the arguments, an error
#       when an option that does not exist in the namespace as a variable was
#       found in the arguments.
#
# Side Effects:
#       None.
proc ::sensml::defaults { cx_ ns args } {
  upvar $cx_ CX

  set parsed [list]
  foreach v [uplevel info vars [string trimright $ns :]::-*] {
    set opt [lindex [split $v :] end]
    if { [getopt args $opt value [set $v]] } {
      lappend parsed $opt
    }
    dict set CX $opt $value
  }

  return $parsed
}


# ::sensml::isolate -- Isolate options from arguments
#
#       Isolate dash-led options from the rest of the arguments. This procedure
#       prefers the double-dash as a marker between the options and the
#       arguments, but it is also able to traverse until the end of the options
#       and the beginning of the arguments. Traversal requires that no value of
#       an option starts with a dash to work properly.
#
# Arguments:
#	args_	Pointer to list of arguments (will be modified!)
#	opts_	Pointer to list of options
#
# Results:
#       None.
#
# Side Effects:
#       Modifies the args and opts lists that are passed as parameters to
#       reflect the arguments and the options.
proc ::sensml::isolate { args_ opts_ } {
  upvar $args_ args $opts_ opts
  set idx [lsearch $args "--"]
  if { $idx >= 0 } {
    set opts [lrange $args 0 [expr {$idx-1}]]
    set args [lrange $args [expr {$idx+1}] end]
  } else {
    set opts [list]
    for {set i 0} {$i <[llength $args] } { incr i 2} {
      set opt [lindex $args $i]
      set val [lindex $args [expr {$i+1}]]
      if { [string index $opt 0] eq "-" } {
        if { [string index $val 0] eq "-" } {
          incr i -1; # Consider next not next-next!
          lappend opts $opt
        } else {
          lappend opts $opt $val
        }
      } else {
        break
      }
    }
    set args [lrange $args $i end]
  }
}

proc ::sensml::Dispatch { s cmd args } {
  # Try finding the command as one of our internally implemented procedures.
  if { [string tolower $cmd] eq $cmd } {
    if { [llength [info commands [namespace current]::$cmd]] } {
      tailcall [namespace current]::$cmd $s {*}$args
    }
  }
  return -code error "$cmd is not a known operation"
}


proc ::sensml::Log { d lvl msg } {
  if { ${vars::-log} ne "" } {
    set lvl [string tolower $lvl]
    puts ${vars::-log} "\[$lvl\] $msg"
  }
}

package provide sensml $::sensml::vars::version