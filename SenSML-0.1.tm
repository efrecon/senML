package require Tcl 8.6
package require json

namespace eval ::sensml {
  namespace eval vars {
    variable id 0;        # Identifier generator
    # Known base fields and their default values.
    variable base {
      bn ""
      bt 0.0
      bu ""
      bv 0.0
      bs 0.0
      bver 10
    }
    # Known fields and the operation to perform together with the corresponding
    # base field when relevant. + will append (for strings) or add (for
    # numbers).
    variable fields {
      n +
      u ""
      v +
      vs ""
      vb ""
      vd ""
      s +
      t +
      ut ""
    }
    variable forbidden "*_";    # Forbidden fields.
    variable reltime 268435456; # Breakout point for relative->absolute time
    variable version [lindex [split [file rootname [file tail [info script]]] -] end]
    variable levels {ERROR WARN NOTICE INFO DEBUG TRACE}
  }

  # defaults for all new streams
  namespace eval sensml {
    variable -log stderr;       # Log stream, empty to switch off
    variable -level WARN;       # loglevel
    variable -callback  ""
    variable -version   10
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
  defaults S sensml
  configure $s {*}$args
  interp alias {} $s {} [namespace current]::Dispatch $s

  # Initialise the internal state of the stream
  dict set S remainder ""
  Init $s

  return $s
}

proc ::sensml::close { s } {
  upvar \#0 $s S

  callback $s CLOSE
  dict set S remainder ""
  Init $s
}


proc ::sensml::configure { s args } {
  upvar \#0 $s S

  foreach {opt val} $args {
    switch -- $opt {
      -level {
        if { [lsearch -nocase $vars::levels [dict get $S -level]] < 0 } {
          return -code error "$val is not a valid log level, should be [join $vars::levels , ]"
        }
      }
      -version {
        if { ! [string is integer -strict $val] } {
          return -code error "version $val should be an integer"
        }
      }
    }

    if { [dict exists $S $opt] } {
      dict set S $opt $val
    }
  }

}

proc ::sensml::stream { s json } {
  upvar \#0 $s S

  # Append remainder of previous part of stream, if any
  if { [dict get $S remainder] ne "" } {
    set json [dict get $S remainder]$json
  }

  set json [string trim $json]
  if { [string index $json 0] eq "\[" } {
    Init $s
    Callback $s OPEN
    set json [string trim [string range $json 1 end]]
  }

  set json [regsub -all -- {\}\s*,\s*\{} $json "\},\{"]
  set json [regsub -all -- {\}\s*\]} $json "\}\]"]
  set start 0
  while {$start<[string length $json]} {
    set open [string first "\{" $json $start]
    set nxt [string first "\},\{" $json $open]
    set end [string first "\}\]" $json $open]
    if { $open >= 0 } {
      if { $end >= 0 && $nxt < 0 } {
        Pack $s [string range $json $open $end]
        Callback $s CLOSE
        dict set S remainder ""
        break
      } elseif { $nxt >= 0 } {
        if { $end < $nxt && $end >= 0 } {
          Pack $s [string range $json $open $end]
          Callback $s CLOSE
          dict set S remainder ""
          break
        } else {
          Pack $s [string range $json $open $nxt]
          set start [expr {$nxt+1}]
        }
      } else {
        dict set S remainder [string range $json $open end]
      }
    }
  }
}

proc ::sensml::Init { s } {
  upvar \#0 $s S

  dict for {base init} $vars::base {
    if { [dict exists $S $base] } {
      dict unset S $base
    }
  }
}

proc ::sensml::Pack { s json } {
  upvar \#0 $s S

  # Parse the JSON Pack properly
  set d [::json::json2dict $json]
  Log $s TRACE "JSON Pack: $d"

  # Set and remember base fields that would be present in the pack.
  dict for {f v} $d {
    if { [string match "b*" $f] } {
      dict set S $f $v
    }
  }

  # We skip all packs that would have a version number larger than the one that
  # we implement. Period, no questions asked.
  if { [Base $s bver] <= [dict get $S -version] } {
    set pack [dict create];  # Resolved pack to be called back

    # First pass: arrange for all "pure" fields, i.e. non-base, to inherit the
    # value from the current base field.
    dict for {f v} $d {
      # Return an error on forbidden fields!
      if { [string match $vars::forbidden $f] } {
        return -code error "$f is a forbidden field name!"
      }

      if { ! [string match "b*" $f] } {
        set added 0
        foreach {known op} $vars::fields {
          if { $f eq $known && $op eq "+" } {
            if { [dict exists $vars::base "b$f"] } {
              set bv [Base $s "b$f"]
              if { [string is double -strict [dict get $vars::base "b$f"]] } {
                dict set pack $f [expr {$bv+$v}]
              } else {
                dict set pack $f "${bv}${v}"
              }
              set added 1
            }
          }
        }
        if { ! $added } {
          dict set pack $f $v
        }
      }
    }

    # Set base fields that are explicitely set for the stream, but not for this
    # pack.
    dict for {bf v } [dict filter $S key b*] {
      set f [string range $bf 1 end]
      if { $bf ne "bver" && ![dict exists $pack $f] } {
        dict set pack $f $v
      }
    }

    # Follow the RFC when it comes to sums and values. Something must be there!
    if { ! [dict exists $pack s] } {
      set nvkeys [llength [dict keys $pack "v*"]]
      if { $nvkeys > 1 } {
        return -code error "More than one value specified in $json"
      } elseif { $nvkeys == 0 } {
        # Default value when nothing is present.
        dict set pack v [dict get $vars::base bv]
      }
    }

    # Scream on wrong names, follow the RFC. Since we already have accounted for
    # basenames when here, we scream if nothing was found.
    if { [dict exists $pack n] } {
      if { ! [regexp -- {[a-zA-Z0-9][a-zA-Z0-9:./_-]*} [dict get $pack n]] } {
        return -code error "Wrong name [dict exists $pack n] in $json"
      }
    } else {
      return -code error "No name, nor basename provided at $json"
    }

    # Arrange for a time to always be present, defaulting to 0.0 (through the
    # default of base time).
    if { ! [dict exists $pack t] } {
      dict set pack t [Base $s bt]
    }

    # Resolve time to absolute value
    set now [clock seconds]
    set t [dict get $pack t]
    if { $t < $vars::reltime } {
      # Seconds from/after now.
      dict set pack t [expr {$now+$t}]
    }

    Callback $s PACK $pack
  }
}


proc ::sensml::Base { s f } {
  upvar \#0 $s S

  if { [dict exists $S $f] } {
    return [dict get $S $f]
  } else {
    dict for {base init} $vars::base {
      if { $base eq $f } {
        return $init
      }
    }
  }
  return "";  # Default for unknown base fields
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


proc ::sensml::Callback { s step args } {
  upvar \#0 $s S

  if { [dict get $S -callback] ne "" } {

  }
  Log $s DEBUG "Callback: $step $args"
}

proc ::sensml::Log { s lvl msg } {
  upvar \#0 $s S
  if { [dict get $S -log] ne "" } {
    set lvl [string tolower $lvl]
    if { [lsearch -nocase $vars::levels $lvl] <= [lsearch -nocase $vars::levels [dict get $S -level]] } {
      puts [dict get $S -log] "\[$lvl\] $msg"
    }
  }
}

package provide sensml $::sensml::vars::version