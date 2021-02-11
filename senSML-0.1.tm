package require Tcl 8.6
package require json

namespace eval ::senSML {
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
  namespace eval senSML {
    variable -log stderr;       # Log stream, empty to switch off
    variable -level WARN;       # loglevel
    variable -callback  ""
    variable -version   10
    variable -relax     TIME;   # A list of features to perform relax parsing on
    variable -future    17179869184; # This is 2**34, everything greater is ms!
  }

  namespace export {[a-z]*}
  # Create an alias for new as the name of the current namespace, this is the
  # only command that is really exposed.
  interp alias {} [namespace current] {} [namespace current]::new
}


# ::senSML::new -- Create parsing context.
#
#       This procedure creates a SenSML parsing context and is the only command
#       exported by this parser. The command returns an identifier to the
#       context that should be used for all further calls to this library. To
#       feed the context, either perform a series of calls to stream with
#       incomplete JSON data, or call it with entire JSON objects in SenML
#       format, with calls to begin/(end) to mark the beginning and end of an
#       array.
#
# Arguments:
#	  args	List of dashled options and arguments, must match content of namespace
#
# Results:
#       Returns an identifier for all further interaction with the parsing
#       context
#
# Side Effects:
#       None.
proc ::senSML::new { args } {
  # Create new stream object
  set s [namespace current]::[incr vars::id]
  upvar \#0 $s S

  # Capture arguments and give good defaults into the stread object (a
  # dictionary). Create the command
  defaults S senSML
  configure $s {*}$args
  interp alias {} $s {} [namespace current]::Dispatch $s

  # Initialise the internal state of the stream
  dict set S remainder ""
  dict set S state ""
  Init $s

  return $s
}


# ::senSML::close -- (force-)close current array
#
#       This procedure closes the current array in progress, if any. It will
#       reset the value of all base fields in preparation for the next array, if
#       any. This does not destroy the context, instead it can be used to parse
#       several JSON arrays using the same context.
#
# Arguments:
#	  s	  Identifier of a parsing context, as returned by new
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::senSML::close { s } {
  end $s
  Init $s
}


# ::senSML::delete -- delete parsing context
#
#       This procedure deletes a parsing context and removes it from memory. The
#       current array is forced closed before deletion..
#
# Arguments:
#	  s	  Identifier of a parsing context, as returned by new
#
# Results:
#       None.
#
# Side Effects:
#       None.
proc ::senSML::delete { s } {
  close $s
  unset $s
}


# ::senSML::configure -- (re)configure a parsing context
#
#       This procedure changes the values of one of the dash-led options for the
#       context.
#
# Arguments:
#	  s	    Identifier of a parsing context, as returned by new
#	  args	List of dashled options and arguments, must match content of namespace
#
# Results:
#       Returns a dictionary of all options and their values after
#       (re)configuration.
#
# Side Effects:
#       None.
proc ::senSML::configure { s args } {
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

  return dict filter $S key "-*"
}


# ::senSML::stream -- Push incomplete JSON for continuous parsing.
#
#       This procedure accepts possibly incomplete JSON and will isolate the
#       beginning of arrays and cut the remaining into separated JSON objects to
#       be parsed as SenML packs. Discovery of arrays and objects is made at the
#       string level, outside of the JSON parser.
#
# Arguments:
#	  s	    Identifier of a parsing context, as returned by new
#	  json	Incomplete JSON
#
# Results:
#       None
#
# Side Effects:
#       Will call the callback associated to the context as many times as needed
proc ::senSML::stream { s json } {
  upvar \#0 $s S

  # Nothing to do on empty input
  if { $json eq "" } {
    return
  }

  Log $s TRACE "Streaming in $json"

  # Append remainder of previous part of stream, if any
  if { [dict get $S remainder] ne "" } {
    set json [dict get $S remainder]$json
  }

  # Remove leading space and detect beginning of array.
  set json [string trimleft $json]
  if { [string index $json 0] eq "\[" } {
    begin $s
    set json [string trim [string range $json 1 end]]
  }

  # Arrange for object-to-object transitions and object-to-end-of-array
  # transitions to not contain whitespaces. This is not JSON aware, thus could
  # in theory intervene with the content of the stream (e.g. content of a
  # matching value).
  set json [regsub -all -- {\}\s*,\s*\{} $json "\},\{"]
  set json [regsub -all -- {\}\s*\]} $json "\}\]"]

  # Isolates blocks and callback with content for each pack
  set start 0
  while {$start<[string length $json]} {
    set open [string first "\{" $json $start]
    set nxt [string first "\}," $json $open]
    set end [string first "\}\]" $json $open]
    if { $open >= 0 } {
      if { $end >= 0 && $nxt < 0 } {
        jsonpack $s [string range $json $open $end]
        end $s
        break
      } elseif { $nxt >= 0 } {
        if { $end < $nxt && $end >= 0 } {
          jsonpack $s [string range $json $open $end]
          end $s
          break
        } else {
          jsonpack $s [string range $json $open $nxt]
          set start [expr {$nxt+2}]
        }
      } else {
        # Remember what is left after the last entire JSON object for next call
        # to this proc.
        dict set S remainder [string range $json $open end]
        break
      }
    } else {
      # Remember what is left after the last entire JSON object for next call
      # to this proc.
      dict set S remainder [string range $json $start end]
      break
    }
  }
}


# ::senSML::begin -- Begin array parsing
#
#       This procedure (re)initialise the parsing context and starts a new array
#       of JSON object packs.
#
# Arguments:
#	  s	    Identifier of a parsing context, as returned by new
#
# Results:
#       None
#
# Side Effects:
#       Will call the callback associated to the context as many times as needed
proc ::senSML::begin { s } {
  upvar \#0 $s S

  Init $s
  if { [dict get $S state] eq "" } {
    Callback $s OPEN
    dict set S state OPEN
  }
}

# ::senSML::end -- End array parsing
#
#       This procedure ends the current JSON array and cleans transient state.
#
# Arguments:
#	  s	    Identifier of a parsing context, as returned by new
#
# Results:
#       None
#
# Side Effects:
#       Will call the callback associated to the context as many times as needed
proc ::senSML::end { s } {
  upvar \#0 $s S
  if { [dict get $S state] eq "OPEN" } {
    Callback $s CLOSE
    dict set S remainder ""
    dict set S state ""
  }
}


# ::senSML::Init -- (re)initialise context
#
#       This procedure (re)initialise a parsing context by clearing out the
#       value of all base fields.
#
# Arguments:
#	  s	    Identifier of a parsing context, as returned by new
#
# Results:
#       None
#
# Side Effects:
#       Clear out base fields value
proc ::senSML::Init { s } {
  upvar \#0 $s S

  dict for {base init} $vars::base {
    if { [dict exists $S $base] } {
      dict unset S $base
    }
  }
}


# ::senSML::jsonpack -- Process one JSON object pack
#
#       This procedure processes a single JSON object pack and performs a
#       callback with all resolved values.
#
# Arguments:
#	  s	    Identifier of a parsing context, as returned by new
#	  json	Valid JSON object
#
# Results:
#       None
#
# Side Effects:
#       Will call the callback associated to the context as many times as needed
proc ::senSML::jsonpack { s json } {
  # Nothing to do on empty input
  if { $json eq "" } {
    return
  }

  Log $s TRACE "JSON Pack: $json"
  # Parse incoming JSON as a Tcl dictionary and pass it along to dictpack which
  # will perform all the work.
  dictpack $s [::json::json2dict $json]
}


# ::senSML::dictpack -- Process one dictionary, representing a JSON object pack
#
#       This procedure processes a dictionary representing a JSON object pack
#       and performs a callback with all resolved values.
#
# Arguments:
#	  s	    Identifier of a parsing context, as returned by new
#	  d	    Tcl dictionary
#
# Results:
#       None
#
# Side Effects:
#       Will call the callback associated to the context as many times as needed
proc ::senSML::dictpack { s d } {
  upvar \#0 $s S

  Log $s TRACE "DICT Pack: $d"
  if { [dict get $S state] ne "OPEN" } {
    begin $s
  }

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

    # If we only have base fields, nothing more should be done. See example in
    # section 5.1.7 in RFC 8428.
    if { [dict size [dict filter $d key b*]] == [dict size $d] } {
      return
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
    if { [lsearch -nocase [dict get $S -relax] TIME*] >= 0 && $t > [dict get $S -future] } {
      dict set pack t [expr {$t/1000.0}]
    } elseif { $t < $vars::reltime } {
      # Seconds from/after now.
      dict set pack t [expr {$now+$t}]
    }

    ## Add resolution for base64 encoded stuff and check types for vb, etc.

    Callback $s PACK $pack
  }
}


# ::senSML::Base -- Value of a base field
#
#       This procedure computes the value of a base field. If the field has been
#       set in the past within the parsing context, the value will be taken from
#       the context. Otherwise, if it is known, the value will be taken from the
#       default values.
#
# Arguments:
#	  s	    Identifier of a parsing context, as returned by new
#	  f	    Name of field, needs to start with b
#
# Results:
#       Value of field, empty for all unknown base fields.
#
# Side Effects:
#       None
proc ::senSML::Base { s f } {
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

# ::senSML::getopt -- Get options
#
#       From http://wiki.tcl.tk/17342
#
# Arguments:
#	  _argv	  "pointer" to incoming arguments
#	  name	  Name of option to extract
#	  _var	  Pointer to variable to set
#	  default	Default value
#
# Results:
#       1 if the option was found, 0 otherwise
#
# Side Effects:
#       None.
proc ::senSML::getopt {_argv name {_var ""} {default ""} } {
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


# ::senSML::defaults -- Init and option parsing based on namespace.
#
#       This procedure takes the dashled variables of a given (sub)namespace to
#       initialise a dictionary. These variables are considered as being the
#       canonical set of options for a command or object and contain good
#       defaults, and the procedure will capture these from the arguments.
#
# Arguments:
#	  cx_	  "Pointer" to dictionary to initialise and parse options in.
#	  ns	  Namespace (FQ or relative to caller) where to get options from
#	  args	List of dashled options and arguments, must match content of namespace
#
# Results:
#       Return the list of options that were taken from the arguments, an error
#       when an option that does not exist in the namespace as a variable was
#       found in the arguments.
#
# Side Effects:
#       None.
proc ::senSML::defaults { cx_ ns args } {
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


# ::senSML::isolate -- Isolate options from arguments
#
#       Isolate dash-led options from the rest of the arguments. This procedure
#       prefers the double-dash as a marker between the options and the
#       arguments, but it is also able to traverse until the end of the options
#       and the beginning of the arguments. Traversal requires that no value of
#       an option starts with a dash to work properly.
#
# Arguments:
#	  args_	  Pointer to list of arguments (will be modified!)
#	  opts_	  Pointer to list of options
#
# Results:
#       None.
#
# Side Effects:
#       Modifies the args and opts lists that are passed as parameters to
#       reflect the arguments and the options.
proc ::senSML::isolate { args_ opts_ } {
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


# ::senSML::Dispatch -- Objectifying proc.
#
#       Dispatch to the procedure matching the command. Uses tailcall for
#       optimisation. This implements the Tk-style calling conventions on
#       contexts.
#
# Arguments:
#	  s	    Identifier of a parsing context, as returned by new
#	  cmd	  Name of command to call, needs to be a first order proc in namespace.
#	  args	Arguments to procedure, passed blindly to proc.
#
# Results:
#       None.
#
# Side Effects:
#       Replaces current call by call to procedure (tailcall)
proc ::senSML::Dispatch { s cmd args } {
  # Try finding the command as one of our internally implemented procedures.
  if { [string tolower $cmd] eq $cmd } {
    if { [llength [info commands [namespace current]::$cmd]] } {
      tailcall [namespace current]::$cmd $s {*}$args
    }
  }
  return -code error "$cmd is not a known operation"
}


# ::senSML::Callback -- Controlled callbacking.
#
#       Perform callbacks, don't bail out on errors, rather scream in log.
#
# Arguments:
#	  s	    Identifier of a parsing context, as returned by new
#	  step	Parsing step.
#	  args	Arguments to procedure, passed blindly to callback command
#
# Results:
#       None.
#
# Side Effects:
#       Catches errors and sends them to log
proc ::senSML::Callback { s step args } {
  upvar \#0 $s S

  Log $s DEBUG "Callback: $step $args"
  if { [llength [dict get $S -callback]] } {
    if { [catch {{*}[dict get $S -callback] $s $step {*}$args} err] } {
      Log $s WARN "Could not callback for $step: $err"
    }
  }
}


# ::senSML::Log -- Conditional logging
#
#       Logs depending on the current log level of the context.
#
# Arguments:
#	  s	    Identifier of a parsing context, as returned by new
#	  lvl	  Level of the message.
#	  msg	  Message to log
#
# Results:
#       None.
#
# Side Effects:
#       Print formatted message to -log file descriptor if relevant.
proc ::senSML::Log { s lvl msg } {
  upvar \#0 $s S
  if { [dict get $S -log] ne "" } {
    if { [lsearch -nocase $vars::levels $lvl] <= [lsearch -nocase $vars::levels [dict get $S -level]] } {
      set dest [dict get $S -log]
      if { [string match "@*" $dest] } {
        set dest [string range $dest 1 end]
        catch {{*}$dest $lvl $msg}
      } else {
        set lvl [string tolower $lvl]
        puts $dest "\[$lvl\] $msg"
      }
    }
  }
}

package provide senSML $::senSML::vars::version