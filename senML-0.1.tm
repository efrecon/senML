package require Tcl 8.6
package require json
package require senSML

namespace eval ::senML {
  namespace eval vars {
    variable id 0;        # Identifier generator
    variable version [lindex [split [file rootname [file tail [info script]]] -] end]
  }

  namespace export {[a-z]*}
  namespace ensemble create
}

# ::senML::parse -- Parse a JSON array in SenML
#
#       Parse a JSON array in SenML and provide callback with resolved packs as
#       a Tcl dictionary. See sensml::new for options.
#
# Arguments:
#	  json	JSON array in SenML
#	  args	List of dash-led options and their values.
#
# Results:
#       None.
#
# Side Effects:
#       Will callback the command at -callback as parsing progresses
proc ::senML::stream { json args } {
  # Nothing to do on empty input
  if { $json eq "" } {
    return
  }

  # Create a SenSML context, pass it all arguments.
  set s [senSML {*}$args]

  # Parse the entire JSON array and pass each dictionary separately for
  # processing at the SenSML parser. This bypasses the heuristics (and therefor
  # possible errors) that would occur if we called $s stream instead.
  $s begin
  foreach d [::json::json2dict $json] {
    $s dictpack $d
  }
  $s end

  # Remove context, we are done
  $s delete
}


# ::senML::resolve -- Convert JSON array in SenML to contain resolved packs only
#
#       Convert a JSON array in SenML in an array that only contains resolved
#       packs, i.e. SenML packs (JSON objects) where the value of all base
#       fields have been integrated into the value of the fields and all time
#       values are absolute.
#
# Arguments:
#	  json	JSON array in SenML
#	  args	List of dash-led options and their values.
#
# Results:
#       A resolved JSON array
#
# Side Effects:
#       None.
proc ::senML::resolve { json args } {
  if { [lsearch $args -callback] >= 0 } {
    return -code error "Cannot provide a callback when resolving to JSON!"
  }

  # Generate a unique string that will be passed to each callback to collect
  # resolved data in JSON format.
  set j [namespace current]::[incr vars::id]
  upvar \#0 $j resolved
  set resolved ""

  # Parse JSON, passing the global string to convert back to JSON progressively.
  stream $json -callback [list ::senML::ResolverJSON $j] {*}$args

  # Copy global string to local var to be able to return it
  # after we've unset the global to avoid leaking memory.
  set str [set $j]
  unset $j

  return $str; # Return the resolved JSON array.
}


proc ::senML::parse { json args } {
  if { [lsearch $args -callback] >= 0 } {
    return -code error "Cannot provide a callback when resolving to dictionaries!"
  }

  # Generate a unique list identifier that will be passed to each callback to
  # collect resolved data in a list
  set l [namespace current]::[incr vars::id]
  upvar \#0 $l resolved
  set resolved [list]

  # Parse JSON, passing the global list to store dictionaries.
  stream $json -callback [list ::senML::ResolverList $l] {*}$args

  # Copy global string to local var to be able to return it
  # after we've unset the global to avoid leaking memory.
  set L [set $l]
  unset $l

  return $L; # Return the resolved list
}


# ::senML::pack2json -- Convert Tcl dictionary back to JSON object
#
#       Convert a Tcl dictionary representing a pack back to a JSON object.
#       Conversion is aware of the field types.
#
# Arguments:
#	  p 	Dictionary representing the JSON pack
#
# Results:
#       A JSON object
#
# Side Effects:
#       None.
proc ::senML::pack2json { p } {
  set json "\{"
  dict for {k v} $p {
    append json "\"$k\":"
    switch -- $k {
      vb {
        if { [string is true -strict $v] } {
          append json "true"
        } else {
          append json "false"
        }
      }
      v -
      s -
      t -
      ut {
        append json "$v"
      }
      vd {
        append json "\"[binary encode base64 $v]\""
      }
      n -
      vs -
      u -
      default {
        append json "\"$v\""
      }
    }
    append json ","
  }
  set json [string trimright $json ","]
  append json "\}"
}


# ::senML::Resolver -- Reconstruct JSON array as parsing progresses
#
#       Complies to -callback format and append JSON to the global string passed
#       as an argument as parsing progresses.
#
# Arguments:
#	  j 	  Identifier of global string where to collect back JSON representation
#	  s 	  Identifier of SenSML parsing context
#	  step 	Step of the parsing
#	  pack 	Dictionary representing the resolved pack, if relevant
#
# Results:
#       None
#
# Side Effects:
#       Reconstruct JSON in j
proc ::senML::ResolverJSON { j s step { pack {} } } {
  upvar \#0 $j json
  switch -- $step {
    OPEN {
      append json "\["
    }
    CLOSE {
      set json [string trimright $json ","]
      append json "\]"
    }
    PACK {
      append json [pack2json $pack ","]
    }
  }
}

proc ::senML::ResolverList { l s step { pack {} } } {
  upvar \#0 $l L

  if { $step eq "PACK" } {
    lappend L $pack
  }
}


package provide senML $::senML::vars::version