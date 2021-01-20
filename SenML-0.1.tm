package require Tcl 8.6
package require json
package require sensml

namespace eval ::senml {
  namespace eval vars {
    variable id 0;        # Identifier generator
    variable version [lindex [split [file rootname [file tail [info script]]] -] end]
  }

  namespace export {[a-z]*}
  namespace ensemble create
}


proc ::senml::parse { json args } {
  set s [sensml {*}$args]
  $s stream $json
  $s close
  unset $s
}

proc ::senml::resolve { json } {
  set j [namespace current]::[incr vars::id]
  upvar \#0 $j resolved
  set resolved ""
  set s [sensml -callback [list ::senml::Resolver $j]]
  $s stream $json
  unset $s
  set str [set $j]
  unset $j
  return $str
}


proc ::senml::Resolver { j s step args } {
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
      append json "\{"
      dict for {k v} [lindex $args 0] {
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
      append json "\},"
    }
  }
}

package provide senml $::senml::vars::version