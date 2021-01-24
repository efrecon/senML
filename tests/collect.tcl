package require ldict

namespace eval ::collect {
  variable collector {}

  namespace export {[a-z]*}
  namespace ensemble create
}

proc ::collect::init {} {
  variable collector
  set collector [list]
}
proc ::collect::equal { l1 l2 } {
  if { [llength $l1] != [llength $l2] } {
    return 0
  }
  foreach {s1 d1} $l1 {s2 d2} $l2 {
    if { $s1 ne $s2 || ![ldict deq $d1 $d2] } {
      return 0
    }
  }
  return 1
}

proc ::collect::is { l1 } {
  variable collector
  return [equal $l1 $collector]
}

proc ::collect::collect { stream step {d {}} } {
  variable collector
  lappend collector $step $d
}

package provide collect 0.1