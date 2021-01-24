package require Tcl 8.6

namespace eval ::ldict {
  namespace eval vars {
    variable debug 0
  }

  namespace export {[a-z]*}
  namespace ensemble create
}

proc ::ldict::deq { d1 d2 } {
  if { [dict size $d1] != [dict size $d2] } {
    return 0
  }
  dict for {k v} $d1 {
    if { ![dict exists $d2 $k] } {
      return 0
    }
    if { ! [Equal $v [dict get $d2 $k]] } {
      return 0
    }
  }
  return 1
}

proc ::ldict::equal { l1 l2 } {
  if { [llength $l1] != [llength $l2] } {
    return 0
  }
  foreach d1 $l1 d2 $l2 {
    if { ![deq $d1 $d2] } {
      return 0
    }
  }
  return 1
}

proc ::ldict::Equal { v1 v2 } {
  if { [string is integer -strict $v1] } {
    if { [string is boolean -strict $v2] } {
      return [expr {[string is true $v1] == [string is true $v2]}]
    } else {
      return [expr {$v1==$v2}]
    }
  } elseif { [string is double -strict $v1] } {
    return [expr {$v1==$v2}]
  } elseif { [string is boolean -strict $v1] && [string is boolean -strict $v2] } {
    return [expr {[string is true $v1] == [string is true $v2]}]
  } else {
    return [string equal $v1 $v2]
  }
  return 0; # Never reached
}

package provide ldict 0.1
