#!/usr/bin/env tclsh

lappend auto_path [file join [file dirname [info script]] ..]
package require senML

# From https://tools.ietf.org/html/rfc8428#section-5.1.2
set json {
   [
     {"bn":"urn:dev:ow:10e2073a0108006:","bt":1.276020076001e+09,
      "bu":"A","bver":5,
      "n":"voltage","u":"V","v":120.1},
     {"n":"current","t":-5,"v":1.2},
     {"n":"current","t":-4,"v":1.3},
     {"n":"current","t":-3,"v":1.4},
     {"n":"current","t":-2,"v":1.5},
     {"n":"current","t":-1,"v":1.6},
     {"n":"current","v":1.7}
   ]
}


proc datapoint { s step {pack {}} } {
  if { $step eq "PACK" } {
    dict for {k v} $pack {
      puts "$k => $v"
    }
    puts ""
  }
}

senML stream $json -callback datapoint