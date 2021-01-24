#!/usr/bin/env tclsh

lappend auto_path [file join [file dirname [info script]] ..]
package require senML

proc datapoint { s step {pack {}} } {
  if { $step eq "PACK" } {
    dict for {k v} $pack {
      puts "$k => $v"
    }
    puts ""
  }
}

set s [senSML -callback datapoint -level TRACE]
$s stream {[{"n":"urn:dev:ow:10e2073a01080063","u":"Cel","t":1.276020076e+09,"v":23.5},}
$s stream {{"n":"urn:dev:ow:10e2073a01080063","u":"Cel","t":1.276020091e+09,"v":23.6},}
$s stream "{\"n\":\"urn:dev:ow:10e2073a01080063\","
$s stream "\"u\":\"Cel\",\"t\":1.276020099e+09,\"v\":23.7},"