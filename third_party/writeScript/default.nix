{ pkgs, depot, ... }:

# Write the given string to $out
# and make it executable.

let
  bins = depot.third_party.getBins pkgs.s6-portable-utils [
           "s6-cat"
           "s6-chmod"
         ];

in
name:
# string of the executable script that is put in $out
script:

depot.third_party.runExecline name {
  stdin = script;
  derivationArgs = {
    preferLocalBuild = true;
    allowSubstitutes = false;
  };
} [
  "importas" "out" "out"
  # this pipes stdout of s6-cat to $out
  # and s6-cat redirects from stdin to stdout
  "if" [ "redirfd" "-w" "1" "$out" bins.s6-cat ]
  bins.s6-chmod "0755" "$out"
]
