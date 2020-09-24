{ pkgs, depot, ... }:

# Write an execline script, represented as nested nix lists.
# Everything is escaped correctly.
# https://skarnet.org/software/execline/

# TODO(Profpatsch) upstream into nixpkgs

name:
{
  # "var": substitute readNArgs variables and start $@
  # from the (readNArgs+1)th argument
  # "var-full": substitute readNArgs variables and start $@ from $0
  # "env": don’t substitute, set # and 0…n environment vaariables, where n=$#
  # "none": don’t substitute or set any positional arguments
  # "env-no-push": like "env", but bypass the push-phase. Not recommended.
  argMode ? "var",
  # Number of arguments to be substituted as variables (passed to "var"/"-s" or "var-full"/"-S"
  readNArgs ? 0,
}:
# Nested list of lists of commands.
# Inner lists are translated to execline blocks.
argList:

let
  env =
    if      argMode == "var" then "s${toString readNArgs}"
    else if argMode == "var-full" then "S${toString readNArgs}"
    else if argMode == "env" then ""
    else if argMode == "none" then "P"
    else if argMode == "env-no-push" then "p"
    else abort ''"${toString argMode}" is not a valid argMode, use one of "var", "var-full", "env", "none", "env-no-push".'';

in
  depot.third_party.writeScript name ''
    #!${pkgs.execline}/bin/execlineb -W${env}
    ${depot.third_party.escapeExecline argList}
  ''
