{ depot, pkgs, lib, ... }:
let
  runExecline = import ./runExecline.nix {
    inherit (pkgs) stdenv;
    inherit (depot.third_party) escapeExecline getBins;
    inherit pkgs lib;
  };

  tests = import ./tests.nix {
    inherit runExecline;
    inherit (depot.third_party) getBins writeScript;
    inherit (pkgs) stdenv coreutils;
    inherit pkgs;
  };

in {
  __functor = _: runExecline;
  inherit tests;
}
