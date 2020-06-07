{ nixpkgs ? import ./nix/nixpkgs-overlayed.nix }:
(nixpkgs.pkgs.haskell.lib.addBuildTools (import ./. {})
  (with nixpkgs.pkgs ; [
    cabal-install
  ])
).env
