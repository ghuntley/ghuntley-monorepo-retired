{ nixpkgs ? import ./nix/nixpkgs-overlayed.nix }:

nixpkgs.pkgs.haskellPackages.callCabal2nix "haskell-example" ./. { }
