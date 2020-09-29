# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

# This file controls the import of external dependencies (i.e.
# third-party code) into my package tree.
#
# This includes *all packages needed from nixpkgs*.
{ ... }:
let nixpkgsConfig = {
  allowBroken = true;
  allowUnfree = true;
  permittedInsecurePackages = [
  ];
}; in
let
  # Tracking nixos-unstable
  nixpkgs = import ../third_party/github.com/nixos/nixpkgs {
    config = nixpkgsConfig;
  };

  exposed = {

    # Inherit the packages from nixos-unstable that should be available inside
    # of the repo. They become available under `pkgs.third_party.<name>`

    # Cloud vendors
    inherit (nixpkgs)
      pulumi-bin
      ;

    # Cryptography
    inherit (nixpkgs)
      sops
      ;

    # Dhall
    inherit (nixpkgs)
      dhall
      ;

    # Editors
    inherit (nixpkgs)
      neovim
      ;

    # Source Control
    inherit (nixpkgs)
      git
      ;

    # NIX
    inherit (nixpkgs)
      cachix
      execline
      fetchFromGitHub
      fetchgit
      fetchurl
      fetchzip
      lib
      linuxPackages
      runCommand
      runCommandLocal
      s6-portable-utils
      stdenvNoCC
      makeWrapper
      mktemp
      nixpkgs-fmt
      nixos-shell
      writeText
      writeShellScript
      writeShellScriptBin
      ;

    # Nomad
    inherit (nixpkgs)
      nomad_0_12
      ;

  };

in
exposed.lib.fix (self: exposed // {
  callPackage = nixpkgs.lib.callPackageWith self;

  # Packages to be overridden
  originals = {
    inherit (nixpkgs);
  };


  # Use LLVM 11
  llvmPackages = nixpkgs.llvmPackages_11;
  clangStdenv = nixpkgs.llvmPackages_11.stdenv;
  stdenv = nixpkgs.llvmPackages_11.stdenv;

  # Supplementals

  # Nomad
  nomad = nixpkgs.nomad_0_12;

  # Make NixOS available
  nixos = import ./../third_party/github.com/nixos/nixpkgs/nixos;
  nixeval = import ./../third_party/github.com/nixos/nixpkgs/nixos/lib/eval-config.nix;

  # Make SOPS available
  sops-nix = import ../third_party/github.com/mic92/sops-nix;
})
