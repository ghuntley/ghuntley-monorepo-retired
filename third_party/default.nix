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
    "p7zip-16.02"
  ];
}; in

let
  # Tracking nixos-unstable
  nixpkgs = import ./github.com/nixos/nixpkgs-channels {
    config = nixpkgsConfig;
  };

  exposed = {

    # Inherit the packages from nixos-unstable that should be available inside
    # of the repo. They become available under `pkgs.third_party.<name>`

    # Archivers
    inherit (nixpkgs)
      p7zip
      unzip
    ;

    # Cloud vendors
    inherit (nixpkgs)
      aws
      azure-cli
      google-cloud-sdk
      pulumi-bin
      terraform
    ;

    # CI
    inherit (nixpkgs)
        editorconfig-checker
    ;

    # Cryptography
    inherit (nixpkgs)
      libsodium
      sops
    ;

    # Dhall
    inherit (nixpkgs)
      dhall
    ;

    # Docker
    inherit (nixpkgs)
      docker
      docker-compose
      dockerTools
    ;

    # Editors
    inherit (nixpkgs)
      ctags
      fzf
      neovim
      vimPlugins
    ;

    # Games
    inherit (nixpkgs)
      ioquake3
      quake3e
      quake3pointrelease
      quake3wrapper
    ;

    # Haskell
    inherit (nixpkgs)
      hlint
      ormolu
    ;

    # Utilities
    inherit (nixpkgs)
      ack
      bat
      curl
      flamegraph
      jq
      openldap
      rpl
      tmux
      tree
      watchman
      youtube-dl
    ;

    # Python
    inherit (nixpkgs)
      black
      python
    ;

    inherit (nixpkgs.pythonPackages)
      cookiecutter
      flake8
      isort
      pip
      pydocstyle
      pylint
    ;

    # Rust
    inherit (nixpkgs)
      rustfmt
    ;

    # Shell
    inherit (nixpkgs)
      shellcheck
    ;

    # Source Control
    inherit (nixpkgs)
      git
      mercurialFull
    ;

    inherit (nixpkgs.gitAndTools)
      git-bug
      rs-git-fsmonitor
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
      runCommandLocal
      s6-portable-utils
      stdenvNoCC
      mktemp
      nixpkgs-fmt
      nixos-shell
      writeText
      writeShellScript
      writeShellScriptBin
    ;

    # node.js
    inherit (nixpkgs)
      nodejs
      yarn;

    inherit (nixpkgs.nodePackages)
      npm;

    inherit (nixpkgs)
      nomad_0_12;

    # Windows
    inherit (nixpkgs)
      powershell;

    # YAML
    inherit (nixpkgs)
      yamllint;
  };

in exposed.lib.fix(self: exposed // {
  callPackage = nixpkgs.lib.callPackageWith self;

  # Packages to be overridden
  originals = {
    inherit (nixpkgs) openldap neovim;
  };

  # Use LLVM 11
  llvmPackages = nixpkgs.llvmPackages_11;
  clangStdenv = nixpkgs.llvmPackages_11.stdenv;
  stdenv = nixpkgs.llvmPackages_11.stdenv;

  # Python 3
  python = nixpkgs.python38;
  pythonPackages = nixpkgs.python38Packages;

  # Terraform
  terraform = nixpkgs.terraform_0_13;

  # Supplementals
  cabal-fmt = nixpkgs.haskellPackages.callCabal2nix "cabal-fmt" ./github.com/phadej/cabal-fmt {};
  nix-linter = nixpkgs.haskellPackages.callCabal2nix "nix-linter" ./github.com/synthetica9/nix-linter {};

  # Nomad
  nomad = nixpkgs.nomad_0_12;

  # Make NixOS available
  nixos = import ./github.com/nixos/nixpkgs-channels/nixos;
  nixeval = import ./github.com/nixos/nixpkgs-channels/nixos/lib/eval-config.nix;

  # Make SOPS available
  sops-nix = import ./github.com/mic92/sops-nix;
})
