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

    # Cryptography
    inherit (nixpkgs)
      libsodium
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

    # Haskell
    inherit (nixpkgs)
      hlint
      ormolu
    ;

    # Utilities
    inherit (nixpkgs)
      ack
      bat
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
      git-bug;

    # NIX
    inherit (nixpkgs)
      cachix
      fetchFromGitHub
      fetchgit
      fetchurl
      fetchzip
      lib
      linuxPackages
      nixpkgs-fmt
      writeShellScript
      writeShellScriptBin;

    # YAML
    inherit (nixpkgs)
      yamllint;

    # node.js
    inherit (nixpkgs)
      nodejs
      yarn;

    inherit (nixpkgs.nodePackages)
      npm;
  };

in exposed.lib.fix(self: exposed // {
  callPackage = nixpkgs.lib.callPackageWith self;

  # Packages to be overridden
  originals = {
    inherit (nixpkgs) openldap neovim;
  };

  # Python 3
  python = nixpkgs.python38;
  pythonPackages = nixpkgs.python38Packages;

  terraform = nixpkgs.terraform_0_13;

  # Supplementals
  cabal-fmt = nixpkgs.haskellPackages.callCabal2nix "cabal-fmt" ./github.com/phadej/cabal-fmt {};
  nix-linter = nixpkgs.haskellPackages.callCabal2nix "nix-linter" ./github.com/synthetica9/nix-linter {};

  # Make NixOS available
  nixos = import ./github.com/nixos/nixpkgs-channels/nixos;
  nixeval = import ./github.com/nixos/nixpkgs-channels/nixos/lib/eval-config.nix;
})
