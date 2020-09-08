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
    inherit (nixpkgs)
      ack
      aws
      azure-cli
      black
      cachix
      ctags
      docker
      lib
      git
      google-cloud-sdk
      hlint
      jq
      rpl
      rustfmt
      shellcheck
      terraform_0_12
      tmux
      tree
      ormolu
      mercurialFull
      neovim
      nixpkgs-fmt
      nodejs
      p7zip
      pulumi-bin
      python
      watchman
      unzip
      vimPlugins
      yamllint
      yarn
      youtube-dl;

    inherit (nixpkgs.gitAndTools)
      git-bug;

    inherit (nixpkgs.nodePackages)
      npm;

    inherit (nixpkgs.pythonPackages)
      cookiecutter
      flake8
      isort
      pip
      pydocstyle
      pylint;
  };

in exposed.lib.fix(self: exposed // {
  callPackage = nixpkgs.lib.callPackageWith self;

  # Packages to be overridden
  originals = {
    inherit (nixpkgs) neovim;
  };

  # Python 3
  python = nixpkgs.python38;
  pythonPackages = nixpkgs.python38Packages;

  # Supplementals
  cabal-fmt = nixpkgs.haskellPackages.callCabal2nix "cabal-fmt" ./github.com/phadej/cabal-fmt {};
  nix-linter = nixpkgs.haskellPackages.callCabal2nix "nix-linter" ./github.com/synthetica9/nix-linter {};

  # Make NixOS available
  nixos = import ./github.com/nixos/nixpkgs-channels/nixos;
  nixeval = import ./github.com/nixos/nixpkgs-channels/nixos/lib/eval-config.nix;
})
