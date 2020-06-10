# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

# This file controls the import of external dependencies (i.e.
# third-party code) into my package tree.
#
# This includes *all packages needed from nixpkgs*.
{ ... }:

let
  # Tracking nixos-unstable
  nixpkgs = (import ./github.com/nixos/nixpkgs-channels) {
    config.allowUnfree = true;
    config.allowBroken = true;
  };

  exposed = {
    # Inherit the packages from nixos-unstable that should be available inside
    # of the repo. They become available under `pkgs.third_party.<name>`
    inherit (nixpkgs)
      ack
      aws
      azure-cli
      cachix
      lib
      git
      google-cloud-sdk
      jq
      rpl
      terraform
      tmux
      tree;
  };

in exposed.lib.fix(self: exposed // {
  callPackage = nixpkgs.lib.callPackageWith self;
  
  # Packages to be overridden
  originals = {
    inherit (nixpkgs);
    git = nixpkgs.gitAndTools.gitFull;
    terraform = nixpkgs.terraform_0_12;
  };

  # Make NixOS available
  nixos = (import ./github.com/nixos/nixpkgs-channels/nixos);
})
