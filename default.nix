# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

# This file sets up the top-level package set by traversing the package tree
# (see read-tree.nix for details) and constructing a matching attribute set
# tree.
#

{ ... }@args:

with builtins;
let
  # This definition of fix is identical to <nixpkgs>.lib.fix, but the global
  # package set is not available here.
  fix = f:
    let x = f x; in x;

  # Global configuration that all packages are called with.
  config = depot: {
    inherit depot;

    # Pass third_party as 'pkgs' (for compatibility with external
    # imports for certain subdirectories)
    pkgs = depot.third_party;

  };

  readTree' = import ./third_party/git.tazj.in/readTree { };

  localPkgs = readTree: {
    presentations = readTree ./presentations;
    third_party = readTree ./third_party;
    tools = readTree ./tools;
    web = readTree ./web;
  };
in
fix (self: {
  config = config self;

  # Elevate 'lib' from nixpkgs
  lib = self.third_party.nixpkgs.lib;

  # Expose readTree for downstream repo consumers.
  readTree = {
    __functor = x: (readTree' x.config);
    config = self.config;
  };
}

# Add local packages as structured by readTree
// (localPkgs (readTree' (self.config // { inherit (self) lib; })))

# Load overrides into the top-level.
#
# This can be used to move things from third_party into the top-level, too (such
# as `lib`).
// (readTree' { depot = self; pkgs = self.third_party; }) ./overrides)
