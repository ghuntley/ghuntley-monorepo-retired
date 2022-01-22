# Copyright (c) 2019-2021 The TVL Authors
# SPDX-License-Identifier: MIT
#
# This file sets up the top-level package set by traversing the
# package tree using readTree[0] and constructing a matching attribute
# set tree.
#
# [0]: https://code.tvl.fyi/about/nix/readTree
#
# Forked from https://code.tvl.fyi/tree/default.nix?id=f59ab9aba506c1ed149f7093f5543ef021567ebc
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

    # Expose lib & ciBuilds attributes to packages.
    inherit (depot) ciBuilds lib;

    # Pass third_party as 'pkgs' (for compatibility with external
    # imports for certain subdirectories)
    pkgs = depot.third_party;
  };

  readTree' = import ./third_party/readTree { };

  localPkgs = readTree: {
    games = readTree ./games;
    infra = readTree ./infra;
    keys = import ./keys;
    ops = readTree ./ops;
    operating-systems = readTree ./operating-systems;
    patterns = readTree ./patterns;
    third_party = readTree ./third_party;
    tools = readTree ./tools;
    web = readTree ./web;
  };

  # To determine build targets, we walk through the depot tree and
  # fetch attributes that were imported by readTree and are buildable.
  #
  # Any build target that contains `meta.ci = false` will be skipped.

  # Is this tree node eligible for build inclusion?
  eligible = node: (node ? outPath) && (node.meta.ci or true);

  # Walk the tree starting with 'node', recursively extending the list
  # of build targets with anything that looks buildable.
  #
  # Any tree node can specify logical targets by exporting a
  # 'meta.targets' attribute containing a list of keys in itself. This
  # enables target specifications that do not exist on disk directly.
  gather = node:
    if node ? __readTree then
    # Include the node itself if it is eligible.
      (if eligible node then [ node ] else [ ])
      # Include eligible children of the node
      ++ concatMap gather (attrValues node)
      # Include specified sub-targets of the node
      ++ filter eligible (map
        (k: (node."${k}" or { }) // {
          # Keep the same tree location, but explicitly mark this
          # node as a subtarget.
          __readTree = node.__readTree;
          __subtarget = k;
        })
        (node.meta.targets or [ ]))
    else [ ];
in
fix (self: {
  config = config self;

  # Elevate 'lib' from nixpkgs
  lib = self.third_party.lib;

  # Expose readTree for downstream repo consumers.
  readTree = {
    __functor = x: (readTree' x.config);
    config = self.config;
  };

  # Make the path to the depot available for things that might need it
  # (e.g. NixOS module inclusions)
  depotPath = ./.;

  # List of all buildable targets, for CI purposes.
  #
  # Note: To prevent infinite recursion, this *must* be a nested
  # attribute set (which does not have a __readTree attribute).
  ci.targets = gather (self // {
    # remove the pipelines themselves from the set over which to
    # generate pipelines because that also leads to infinite
    # recursion.
    ops = self.ops // { ci = null; };
  });
}

# Add local packages as structured by readTree
// (localPkgs (readTree' self.config))

# Load overrides into the top-level.
#
# This can be used to move things from third_party into the top-level, too (such
# as `lib`).
// (readTree' { depot = self; pkgs = self.third_party; }) ./overrides
)
