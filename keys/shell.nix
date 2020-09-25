# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

with import <nixpkgs> { };
mkShell {
  # imports all files ending in .asc/.gpg and sets $SOPS_PGP_FP.
  sopsPGPKeyDirs = [
    "./hosts"
    "./users"
  ];
  nativeBuildInputs = [
    (pkgs.callPackage <sops-nix> { }).sops-pgp-hook
  ];
}
