# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs ? import <nixpkgs> { } }: {
  test = pkgs.nixosTest ./test.nix;
}
