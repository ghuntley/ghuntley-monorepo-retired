# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ config, pkgs, lib, stdenv, ... }: {
  imports = [ ./root.nix ./mgmt.nix ];
}
