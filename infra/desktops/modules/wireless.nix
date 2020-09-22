# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, sops, ... }: {
  networking.wireless.enable = true; # Enables wireless support via wpa_supplicant.

  imports = [ ../secrets/wireless-networks.nix ];

  #networking.wireless.networks = builtins.fromJSON (builtins.readFile ../secrets/wireless-networks.json);
}
