# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ lib, pkgs, ... }:
with lib;

{
  # This enables "lingering" for the user.
  # Inspired by the discussion (and linked code)
  # in https://github.com/NixOS/nixpkgs/issues/3702
  # This should just be a NixOS option really.
  system.activationScripts = {
    enableLingering = ''
      # remove all existing lingering users
      rm -r /var/lib/systemd/linger
      mkdir /var/lib/systemd/linger
      # enable for the subset of declared users
      touch /var/lib/systemd/linger/ghuntley
    '';
  };

  environment.systemPackages = [ pkgs.mosh ];

  networking.firewall = {
    allowedUDPPortRanges = [{
      from = 60000;
      to = 61000;
    } # mosh
      ];
  };

}

