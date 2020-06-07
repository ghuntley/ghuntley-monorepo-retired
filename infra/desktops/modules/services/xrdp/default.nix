# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, ... }: {
  services.xrdp.enable = true;
  services.xrdp.defaultWindowManager = "${pkgs.i3}/bin/i3";
  networking.firewall.allowedTCPPorts = [ 3389 ];
}

