# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {

  environment.systemPackages = with pkgs; [ tailscale ];

  services.tailscale.enable = true;

  networking.firewall.allowedUDPPorts = [ 41641 ];
}
