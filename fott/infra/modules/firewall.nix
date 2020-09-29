# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, ... }: {

  networking = {
    firewall = {
      allowPing = true;
      enable = true;
      rejectPackets = true;
    };
  };

  networking.firewall.allowedTCPPorts = [ ];

  networking.firewall.allowedUDPPorts = [
    lib.optionals
    (config.services.tailscale.enable)
    41641
  ];

  networking.firewall.interfaces."tailscale0".allowedTCPPorts = [
    lib.optionals
    (config.services.openssh.enable)
    22
    lib.optionals
    (config.services.telegraf.enable)
    9273
  ];

  networking.firewall.interfaces."tailscale0".allowedUDPPortRanges = [
    # Mosh
    lib.optionals
    (config.services.openssh.enable)
    { from = 60000; to = 60010; }
  ];
}
