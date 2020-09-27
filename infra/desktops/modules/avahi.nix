# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {

  networking.firewall.allowedTCPPorts = [ 5353 ];

  services.avahi = {
    enable = true;
    nssmdns = true;

    publish = {
      enable = true;
      userServices = true;
    };
  };

}
