# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ lib, config, ... }: {
  networking.firewall.interfaces."tailscale0".allowedTCPPorts = [ 9273 ];
  services.telegraf = {
    enable = true;
    extraConfig = {
      inputs = {
        system = { };
        mem = { };
        zfs = lib.optionalAttrs (lib.any (fs: fs == "zfs") config.boot.supportedFilesystems) { };
        systemd_units = { };
        swap = { };
        disk.tagdrop = {
          fstype = [ "tmpfs" "ramfs" "devtmpfs" "devfs" "iso9660" "overlay" "aufs" "squashfs" ];
          device = [ "rpc_pipefs" "lxcfs" "nsfs" "borgfs" ];
        };
      };
      outputs = {
        prometheus_client = {
          listen = ":9273";
          metric_version = 2;
        };
      };
    };
  };
}
