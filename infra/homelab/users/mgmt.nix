# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, ... }: {
  users.extraUsers.mgmt = {
    shell = pkgs.zsh;
    home = "/home/mgmt";
    description = "QSECOFR";
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "docker"
      "libvirtd"
      "audio"
      "video"
      "networkmanager"
      "cdrom"
    ];
    uid = 1000;
    openssh.authorizedKeys.keys = [ "" ];
  };

  users.extraGroups.mgmt.gid = 1000;
}
