# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, ... }: {
  users.extraUsers.ghuntley = {
    shell = pkgs.zsh;
    home = "/home/ghuntley";
    description = "Geoffrey Huntley";
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "docker"
      "libvirtd"
      "audio"
      "video"
      "transmission"
      "networkmanager"
      "cdrom"
    ];
    uid = 1000;
    openssh.authorizedKeys.keys = [ "" ];
  };

  users.extraGroups.ghuntley.gid = 1000;
}
