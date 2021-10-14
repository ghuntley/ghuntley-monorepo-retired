# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

let keys = import ../../../keys; # TODO(high): retrieve keys from depot instead of manually importing via file
in
{ pkgs, sops, ... }: {

  users.extraUsers.mgmt = {
    shell = pkgs.fish;
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
    openssh.authorizedKeys.keys = [ keys.users.mgmt.ssh ];
  };

  users.extraGroups.mgmt.gid = 1000;

#  sops.secrets.yubico-challenge-10158360-mgmt = {
#    format = "binary";
#    sopsFile = ../secrets/yubico-challenge-10158360;
#    path = "/home/mgmt/.yubico/challenge-10158360";
#    owner = "mgmt";
#    group = "users";
#    mode = "0440";
#  };
#
#  sops.secrets.yubico-challenge-7029292-mgmt = {
#    format = "binary";
#    sopsFile = ../secrets/yubico-challenge-7029292;
#    path = "/home/mgmt/.yubico/challenge-7029292";
#    owner = "mgmt";
#    group = "users";
#    mode = "0440";
#  };

}
