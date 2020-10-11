# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary
let keys = import ../../../keys; # TODO(high): retrieve keys from depot instead of manually importing via file
in
{ pkgs, config, lib, ... }: {

  users.extraUsers = {
    ghuntley = {
      isNormalUser = true;
      home = "/home/ghuntley";
      extraGroups = [
        "adbusers"
        "docker"
        "input"
        "keys"
        "kvm"
        "libvirtd"
        "networkmanager"
        "plugdev"
        "vboxusers"
        "wheel"
        "wireshark"
      ];
      shell = pkgs.fish;
      uid = 1000;
      openssh.authorizedKeys.keys = [ keys.users.ghuntley.ssh ];
    };

    root.openssh.authorizedKeys.keys = [ keys.users.ghuntley.ssh ];
  };

  sops.secrets.yubico-challenge-10158360-ghuntley = {
    format = "binary";
    sopsFile = ../secrets/yubico-challenge-10158360;
    path = "/home/ghuntley/.yubico/challenge-10158360";
    owner = "ghuntley";
    group = "users";
    mode = "0440";
  };

  sops.secrets.yubico-challenge-10158360-root = {
    format = "binary";
    sopsFile = ../secrets/yubico-challenge-10158360;
    path = "/root/.yubico/challenge-10158360";
    owner = "root";
    group = "users";
    mode = "0440";
  };

  sops.secrets.yubico-challenge-7029292-ghuntley = {
    format = "binary";
    sopsFile = ../secrets/yubico-challenge-7029292;
    path = "/home/ghuntley/.yubico/challenge-7029292";
    owner = "ghuntley";
    group = "users";
    mode = "0440";
  };

  sops.secrets.yubikey-challenge-7029292-root = {
    format = "binary";
    sopsFile = ../secrets/yubico-challenge-7029292;
    path = "/root/.yubico/challenge-7029292";
    owner = "root";
    group = "users";
    mode = "0440";
  };

}
