# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary
let keys = import ../../../keys; # TODO(high): retrieve keys from depot instead of manually importing via file
in
{ pkgs, config, lib, ... }: {

  users.extraUsers = {
    mgmt = {
      isNormalUser = true;
      home = "/home/mgmt";
      extraGroups = [
        "keys"
        "wheel"
      ];
      shell = pkgs.bash;
      uid = 1000;
      openssh.authorizedKeys.keys = [ keys.users.mgmt.ssh ];
    };

    root.openssh.authorizedKeys.keys = [ keys.users.mgmt.ssh ];
  };

  sops.secrets.yubico-challenge-10158360-mgmt = {
    format = "binary";
    sopsFile = ../secrets/yubico-challenge-10158360;
    path = "/home/mgmt/.yubico/challenge-10158360";
    owner = "mgmt";
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

  sops.secrets.yubico-challenge-7029292-mgmt = {
    format = "binary";
    sopsFile = ../secrets/yubico-challenge-7029292;
    path = "/home/mgmt/.yubico/challenge-7029292";
    owner = "mgmt";
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
