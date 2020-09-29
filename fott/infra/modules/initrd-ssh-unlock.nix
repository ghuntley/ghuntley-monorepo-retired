# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary
let keys = import ../../../keys; # TODO(high): retrieve keys from depot instead of manually importing via file
in
{ pkgs, config, lib, ... }: {

  boot.initrd.network = {
    enable = true;
    ssh = {
      enable = true;
      port = 22;
      hostECDSAKey = /run/keys/initrd-ssh-key;
      authorizedKeys = [ keys.users.mgmt.ssh ];
    };
  };

}
