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
        "plugdev"
        "vboxusers"
        "wheel"
        "wireshark"
      ];
      shell = "/run/current-system/sw/bin/zsh";
      uid = 1000;
      openssh.authorizedKeys.keys = [ keys.users.ghuntley.ssh ];
    };

    root.openssh.authorizedKeys.keys = [ keys.users.ghuntley.ssh ];
  };

}
