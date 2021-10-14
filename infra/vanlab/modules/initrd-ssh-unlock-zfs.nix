# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

#let keys = import ../../../keys; # TODO(high): retrieve keys from depot instead of manually importing via file
#in
{ pkgs, config, lib, ... }: {
  #  boot.initrd.network = {
  #    enable = true;
  #    ssh = {
  #      enable = true;
  #      port = 2222;
  #      authorizedKeys = [ keys.users.mgmt.ssh ];
  #    };
  #    # this will automatically load the zfs password prompt on login
  #    # and kill the other prompt so boot can continue
  #    postCommands = ''
  #      echo "zfs load-key -a; killall zfs" >> /root/.profile
  #    '';
  #  };
}
