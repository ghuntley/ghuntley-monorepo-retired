# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, ... }: {

  services.openssh.enable = true;
  services.openssh.passwordAuthentication = true;

  users.users.mgmt = {
    extraGroups = [ "wheel" ];
    isNormalUser = true;
    password = "hunter2";
  };
}
