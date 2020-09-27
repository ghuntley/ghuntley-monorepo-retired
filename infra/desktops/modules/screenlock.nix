# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {

  environment.systemPackages = with pkgs; [ xautolock keychain ];

  services.physlock = {
    allowAnyUser = true;
    enable = true;
    disableSysRq = true;
    lockOn = {
      hibernate = true;
      suspend = true;
      
      # start x11 and associated programs in background but require password to unlock workstation
      extraTargets = [ "display-manager.service" ];
    };
    muteKernelMessages = true;
  };

}

