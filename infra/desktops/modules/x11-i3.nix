# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {
  environment.systemPackages = with pkgs; [
  ];

  services.xserver = {
    enable = true;
    libinput.enable = true;

    # displayManager.defaultSession = "plasma5";
    # desktopManager.plasma5.enable = true;

    desktopManager = {
      xterm.enable = false;
      xfce = {
        enable = true;
        noDesktop = true;
        enableXfwm = false;
      };
    };

    displayManager = {
      autoLogin = {
        user = "ghuntley";
        enable = true;
      };

      defaultSession = "none+i3";

      lightdm.enable = true;
    };

    windowManager.i3.enable = true;
  };

  services.xserver.windowManager.i3.package = pkgs.i3-gaps;

  services.compton = {
    enable = true;
    shadow = true;
    inactiveOpacity = 1.0;
  };

}
