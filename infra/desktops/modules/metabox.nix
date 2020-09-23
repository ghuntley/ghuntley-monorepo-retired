# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {

  # https://github.com/davidrohr/clevo-indicator/blob/master/src/clevo-indicator.c
  boot.kernelModules = [ "ec_sys write_support=1" ];

  hardware.tuxedo-keyboard = {
    enable = true;
  };

  boot.kernelParams = [
    "tuxedo_keyboard.mode=0"
    "tuxedo_keyboard.brightness=255"
    "tuxedo_keyboard.color_left=0xff0a0a"
  ];
}
