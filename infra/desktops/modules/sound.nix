# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {

  environment.systemPackages = with pkgs; [
    pulseaudio-ctl
  ];

  sound.enable = true;
  hardware.pulseaudio.enable = true;
}
