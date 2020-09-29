# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {

  environment.systemPackages = with pkgs; [ smartmontools ];

  services.smartd = {
    enable = true;
    autodetect = true;
    # SMART Automatic Offline Testing on startup, and schedules short self-tests daily, and long self-tests weekly.
    defaults.monitored = "-a -o on -s (S/../.././02|L/../../7/04)";
    notifications.wall.enable = true;
  };
};
