# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ config, lib, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [ gpsd ];

  services.gpsd = {
    enable = true;
    nowait = true;
  };
}
