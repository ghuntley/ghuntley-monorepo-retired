# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {
  services.timesyncd.enable = true;

  networking.timeServers = [
    "0.au.pool.ntp.org"
    "1.au.pool.ntp.org"
    "2.au.pool.ntp.org"
    "3.au.pool.ntp.org"
  ];
}
