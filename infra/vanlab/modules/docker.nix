# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {
  virtualisation.docker = {
    enable = true;
    autoPrune = {
      dates = "daily";
      enable = true;
    };
  };
}
