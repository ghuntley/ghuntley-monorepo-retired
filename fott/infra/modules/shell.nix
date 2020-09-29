# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {

  environment.systemPackages = with pkgs; [ bash bat ];

  programs.fish = {
    enable = true;
    shellAliases = {
      "cat" = "${pkgs.bat}/bin/bat";
    };
  };

}
