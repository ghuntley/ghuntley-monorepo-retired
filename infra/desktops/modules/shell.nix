# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {
  environment.systemPackages = with pkgs; [ bat fzf ];

  programs.bash.enableCompletion = true;

  programs.zsh = {
    enable = true;
    ohMyZsh = {
      enable = true;
      plugins = [
        "battery"
        "colorize"
        "command-not-found"
        "github"
        "gitignore"
        "postgres"
        "systemd"
        "themes"
      ];
    };
    shellAliases = {
      "cat" = "${pkgs.bat}/bin/bat";
    };
  };
}
