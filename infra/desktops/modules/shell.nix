# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {
  environment.systemPackages = with pkgs; [ bat fzf ];

  programs.bash.enableCompletion = true;

  programs.zsh = {
    enable = true;
    ohMyZsh = {
      enable = true;
      theme = "lambda";
      plugins = [
        "battery"
        "colorize"
        "direnv"
        "docker"
        "command-not-found"
        "git"
        "git-prompt"
        "github"
        "gitignore"
        "last-working-dir"
        "history"
        "kubectl"
        "postgres"
        "ssh-agent"
        "systemd"
        "themes"
        "tmux"
      ];
    };
    shellAliases = {
      "cat" = "${pkgs.bat}/bin/bat";
      "mutt" = "${pkgs.neomutt}/bin/neomutt";
    };
  };
}
