# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {
  environment.systemPackages = with pkgs; [ bat fzf ];

  programs.bash.enableCompletion = true;

  programs.fish = {
    enable = true;
    shellAliases = {
      "cat" = "${pkgs.bat}/bin/bat";
      "mutt" = "${pkgs.neomutt}/bin/neomutt";
    };
  };
}

