# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, ... }: {
  home-manager.users.ghuntley = {

    programs.git = {
      enable = true;
      userName = "Geoffrey Huntley";
      userEmail = "ghuntley@ghuntley.com";
      extraConfig = {
        merge.conflictstyle = "diff3";
        pull.rebase = true;
        rebase.autosquash = true;
        rebase.autostash = true;
        color.ui = true;
      };

      aliases = {
        unstage = "reset HEAD --";
        last = "log -1 HEAD";
        ls = ''
          log --graph --decorate --pretty=format:\"%C(yellow)%h%C(red)%d %C(reset)%s %C(blue)[%cn]\"'';
        cp = "cherry-pick";
        sh = "show --word-diff";
        ci = "commit";
        dc = "diff --cached";
        wd = "diff --word-diff";
        ll = ''
          log --pretty=format:\"%h%C(reset)%C(red) %d %C(bold green)%s%C(reset)%Cblue [%cn] %C(green) %ad\" --decorate --numstat --date=iso'';
        nc = ''commit -a --allow-empty-message -m \"\"'';
        cr = "commit -C HEAD@{1}";
      };
    };

  };
}

