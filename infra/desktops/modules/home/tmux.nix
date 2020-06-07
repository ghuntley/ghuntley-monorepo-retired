# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, ... }: {
  home-manager.users.ghuntley = {

    programs.tmux = {
      enable = true;
      baseIndex = 1;
      clock24 = true;
      disableConfirmationPrompt = true;
      shortcut = "a";
      newSession = true;
      terminal = "xterm-24bits";
      plugins = with pkgs.tmuxPlugins; [ resurrect ];
      extraConfig = ''
        bind C-a send-prefix
        set -g mouse on
        set -sa terminal-overrides ",xterm*:Tc"
      '';
    };

  };
}

