# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, ... }: {
  imports = [ ./alacritty.nix ./firefox.nix ./git.nix ./shell.nix ./tmux.nix ];

  home-manager.users.ghuntley = {

    services = { gpg-agent.enable = true; };

  };
}
