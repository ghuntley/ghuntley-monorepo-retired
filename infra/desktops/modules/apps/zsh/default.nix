# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ config, pkgs, lib, ... }: {

  programs = {
    zsh = {
      enable = true;
      enableCompletion = true;
      histSize = 100000;
      promptInit = ''
        autoload -U promptinit && promptinit && prompt suse && setopt prompt_sp
      '';
      interactiveShellInit = ''
        export HYPHEN_INSENSITIVE="true"
        export EDITOR="${pkgs.neovim}/bin/nvim";
        export PAGER="${pkgs.most}/bin/most";
        export WORDCHARS=

        alias vi='nvim'
        alias vim='nvim'
      '';
      setOptions = [
        "hist_ignore_dups"
        "share_history"
        "hist_fcntl_lock"
        "auto_cd"
        "extended_glob"
      ];
    };

  };
}
