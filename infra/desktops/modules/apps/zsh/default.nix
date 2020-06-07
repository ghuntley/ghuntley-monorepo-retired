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
        HYPHEN_INSENSITIVE="true"
        EDITOR = "${pkgs.neovim}/bin/nvim";
        PAGER = "${pkgs.most}/bin/most";
        WORDCHARS=

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

