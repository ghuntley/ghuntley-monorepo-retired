{ pkgs, ... }: {
  home-manager.users.ghuntley = {

    programs = {

      direnv.enable = true;

      bash.enable = true;

      zsh = {
        enable = false;
        enableCompletion = true;
        shellAliases = ''
          alias vi='nvim'
          alias vim='nvim'
        '';
        initExtra = ''
          export PATH=$HOME/.local/bin:$PATH
          HYPHEN_INSENSITIVE="true"
          WORDCHARS=

          eval $(${pkgs.coreutils}/bin/dircolors "${
            ./dircolors.ansi-universal
          }")

          # Fix tramp:
          if [[ "$TERM" == "dumb" ]]
          then
          unsetopt zle
          unsetopt prompt_cr
          unsetopt prompt_subst
          unfunction precmd
          unfunction preexec
          PS1='$ '
          fi
        '';
      };
    };

    home.sessionVariables = {
      EDITOR = "${pkgs.neovim}/bin/nvim";
      PAGER = "${pkgs.most}/bin/most";
    };

    nixpkgs.config.vim = { ftNixSupport = true; };

  };

  environment.interactiveShellInit = ''
    alias vi='nvim'
  '';

  environment.etc.vimrc = {
    text = ''
      " Use Vim settings, rather than Vi settings (much better!).
      " This must be first, because it changes other options as a side effect.
      set nocompatible

      " allow backspacing over everything in insert mode
      set backspace=indent,eol,start

      " keep 1024 lines of command line history
      set history=1024

      " use syntax highlighting if possible
      if has("syntax")
        syntax on
      endif

      " spaces not tabs
      set tabstop=2
      set shiftwidth=2
      set expandtab

      " show the cursor position all the time
      set ruler
    '';
  };

}
