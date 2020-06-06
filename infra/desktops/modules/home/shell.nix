{ pkgs, ... }: {
  home-manager.users.ghuntley = {

    programs = {

      direnv.enable = true;

      bash.enable = true;

      zsh = {
        enable = false;
        enableCompletion = true;
        shellAliases = { };
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

  };
}
