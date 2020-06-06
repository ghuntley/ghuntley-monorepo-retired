{ pkgs, ... }: {
  home-manager.users.ghuntley = {

    programs.alacritty = {
      enable = true;

      settings = {
        font.normal.family = "Source Code Pro";
        font.bold.family = "Source Code Pro";
        font.italic.family = "Source Code Pro";
      };

    };

  };
}
