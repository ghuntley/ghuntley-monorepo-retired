{ pkgs, ... }: {
  home-manager.users.ghuntley = {

    programs.vscode = {
      enable = false;

      extensions = with pkgs.vscode-extensions; [ ];

      userSettings = { };

    };

  };
}
