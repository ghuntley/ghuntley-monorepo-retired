{ pkgs, ... }: {
  imports = [
    ./alacritty.nix
    ./firefox.nix
    ./git.nix
    ./shell.nix
    ./tmux.nix
    ./vscode.nix
  ];

  home-manager.users.ghuntley = {

    services = { gpg-agent.enable = true; };

  };
}
