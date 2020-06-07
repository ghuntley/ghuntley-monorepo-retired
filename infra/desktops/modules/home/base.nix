{ pkgs, ... }: {
  imports = [ ./alacritty.nix ./firefox.nix ./git.nix ./shell.nix ./tmux.nix ];

  home-manager.users.ghuntley = {

    services = { gpg-agent.enable = true; };

  };
}
