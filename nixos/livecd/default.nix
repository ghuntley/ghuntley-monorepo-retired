{ config, pkgs, ... }:
{
  imports = [
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>

    # Provide an initial copy of the NixOS channel so that the user
    # doesn't need to run "nix-channel --update" first.
    <nixpkgs/nixos/modules/installer/cd-dvd/channel.nix>
  ];

  services.openssh.enable = true;
  services.openssh.passwordAuthentication = true;

  systemd.services.sshd.wantedBy = pkgs.lib.mkForce [ "multi-user.target" ];

  users.extraUsers.root.password = "nixos";
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519
          AaAeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
          username@host"
  ];

  environment.systemPackages =  [
    pkgs.ack
    pkgs.cachix
    pkgs.curl
    pkgs.direnv
    pkgs.dos2unix
    pkgs.git-lfs
    pkgs.gitAndTools.gitFull
    pkgs.htop
    pkgs.iftop
    pkgs.inetutils
    pkgs.iotop
    pkgs.lsof
    pkgs.p7zip
    pkgs.rpl
    pkgs.tmux
    pkgs.tree
    pkgs.unzip
    pkgs.wget
    pkgs.zip
    #third_party.neovim
  ];


}
