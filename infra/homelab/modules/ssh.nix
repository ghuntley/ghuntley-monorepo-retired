{ config, lib, pkgs, ... }:
{
  programs.ssh.knownHosts.ssh-ca = {
    certAuthority = true;
    hostNames = [
      "*.ghuntley.net"
    ];
    publicKeyFile = ./ssh-ca.pub;
  };
}
