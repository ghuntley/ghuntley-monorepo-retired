# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{
  imports = [
    ./fail2ban.nix
    ./mosh.nix
    ./ssh.nix
  ];

  services.openssh = {
    enable = true;
    forwardX11 = true;
    openFirewall = false;
    passwordAuthentication = false;
    permitRootLogin = "no";
    useDns = false;
    # unbind gnupg sockets if they exists
    extraConfig = ''
      StreamLocalBindUnlink yes
    '';
  };

  networking.firewall.interfaces."tailscale0".allowedTCPPorts = [ 22 ];

  # Allow sudo-ing via the forwarded SSH agent.
  # security.pam.enableSSHAgentAuth = true;
}
