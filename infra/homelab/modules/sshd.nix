{
  imports = [
    ./fail2ban.nix
    ./mosh.nix
    ./ssh.nix
  ];

  services.openssh = {
    enable = true;
    forwardX11 = true;
    passwordAuthentication = false;
    permitRootLogin = "no";
    useDns = false;
    # unbind gnupg sockets if they exists
    extraConfig = ''
      StreamLocalBindUnlink yes
    '';
  };

  # Allow sudo-ing via the forwarded SSH agent.
  security.pam.enableSSHAgentAuth = true;
}
