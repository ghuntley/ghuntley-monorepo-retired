# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ config, lib, pkgs, ... }:

{

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  boot.initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "ehci_pci" "ahci" "vmw_pvscsi" "sd_mod" "sr_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    {
      device = "/dev/sda1";
      fsType = "ext4";
    };

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 1;

  virtualisation.vmware.guest.enable = true;

  networking = {
    hostName = "proxy.ghuntley.net";
    defaultGateway = "10.10.10.254";
    nameservers = [ "10.10.10.1" ];

    # lan
    interfaces.ens192 = {
      useDHCP = false;
      ipv4.addresses = [{
        address = "10.10.10.2";
        prefixLength = 24;
      }];
    };

    # guest
    interfaces.ens224 = {
      useDHCP = false;
      ipv4.addresses = [{
        address = "10.10.20.2";
        prefixLength = 24;
      }];
    };

    # hotspot
    interfaces.ens256 = {
      useDHCP = false;
      ipv4.addresses = [{
        address = "10.10.30.2";
        prefixLength = 24;
      }];
    };

    # hotspot
    interfaces.ens161 = {
      useDHCP = false;
      ipv4.addresses = [{
        address = "10.10.254.2";
        prefixLength = 24;
      }];
    };
  };

  # Configure network proxy if necessary
  #networking.proxy.default = "http://127.0.0.1:3128";
  #networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  # Set your time zone.
  time.timeZone = "Australia/Brisbane";

  # Packages installed in system profile. 
  environment.systemPackages = with pkgs; [
    htop
    tmux
    vim
    tailscale
  ];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.tailscale.enable = true;

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 22 80 443 8081 3128 ];

  services.squid = {
    enable = true;
    configText =
      ''
        acl localnet src 10.10.10.0/24  # lan
        acl guestnet src 10.0.20.0/24   # guest network
        acl hotspotnet src 10.0.30.0/24 # guest network
        acl SSL_ports port 443          # https
        acl SSL_ports port 4070         # https spotify - https://community.spotify.com/t5/Desktop-Windows/Proxy-not-working-since-1-0-83-318/td-p/4513939/page/9
        acl Safe_ports port 80          # http
        acl Safe_ports port 21          # ftp
        acl Safe_ports port 443         # https
        acl Safe_ports port 70          # gopher
        acl Safe_ports port 210         # wais
        acl Safe_ports port 1025-65535  # unregistered ports
        acl Safe_ports port 280         # http-mgmt
        acl Safe_ports port 488         # gss-http
        acl Safe_ports port 591         # filemaker
        acl Safe_ports port 777         # multiling http
        acl CONNECT method CONNECT

        # Deny requests to certain unsafe ports
        http_access deny !Safe_ports

        # Deny CONNECT to other than secure SSL ports
        http_access deny CONNECT !SSL_ports

        # https://wiki.squid-cache.org/Features/CacheManager
        http_access allow localnet manager
        http_access allow localhost manager
        http_access deny manager

        # Protect innocent web applications running on the proxy server who think the only
        # one who can access services on "localhost" is a local user
        http_access deny to_localhost

        # Application logs to syslog, access and store logs have specific files
        cache_log       syslog
        access_log      stdio:/var/log/squid/access.log
        cache_store_log stdio:/var/log/squid/store.log

        # Required by systemd service
        pid_filename    /run/squid.pid

        # Run as user and group squid
        cache_effective_user squid squid

        # Example rule allowing access from your local networks.
        # Adapt localnet in the ACL section to list your (internal) IP networks
        # from where browsing should be allowed
        http_access allow localnet
        http_access allow localhost

        # And finally deny all other access to this proxy
        http_access deny all

        # Squid normally listens to port 3128
        http_port 3128

        # Leave coredumps in the first cache dir
        coredump_dir /var/cache/squid

        ftp_user anonymous@example.com
        always_direct allow all

        cache_mem 3072 MB
        maximum_object_size 10240 MB
        maximum_object_size_in_memory 8 MB

        cache_dir aufs /srv/squid 256000 16 256
        cache_replacement_policy heap LFUDA

        # https://www.midnightfreddie.com/using-squid-to-cache-apt-updates-for-debian-and-ubuntu.html 
        refresh_pattern Packages\.bz2$ 0       20%     4320 refresh-ims
        refresh_pattern Sources\.bz2$  0       20%     4320 refresh-ims
        refresh_pattern Release\.gpg$  0       20%     4320 refresh-ims
        refresh_pattern Release$       0       20%     4320 refresh-ims
        refresh_pattern -i .deb$ 129600 100% 129600 refresh-ims override-expire

        # https://wiki.squid-cache.org/SquidFaq/WindowsUpdate
        refresh_pattern -i microsoft.com/.*\.(cab|exe|ms[i|u|f]|[ap]sf|wm[v|a]|dat|zip) 4320 80% 43200 reload-into-ims
        refresh_pattern -i windowsupdate.com/.*\.(cab|exe|ms[i|u|f]|[ap]sf|wm[v|a]|dat|zip) 4320 80% 43200 reload-into-ims
        refresh_pattern -i windows.com/.*\.(cab|exe|ms[i|u|f]|[ap]sf|wm[v|a]|dat|zip) 4320 80% 43200 reload-into-ims

        # https://gist.github.com/hvrauhal/f98d7811f19ad1792210
        refresh_pattern registry.npmjs.org 900 20% 4320 ignore-auth ignore-private ignore-no-cache ignore-reload override-expire

        # nuget
        refresh_pattern api.nuget.org 900 20% 4320 ignore-auth ignore-private ignore-no-cache ignore-reload override-expire

        # haskell
        refresh_pattern hackage.haskell.org 900 20% 4320 ignore-auth ignore-private ignore-no-cache ignore-reload override-expire

        # https://nixos.wiki/wiki/FAQ/Private_Cache_Proxy
        refresh_pattern -i nix-cache-info$ 0       20%     4320 refresh-ims
        refresh_pattern -i cache.nixos.org/nar.* 129600 100% 129600 refresh-ims override-expire

        #
        # Add any of your own refresh_pattern entries above these.
        #
        refresh_pattern ^ftp:           1440    20%     10080
        refresh_pattern ^gopher:        1440    0%      1440
        refresh_pattern -i (/cgi-bin/|\?) 0     0%      0
        refresh_pattern .               0       20%     4320
      '';

  };

  services.nginx = {
    enable = true;

    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;

    commonHttpConfig = ''
      add_header 'application/x-ns-proxy-autoconfig' '.dat';
    '';

    virtualHosts."wpad" = {
      addSSL = false;
      enableACME = false;
      root = "/srv/www/wpad";
    };

    virtualHosts."wpad.ghuntley.net" = {
      addSSL = false;
      enableACME = false;
      root = "/srv/www/wpad";
    };
  };

  users.users.mgmt = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable âsudoâ for the user.
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?

}
