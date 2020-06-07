{ config, pkgs, ... }:

{
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  boot.initrd.availableKernelModules =
    [ "ata_piix" "uhci_hcd" "ehci_pci" "ahci" "vmw_pvscsi" "sd_mod" "sr_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/30788035-0ec4-429f-ac03-1d75a2c10c59";
    fsType = "ext4";
  };

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 1;

  fileSystems."/mnt/downloads" = {
    device = "//10.0.10.10/downloads";
    fsType = "cifs";
    options = let
      # this line prevents hanging on network split
      automount_opts =
        "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s,dir_mode=0777,file_mode=0777";

    in [ "${automount_opts},credentials=/etc/nixos/smb-secrets" ];
  };

  fileSystems."/mnt/media" = {
    device = "//10.0.10.10/media";
    fsType = "cifs";
    options = let
      # this line prevents hanging on network split
      automount_opts =
        "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s,dir_mode=0777,file_mode=0777";

    in [ "${automount_opts},credentials=/etc/nixos/smb-secrets" ];
  };

  networking.hostName = "media";
  networking.useDHCP = false;
  networking.interfaces.ens192.useDHCP = true;

  time.timeZone = "Australia/Brisbane";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
    vim
    tmux
    htop
    chromaprint
    git
    ffmpeg
  ];

  services.openssh.enable = true;

  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  networking.firewall.enable = false;

  users.users.mgmt = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable "sudo" for the user.
  };

  virtualisation.vmware.guest.enable = true;
  virtualisation.docker.enable = true;

  services.cron.enable = true;
  services.cron.systemCronJobs = [ "0 22 * * * nixos-rebuild switch" ];

  services.nginx.enable = true;
  services.nginx.virtualHosts."media" = { root = "/var/www"; };

  services.nginx.virtualHosts."media.yurt" = { root = "/var/www"; };

  services.jackett.enable = true;

  services.radarr.enable = true;
  services.radarr.user = "root";
  services.radarr.group = "wheel";

  services.lidarr.enable = true;
  services.lidarr.user = "root";
  services.lidarr.group = "wheel";

  services.nzbget.enable = true;
  services.nzbget.user = "root";
  services.nzbget.group = "wheel";

  services.sonarr.enable = true;
  services.sonarr.user = "root";
  services.sonarr.group = "wheel";

  services.airsonic.enable = true;
  services.airsonic.listenAddress = "10.0.10.214";
  services.airsonic.maxMemory = 4096;

  #  services.subsonic.enable = true;
  #  services.subsonic.listenAddress = "10.0.10.214";
  #  services.subsonic.maxMemory = 4096;

  services.plex.enable = true;
  #services.plex.user = "root";
  #services.plex.group = "wheel";

  #services.transmission.enable = true;
  #services.transmission.settings = {
  #  download-dir = "/mnt/downloads/torrents/complete";
  #  incomplete-dir = "/mnt/downloads/torrents/incomplete/";
  #  incomplete-dir-enabled = true;
  #  rpc-whitelist = "127.0.0.1,10.0.10.*";
  #};
  #
  services.deluge.enable = true;
  services.deluge.web.enable = true;

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?

  nixpkgs.config.allowUnfree = true;
}

