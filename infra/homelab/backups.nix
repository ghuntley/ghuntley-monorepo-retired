# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda"; 

  networking.hostName = "backups";

  time.timeZone = "UTC";

  networking.useDHCP = false;
  networking.interfaces.ens18.useDHCP = true;

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  users.users.ghuntley = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  environment.systemPackages = with pkgs; [
    vim
    rclone restic
    getmail
  ];


  services.restic.backups = {
    b2 = {
      user = "root";
      repository = "rclone:b2-gmail:ghuntley-gmail";
      passwordFile = "/etc/resticPasswd";
      extraBackupArgs = [ "" ];
      paths = [
        "/home/ghuntley/mail"
      ];
      timerConfig = {
        onCalendar = "hourly";
      };
    };
  };

  systemd.services.prunebackups = {
    serviceConfig.User = "root";
    serviceConfig.Type = "oneshot";

    path = [
      pkgs.restic
      pkgs.rclone
    ];

    script = ''
      ${pkgs.restic}/bin/restic unlock
      ${pkgs.restic}/bin/restic forget --keep-hourly 48 --keep-daily 7 --keep-weekly 8 --keep-monthly 6 -r rclone:b2-gmail:ghuntley-gmail --password-file /etc/resticPasswd
      ${pkgs.restic}/bin/restic prune -r rclone:b2-gmail:ghuntley-gmail --password-file /etc/resticPasswd
    '';
  };

  systemd.timers.prunebackups = {
      wantedBy = [ "timers.target" ];
      partOf = [ "prunebackups.service" ];
      timerConfig.OnCalendar = "daily";
  };

  services.cron.enable = true;

  services.tailscale.enable = true;
  services.openssh.enable = true;
  programs.mosh.enable = true;

  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}

