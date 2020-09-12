{ pkgs, config, lib, ... }: {

  boot.initrd.supportedFilesystems = [ "zfs" ];
  boot.supportedFilesystems = [ "zfs" ];

  services.zfs = {
    autoScrub = {
      enable = true;
      interval = "weekly";
    };
    autoSnapshot = {
      enable = true;
      frequent = 32; # 4 hours.
      daily = 14;
      weekly = 0;
      monthly = 0;
    };
    trim.enable = true;
  };
}
