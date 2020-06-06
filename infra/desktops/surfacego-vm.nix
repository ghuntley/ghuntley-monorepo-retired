{ config, pkgs, lib, ... }: {
  imports = [ ./modules/vm.nix ];

  networking.hostName = "surface-nixos";

  nix.maxJobs = 4;

  home-manager.users.ghuntley.xresources.properties = { "Xft.dpi" = 144; };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/9673761a-caf2-4329-ba53-0a1a883a1228";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/0328-BCA3";
    fsType = "vfat";
  };

  swapDevices = [ ];
}
