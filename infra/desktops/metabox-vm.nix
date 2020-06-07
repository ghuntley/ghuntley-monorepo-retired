{ config, pkgs, lib, ... }: {
  imports = [
    <home-manager/nixos>
    ./modules/vm.nix
    ./modules/sys/bluetooth
    ./modules/apps/steam
  ];

  # High-DPI console
  console.font =
    lib.mkDefault "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";

  networking.hostName = "metabox";
  networking.useDHCP = true;
  networking.interfaces.enp109s0.useDHCP = true;
  networking.interfaces.wlp112s0.useDHCP = true;

  nix.maxJobs = 8;

  home-manager.users.ghuntley.xresources.properties = { "Xft.dpi" = 192; };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostId = "DEADBEEF";
  boot.supportedFilesystems = [ "zfs" ];
  boot.zfs.requestEncryptionCredentials = true;
  boot.zfs.devNodes = "/dev/";

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ahci"
    "nvme"
    "usbhid"
    "usb_storage"
    "sd_mod"
    "sr_mod"
    "sdhci_pci"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "tank/root";
    fsType = "zfs";
  };

  fileSystems."/etc" = {
    device = "tank/etc";
    fsType = "zfs";
  };

  fileSystems."/nix" = {
    device = "tank/nix";
    fsType = "zfs";
  };

  fileSystems."/tmp" = {
    device = "tank/tmp";
    fsType = "zfs";
  };

  fileSystems."/home" = {
    device = "tank/home";
    fsType = "zfs";
  };

  fileSystems."/srv" = {
    device = "tank/srv";
    fsType = "zfs";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/BA52-4F22";
    fsType = "vfat";
  };

  swapDevices = [ ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  services.xserver.videoDrivers = [ "nvidia" ];

}
