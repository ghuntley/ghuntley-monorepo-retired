# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ config, lib, pkgs, ... }:

{
  boot.initrd.availableKernelModules = [ "ata_piix" "vmw_pvscsi" "xhci_pci" "sd_mod" "sr_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    {
      device = "tank/root";
      fsType = "zfs";
    };

  fileSystems."/etc" =
    {
      device = "tank/root/etc";
      fsType = "zfs";
    };

  fileSystems."/home" =
    {
      device = "tank/root/home";
      fsType = "zfs";
    };

  fileSystems."/nix" =
    {
      device = "tank/root/nix";
      fsType = "zfs";
    };

  fileSystems."/srv" =
    {
      device = "tank/srv";
      fsType = "zfs";
    };

  fileSystems."/srv/git" =
    {
      device = "tank/srv/git";
      fsType = "zfs";
    };

  fileSystems."/boot" =
    {
      device = "/dev/sda1";
      fsType = "vfat";
    };

  swapDevices = [ ];
}
