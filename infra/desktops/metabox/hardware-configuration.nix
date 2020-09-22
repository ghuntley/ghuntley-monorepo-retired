# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ config, lib, pkgs, ... }:

{

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod" "sdhci_pci" ];
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

  fileSystems."/tmp" =
    {
      device = "tank/root/tmp";
      fsType = "zfs";
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/CA50-E9CD";
      fsType = "vfat";
    };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/9a476a58-94fb-4491-ac36-9eaa1e2bb050"; }];
}
