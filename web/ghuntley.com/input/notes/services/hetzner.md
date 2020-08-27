---
title: Hetzner
layout: notes
---

Hetzner has all of the more popular distributions available for installation via PXE but I prefer NixOS. Here are my notes on installing NixOS on a Hetzner bare metal server with ZFS encryption (via LUKS) manually without any form of automation (ie nixops or terraform). 

Most of this is scrambled from the following pages:
* [Installation of NixOS with encrypted root](https://gist.github.com/martijnvermaat/76f2e24d0239470dd71050358b4d5134)
* [NixOS with ZFS on LUKS](https://github.com/ghuntley/nixos-iso)
* [nixos from ubuntu](https://gist.github.com/chris-martin/4ead9b0acbd2e3ce084576ee06961000)

# Preperation
* Log a support ticket requesting that the engineers burn NixOS (send them a link to the ISO) to a USB key and ask for them to also connect a KVM. Once you get KVM login details you'll be able intercept the boot process - typically via F11.
* Run `memtest86+` to ensure that your hardware is on point.
* Boot nixos when `memtest86+` passes

The remote KVM is very sensitive to internet latency and highly unoptimised. If you experience problems with repeated keyboard strokes then provision a jumpbox in germany (ie azure) and use the KVM via RDP.

The url to the KVM  in the email you receive uses http *do not use it* use https instead otherwise your keystrokes are being sent out onto the internet unencrypted (ie your luks passwords will leak)

# Installation

Edit `/etc/nixos/configuration.nix` and add

```nix
boot.supportedFilesystems = [ "zfs" ];
```

Activate the changes and install the zfs kernel module/userland tools by running

```
nixos-rebuild switch
```

Create a 500MB boot partition (`/dev/nvme0n1`) and the rest will be our LUKS encrypted physical volume (`/dev/nvme0n1p2`).

```
gdisk /dev/nvme0n1

    o (create new empty partition table)
    n (add partition, 500M, type ef02 BIOS Boot Partition)
    n (add partition, remaining space, type 8300 Linux LVM)
    w (write partition table and exit)
```

Setup the encrypted LUKS partition and open it:

```
cryptsetup luksFormat /dev/nvme0n1p2
cryptsetup luksOpen /dev/nvme0n1p2 nvme0n1p2-enc
```

Create a 500MB boot partition (`/dev/nvme1n1`) and the rest will be our LUKS encrypted physical volume (`/dev/nvme1n1p2`).

```
gdisk /dev/nvme1n1

    o (create new empty partition table)
    n (add partition, 500M, type ef02 BIOS Boot Partition)
    n (add partition, remaining space, type 8300 Linux LVM)
    w (write partition table and exit)
```

Setup the encrypted LUKS partition and open it:

```
cryptsetup luksFormat /dev/nvme1n1p2
cryptsetup luksOpen /dev/nvme1n1p2 nvme1n1p2-enc
```

Create a mirrored zfs pool that uses both LUKS partitions

```
# -O atime=on         #
# -O relatime=on      # only write access time (requires atime, see man zfs)
# -O compression=lz4  # compress all the things! it makes things faster wierd huh (man zfs) 
# -O snapdir=visible  # ever so slightly easier snap management (man zfs)
# -O xattr=sa         # selinux file permissions (man zfs)
# -o ashift=12        # 4k blocks as alignment is extremely important if you use spinning rust disks (man zpool)
# -o altroot=/mnt     # temp mount during install (man zpool)

zpool create                \
  -O atime=on               \
  -O relatime=on            \
  -O compression=lz4        \
  -O snapdir=visible        \
  -O xattr=sa               \
  -o ashift=12              \
  -o altroot=/mnt           \
  tank                      \
  mirror
  /dev/mapper/nvme0n1p2-enc \
  /dev/mapper/nvme1n1p2-enc
```

Create the filesystem datasets

```
zfs create -o mountpoint=legacy tank/root
zfs create -o mountpoint=legacy tank/etc
zfs create -o mountpoint=legacy tank/nix
zfs create -o mountpoint=legacy tank/tmp
zfs create -o mountpoint=legacy tank/home
zfs create -o mountpoint=legacy tank/srv
```

Create the dataset for swap
```
zfs create -o compression=off -V 8G tank/swap
mkswap -L SWAP /dev/zvol/tank/swap
swapon /dev/zvol/tank/swap
```

Mount the datasets at /mnt

```
mount -t zfs tank/root /mnt/root
mount -t zfs tank/etc /mnt/etc
mount -t zfs tank/tmp /mnt/tmp
chmod a+rwx /mnt/tmp
mount -t zfs tank/nix /mnt/nix
mount -t zfs tank/home /mnt/home
mount -t zfs tank/srv /mnt/srv
```

Since zfs is a copy-on-write filesystem even for deleting files disk space is needed. Therefore it should be avoided to run out of disk space. Luckily it is possible to reserve disk space for datasets to prevent this. 

```
zfs set reservation=1G tank
```

Configure ZFS snapshots

```
zfs set com.sun:auto-snapshot=true tank
zfs set com.sun:auto-snapshot=true tank/etc
zfs set com.sun:auto-snapshot=true tank/home
zfs set com.sun:auto-snapshot:monthly=false tank/home
zfs set com.sun:auto-snapshot=true tank/srv

zfs set com.sun:auto-snapshot=false tank/nix # its pointless to snapshot this as it's automatically generated mutable state 
zfs set com.sun:auto-snapshot=false tank/tmp
```

Generate a baseline configuration for nixos

```
nixos-generate-config --root /mnt
```

Customise `/mnt/nixos/*.nix` as per instructions below and then commence installation

```
nixos-install --root /mnt
```
# Configuration

## configuration.nix

```nix
# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.supportedFilesystems = ["zfs" ];
  boot.kernelParams = ["zfs_force=1"]; # this can be removed after initial boot
  boot.zfs.forceImportRoot = false;
  boot.zfs.forceImportAll = false;

  # required by ZFS - see https://nixos.org/nixos/options.html#networking.hostid
  networking.hostId = "cafebabe"; # you must customise this on per system basis

  boot.initrd.luks.devices = [
    {
      name = "nvme0n1p2-enc";
      device = "/dev/nvme0n1p2";
      preLVM = true;
      allowDiscards = true;
    }
    {
      name = "nvme1n1p2-enc";
      device = "/dev/nvme1n1p2";
      preLVM = true;
      allowDiscards = true;
   }
  ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  #boot.loader.grub.efiSupport = true;
  boot.loader.grub.devices = ["/dev/nvme0n1" "/dev/nvme1n1"];
  boot.loader.grub.enableCryptodisk = true;
  boot.loader.grub.memtest86.enable = true;

  #could not get this working so I used grub embedding into the bios partitions /dev/nvme[0|1]n1
  #boot.loader.grub.mirroredBoots = [
  #  { devices = "/dev/nvme0n1"; path = "/boot0"; efiSysMountPoint = "/boot0"; }
  #  { devices = "/dev/nvme1n1"; path = "/boot1"; efiSysMountPoint = "/boot1"; }
  #];
									
  boot.loader.grub.version = 2;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos"; # Define your hostname.
  
  environment.systemPackages = with pkgs; [
    wget vim tmux
  ];

  services.openssh.enable = true;

  services.zfs.autoSnapshot = {
   enable = true;
   flags = "-k -p --utc";
  };

  services.zfs.autoScrub.enable = true;

  users.extraUsers.ghuntley = {
    isNormalUser = true;
    uid = 1000;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?

}
```
## hardware-configuration.nix

```nix
# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "usbhid" "sd_mod" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "tank/root";
      fsType = "zfs";
    };

  fileSystems."/etc" =
    { device = "tank/etc";
      fsType = "zfs";
    };

  fileSystems."/nix" =
    { device = "tank/nix";
      fsType = "zfs";
    };

  fileSystems."/tmp" =
    { device = "tank/tmp";
      fsType = "zfs";
    };

  fileSystems."/home" =
    { device = "tank/home";
      fsType = "zfs";
    };

  fileSystems."/srv" =
    { device = "tank/srv";
      fsType = "zfs";
    };

  # swap disabled for k8
  # swapDevices = [ { device = "/dev/zd0"; } ];

  nix.maxJobs = lib.mkDefault 12; # EX61-NVMe
}
```
