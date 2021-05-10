## Partitioning

Create a 500MB EFI boot partition (`/dev/vda`) and the rest will be our LUKS encrypted physical volume for LVM (`/dev/vda`).

    $ fdisk /dev/sda

- create new empty partition table
- add partition, 500M, type 82 Linux
- add partition, remaining space, type 8E Linux LVM
- write partition table and exit

Setup the encrypted LUKS partition and open it:

    $ cryptsetup luksFormat /dev/vda2
    $ cryptsetup luksOpen /dev/vda2 enc-pv

We create two logical volumes, a 8GB swap parition and the rest will be our rootvg
 filesystem

    $ pvcreate /dev/mapper/enc-pv
    $ vgcreate rootvg /dev/mapper/enc-pv
    $ lvcreate -L 8G -n swap rootvg
    $ lvcreate -l '100%FREE' -n nixos rootvg

Format the partitions:

    $ mkfs.fat /dev/vda1
    $ mkfs.ext4 -L nixos /dev/rootvg/nixos

    $ mkswap -L swap /dev/rootvg/swap


## Installing NixOS

We mount the partitions we just created under `/mnt` so we can install NixOS on them.

    $ mount /dev/rootvg/nixos /mnt
    $ mkdir /mnt/boot
    $ mount /dev/vda1 /mnt/boot
    $ swapon /dev/rootvg/swap

## Configuration

```nix
  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "nodev";
  boot.loader.grub.efiSupport = true;
  boot.loader.efi.canTouchEfiVariables = true;

    boot.initrd.luks.devices = {
      nixos = {
        device = "/dev/disk/by-uuid/"; # blkid /dev/vda2
        preLVM = true;
        allowDiscards = true;
    };
  };

## See also
- https://gist.github.com/martijnvermaat/76f2e24d0239470dd71050358b4d5134
- https://nixos.wiki/wiki/Full_Disk_Encryption
- https://nixos.wiki/wiki/Remote_LUKS_Unlocking
- https://mth.st/blog/nixos-initrd-ssh/
