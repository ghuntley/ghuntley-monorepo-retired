# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ config, pkgs, sops, ... }: {
  imports =
    [
      <sops-nix/modules/sops>
      ./hardware-configuration.nix
      ../modules/bluetooth.nix
      ../modules/boot.nix
      ../modules/corsair.nix
      ../modules/docker.nix
      ../modules/efi.nix
      ../modules/entropy.nix
      ../modules/fail2ban.nix
      ../modules/firewall.nix
      ../modules/gist.nix
      ../modules/hidpi.nix
      ../modules/i18n.nix
      ../modules/initrd.nix
      ../modules/libvirtd.nix
      ../modules/logging.nix
      ../modules/mbsyncd.nix
      ../modules/mosh.nix
      ../modules/setcap-wrapper.nix
      ../modules/microcode.nix
      ../modules/googlechrome.nix
      ../modules/nvme.nix
      ../modules/neovim.nix
      ../modules/networking-ipv6.nix
      ../modules/nix-daemon.nix
      ../modules/pkgs.nix
      ../modules/pkgs-x11.nix
      ../modules/printing.nix
      ../modules/powermanagement.nix
      ../modules/sourcegraph.nix
      ../modules/shell.nix
      ../modules/logitech.nix
      ../modules/sshd.nix
      ../modules/sound.nix
      ../modules/sops.nix
      ../modules/sudo.nix
      ../modules/nvidia.nix
      ../modules/steam.nix
      ../modules/sysctl.nix
      ../modules/tailscale.nix
      ../modules/telegraf.nix
      ../modules/timesyncd.nix
      ../modules/timezone.nix
      ../modules/users.nix
      ../modules/wireless.nix
      ../modules/x11-kde.nix
      ../modules/zfs.nix
    ];

  networking.hostId = "DEADBEEF";
  networking.hostName = "metabox"; # Define your hostname.

  networking.interfaces.enp109s0.useDHCP = true;
  networking.interfaces.wlp112s0.useDHCP = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
    vim
    htop
    tmux
    firefox
  ];

  system.stateVersion = "20.03";
}
