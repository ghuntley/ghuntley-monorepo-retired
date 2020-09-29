# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ config, pkgs, sops, ... }: {
  imports =
    [
      <sops-nix/modules/sops>
      ./modules/boot.nix
      ./modules/entropy.nix
      ./modules/fail2ban.nix
      ./modules/firewall.nix
      ./modules/i18n.nix
      ./modules/initrd-ssh-unlock.nix
      ./modules/libvirtd.nix
      ./modules/logging.nix
      ./modules/microcode.nix
      ./modules/mosh.nix
      ./modules/neovim.nix
      ./modules/networking-ipv4.nix
      ./modules/networking-ipv6.nix
      ./modules/nix-daemon.nix
      ./modules/nvme.nix
      ./modules/pkgs.nix
      ./modules/powermanagement.nix
      ./modules/prometheus.nix
      ./modules/shell.nix
      ./modules/smartd.nix
      ./modules/smartmon.sh
      ./modules/sops.nix
      ./modules/sshd.nix
      ./modules/ssh.nix
      ./modules/sudo.nix
      ./modules/sysctl.nix
      ./modules/tailscale.nix
      ./modules/telegraf.nix
      ./modules/timesyncd.nix
      ./modules/timezone.nix
      ./modules/users.nix
      ./modules/xfs.nix
      ./modules/yubikey.nix
    ];

  system.stateVersion = "20.03";
}
