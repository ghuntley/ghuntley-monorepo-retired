# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ config, pkgs, lib, ... }:
{
  imports =
    [
      ./hardware-configuration.nix
      ../modules/bluetooth.nix
      ../modules/boot.nix
      ../modules/docker.nix
      ../modules/efi.nix
      ../modules/fail2ban.nix
      ../modules/firewall.nix
      ../modules/gist.nix
      ../modules/gpsd.nix
      ../modules/i18n.nix
      ../modules/initrd.nix
      ../modules/initrd-ssh-unlock-zfs.nix
      ../modules/libvirtd.nix
      ../modules/logging.nix
      ../modules/microcode.nix
      ../modules/mosh.nix
      ../modules/neovim.nix
      ../modules/networking-ipv6.nix
      ../modules/nix-daemon.nix
      ../modules/nvme.nix
      ../modules/pkgs.nix
      ../modules/prometheus.nix
      ../modules/screenlock.nix
      ../modules/shell.nix
      ../modules/smartd.nix
      ../modules/sops.nix
      ../modules/sshd.nix
      ../modules/sudo.nix
      ../modules/sysctl.nix
      ../modules/tailscale.nix
      ../modules/telegraf.nix
      ../modules/timesyncd.nix
      ../modules/timezone.nix
      ../modules/vmware-guest.nix
      ../modules/zfs.nix
      ../users/default.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "vanpute";
  networking.hostId = "DEADBEEF";

  networking.useDHCP = false;
  networking.defaultGateway = "10.10.10.254";
  networking.nameservers = [ "10.10.10.254" ];

  # virtual machines
  networking.interfaces.ens192 = {
    useDHCP = false;
    ipv4.addresses = [{ address = "10.10.10.250"; prefixLength = 24; }];
  };

  # telstra
  #networking.interfaces.ens192 = {
  #  useDHCP = false;
  #  ipv4.addresses = [{ address = "10.10.103.250"; prefixLength = 24; }];
  #};

  # optus
  #networking.interfaces.ens224 = {
  #  useDHCP = false;
  #  ipv4.addresses = [{ address = "10.10.104.250"; prefixLength = 24; }];
  #};

  # vodafone
  #networking.interfaces.ens256 = {
  #  useDHCP = false;
  #  ipv4.addresses = [{ address = "10.10.105.250"; prefixLength = 24; }];
  #};


  #networking.firewall.allowedTCPPorts = [ ];
  #networking.firewall.allowedUDPPorts = [ ];

  networking.firewall.interfaces."tailscale0".allowedTCPPorts = [
    80
    443
    8080
  ];

  virtualisation.oci-containers.containers = {
    code-server = {
      image = "codercom/code-server:latest";
      ports = [
        "0.0.0.0:8080:8080"
      ];
      volumes = [
        #
        # nb. careful this allows root access
        "/var/run/docker.sock:/var/run/docker.sock"
        # nb. careful this allows root access
        #
        "/srv/git:/home/coder/code"
      ];
      environment = {
        PASSWORD = "TODO";
      };
      cmd = [
        "--auth"
        "password"
      ];
    };
  };

  system.stateVersion = "20.03";
}
