# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ config, pkgs, ... }:
let sources = import ../nix/sources.nix;
in {
  imports = [ <home-manager/nixos> ./base.nix ./home/base.nix ./home/xorg.nix ];

  config = {
    nixpkgs = { config = { packageOverrides = pkgs: rec { }; }; };

    fonts = { fonts = with pkgs; [ source-code-pro ]; };

    environment.systemPackages = with pkgs; [
      cabal2nix
      firefox
      most
      niv
      racket
      tcpdump
      xclip
    ];

    services = {
      dbus.packages = [ pkgs.gnome3.dconf ];

      samba = {
        enable = true;
        extraConfig = ''
          follow symlinks = yes
        '';
        # extraConfig = ''
        # workgroup = WORKGROUP
        # server string = "SURFACE-NIXOS"
        # netbios name = "SURFACE-NIXOS"
        #   guest account = nobody
        #   map to guest = bad user
        #   follow symlinks = yes
        # '';
        shares = {
          ghuntley = {
            comment = "Geoff's home";
            path = "/home/ghuntley";
            "valid users" = [ "ghuntley" ];
            public = "no";
            writable = "yes";
            browseable = "yes";
            printable = "no";
          };
        };
      };
    };

    networking = {
      enableIPv6 = true;
      #defaultGateway = "172.21.21.1";
      #nameservers = ["10.0.0.1" "8.8.8.8"];
      useDHCP = true;
      #interfaces = {
      #  eth0 = {
      #    ipv4 = {
      #      addresses = [{
      #        address = "172.21.21.2";
      #        prefixLength = 24;
      #      }];
      #    };
      #  };
      #};
    };

    users.users.ghuntley.openssh.authorizedKeys.keyFiles = [ ./id_rsa.pub.vm ];

    boot = {
      kernelParams = [ "elevator=noop" ];
      loader = {
        systemd-boot = { enable = true; };
        efi = { canTouchEfiVariables = true; };
      };
      initrd = { availableKernelModules = [ "sd_mod" "sr_mod" ]; };
      kernelModules = [ "hv_sock" ];
      extraModulePackages = [ ];
    };

    virtualisation = {
      hypervGuest = {
        enable = true;
        videoMode = "1024x768";
      };
      docker = { enable = true; };
    };

    # fileSystems."/home/ghuntley/winhome" = {
    #   device = "//172.21.21.1/ghuntley";
    #   fsType = "cifs";
    #   options = [
    #     "uid=1000"
    #     "gid=100"
    #     "credentials=/home/ghuntley/secrets/samba"
    #     "noauto"
    #     "x-systemd.idle-timeout=60"
    #     "x-systemd.device-timeout=5s"
    #     "x-systemd.mount-timeout=5s"
    #     "x-systemd.automount"
    #   ];
    # };
  };
}

