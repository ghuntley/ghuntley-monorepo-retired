# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ config, pkgs, lib, ... }:
{
  imports =
    [
      ./hardware-configuration.nix
      ../modules/bluetooth.nix
      ../modules/docker.nix
      ../modules/fail2ban.nix
      ../modules/firewall.nix
      ../modules/gist.nix
      ../modules/gpsd.nix
      ../modules/i18n.nix
      ../modules/initrd.nix
      ../modules/libvirtd.nix
      ../modules/logging.nix
      ../modules/mosh.nix
      ../modules/neovim.nix
      ../modules/networking-ipv6.nix
      ../modules/nix-daemon.nix
      ../modules/pkgs.nix
      ../modules/shell.nix
      ../modules/sshd.nix
      ../modules/sudo.nix
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

  networking.hostName = "compute";
  networking.hostId = "DEADBEEF";

  networking.useDHCP = false;
  networking.defaultGateway = "10.10.10.254";
  networking.nameservers = [ "10.10.10.254" ];

  # virtual machines
  networking.interfaces.ens32 = {
    useDHCP = false;
    ipv4.addresses = [{ address = "10.10.10.250"; prefixLength = 24; }];
  };

  # telstra
  networking.interfaces.ens192 = {
    useDHCP = false;
    ipv4.addresses = [{ address = "10.10.103.250"; prefixLength = 24; }];
  };

  # optus
  networking.interfaces.ens224 = {
    useDHCP = false;
    ipv4.addresses = [{ address = "10.10.104.250"; prefixLength = 24; }];
  };

  # vodafone
  networking.interfaces.ens256 = {
    useDHCP = false;
    ipv4.addresses = [{ address = "10.10.105.250"; prefixLength = 24; }];
  };

  docker-containers = {

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
        PASSWORD = "secrets.codeserver-password";
      };
      cmd = [
        "--auth"
        "password"
      ];
    };

    grafana = {
      image = "grafana/grafana";
      ports = [
        "0.0.0.0:3000:3000"
      ];
      volumes = [
        "/srv/grafana:/var/lib/grafana"
      ];
      environment = {
        GF_INSTALL_PLUGINS = "grafana-clock-panel";
        GF_SMTP_ENABLED = "true";
        GF_SMTP_HOST = "smtp.ghuntley.net:25";
      };
      cmd = [
      ];
    };

    homeassistant = {
      image = "homeassistant/home-assistant:stable";
      volumes = [
        "/etc/localtime:/etc/localtime:ro"
        "/srv/git/ghuntley/trunk/infra/homeassistant:/config"
      ];
      environment = { };
      extraDockerOptions = [
        "--net=host"
      ];
    };

    influxdb = {
      image = "quay.io/influxdb/influxdb:2.0.0-beta";
      ports = [
        "0.0.0.0:9999:9999"
        "0.0.0.0:8086:8086"
      ];
      volumes = [
        "/srv/influxdb:/var/lib/influxdb"
      ];
      environment = { };
      cmd = [
        "--reporting-disabled"
      ];
    };

    motion = {
      image = "motionproject/motion:latest";
      ports = [
        "0.0.0.0:7070:7999"
      ];
      volumes = [
        "/srv/motion/config:/usr/local/etc/motion"
        "/srv/motion/storage:/var/lib/motion"
      ];
      environment = {
        TZ = "Australia/Brisbane";
      };
      cmd = [
      ];
    };

    nexus = {
      image = "sonatype/nexus3";
      ports = [
        "0.0.0.0:8081:8081"
      ];
      volumes = [
        "/srv/nexus:/nexus-data"
      ];
      environment = { };
      cmd = [
      ];
    };

    smtp = {
      image = "juanluisbaptiste/postfix";
      ports = [
        "0.0.0.0:25:25"
      ];
      volumes = [
        "/srv/smtp:/var/spool/postfix"
      ];
      environment = {
        ALWAYS_ADD_MISSING_HEADERS = "yes";
        SERVER_HOSTNAME = "smtp.ghuntley.net";
        SMTP_SERVER = "secrets.smtp-server";
        SMTP_USERNAME = "secrets.smtp-username";
        SMTP_PASSWORD = "secrets.smtp-password";
      };
      cmd = [
      ];
    };

    prometheus = {
      image = "prom/prometheus";
      ports = [
        "0.0.0.0:9090:9090"
      ];
      volumes = [
        "/srv/prometheus:/etc/prometheus"
      ];
      environment = { };
      cmd = [
      ];
    };

    sourcegraph = {
      image = "sourcegraph/server:3.19.2";
      ports = [
        "0.0.0.0:7080:7080"
      ];
      volumes = [
        "/srv/sourcegraph/etc:/etc/sourcegraph"
        "/srv/sourcegraph/data:/var/opt/sourcegraph"
      ];
      environment = {
        SRC_SYNTECT_SERVER = "http://compute.wg.ghuntley.net:9238";
      };
      cmd = [
      ];
    };

    syntect_server = {
      image = "sourcegraph/syntech_server";
      ports = [
        "0.0.0.0:9238:9238"
      ];
      volumes = [
      ];
      environment = { };
      cmd = [
      ];
    };

  };

  networking.firewall.enable = true;

  networking.firewall.allowedTCPPorts = [ ];
  networking.firewall.allowedUDPPorts = [ ];

  # networking.firewall."tailscale0".allowedTCPPorts = [
  #   80
  #   443
  # ];


  system.stateVersion = "20.03";
}
