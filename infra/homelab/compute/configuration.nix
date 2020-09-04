# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ config, pkgs, lib, ... }:
let secrets = import /etc/nixos/secrets.nix;
in
{
  imports =
    [
      /etc/nixos/hardware-configuration.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "compute.wg.ghuntley.net";

  networking.useDHCP = false;
  networking.interfaces.ens32.useDHCP = true;

  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  i18n.defaultLocale = "en_US.UTF-8";

  time.timeZone = "Australia/Brisbane";

  environment.systemPackages = with pkgs; [
    direnv
    git
    git-lfs
    htop
    inetutils
    smartmontools
    tailscale
    tmux
    vim
    wget
  ];

  nixpkgs.config.vim = {
    ftNixSupport = true;
  };

  environment.interactiveShellInit = ''
    alias vi='vim'
  '';

  environment.variables = {
    EDITOR = [ "${pkgs.vim}/bin/vim" ];
  };

  services.openssh.enable = true;
  services.tailscale.enable = true;

  services.telegraf.enable = true;

  virtualisation.vmware.guest.enable = true;
  virtualisation.docker.enable = true;

  docker-containers = {

    buildkite = {
      image = "buildkite/agent:3-ubuntu";
      volumes = [
        "/srv/buildkite/builds:/buildkite/builds"
        #
        # nb. careful this allows root access
        #"/var/run/docker.sock:/var/run/docker.sock" # nb. careful this allows root access
        # nb. careful this allows root access
        #
      ];
      environment = {
        BUILDKITE_BUILD_PATH = "/var/lib/buildkite/builds";
        BUILDKITE_AGENT_TOKEN = secrets.buildkite-agent-token;
      };
      cmd = [
        "start"
      ];
    };

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
        PASSWORD = secrets.codeserver-password;
      };
      cmd = [
        "--auth"
        "password"
      ];
    };

    cgit = {
      image = "clearlinux/cgit";
      ports = [
        "0.0.0.0:8888:80"
      ];
      volumes = [
        "/srv/cgit:/var/www/cgit"
      ];
      environment = { };
      cmd = [
      ];
    };

    dnsrobocert = {
      image = "adferrand/dnsrobocert";
      volumes = [
        #
        # nb. careful this allows root access
        "/var/run/docker.sock:/var/run/docker.sock"
        # nb. careful this allows root access
        #
        "/srv/dnsrobocert/dnsrobocert/config.yml:/etc/dnsrobocert/config.yml"
        "/srv/dnsrobocert/letsencrypt:/etc/letsencrypt"
      ];
      environment = {
        CLOUDFLARE_AUTH_USERNAME = secrets.dnsrobocert-cloudflare-auth-username;
        CLOUDFLARE_AUTH_TOKEN = secrets.dnsrobocert-cloudflare-auth-token;
        CLOUDFLARE_ZONE_ID = secrets.dnsrobocert-cloudflare-zone-id;
      };
      cmd = [
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
      };
      cmd = [
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

    ingress = {
      image = "library/nginx:latest";
      ports = [
        "0.0.0.0:80:80"
        "0.0.0.0:443:443"
      ];
      volumes = [
        "/srv/git/ghuntley/trunk/infra/homelab/ingress/default.conf:/etc/nginx/conf.d/default.conf"
        "/srv/dnsrobocert/letsencrypt:/etc/letsencrypt"
      ];
      environment = { };
      cmd = [
      ];
    };


    #motion = {
    #  image = "motionproject/motion:latest";
    #  ports = [
    #    "0.0.0.0:7070:7999"
    #  ];
    #  volumes = [
    #  ];
    #  environment = {
    #    TZ = "Australia/Brisbane";
    #  };
    #  cmd = [
    #  ];
    #};

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

    #rclone = {
    #  image = "nixery.dev/rclone";
    #  ports = [
    #  ];
    #  volumes = [
    #  ];
    #  environment = { };
    #  cmd = [
    #  ];
    #};

    smtp = {
      image = "juanluisbaptiste/postfix";
      ports = [
        "0.0.0.0:25:25"
      ];
      volumes = [
      ];
      environment = {
        ALWAYS_ADD_MISSING_HEADERS = "yes";
        SERVER_HOSTNAME = "smtp.ghuntley.net";
        SMTP_SERVER = secrets.smtp-server;
        SMTP_USERNAME = secrets.smtp-username;
        SMTP_PASSWORD = secrets.smtp-password;
      };
      cmd = [
      ];
    };

    #telegraf = {
    #  image = "telegraf";
    #  ports = [
    #  ];
    #  volumes = [
    #    "/srv/telegraf/telegraf.conf:/etc/telegraf/telegraf.conf"
    #  ];
    #  environment = {
    #  };
    #  cmd = [
    #  ];
    #};

    #};

    portainer = {
      image = "portainer/portainer";
      ports = [
        "0.0.0.0:9000:9000"
      ];
      volumes = [
        #
        # nb. careful this allows root access
        "/var/run/docker.sock:/var/run/docker.sock"
        # nb. careful this allows root access
        #
        "/srv/portainer:/data"
      ];
      environment = { };
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

    #squid = {
    #  image = "nixery.dev/squid";
    #  ports = [
    #    "0.0.0.0:3128:3128"
    #  ];
    #  volumes = [
    #  ];
    #  environment = { };
    #  cmd = [
    #  ];
    #};

  };

  networking.firewall.enable = false;
  #networking.firewall.allowedTCPPorts = [
  #  80
  #  443
  #];

  users.users.mgmt = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  system.autoUpgrade.enable = true;

  nix.gc.automatic = true;
  nix.gc.dates = "weekly";
  nix.gc.options = "--delete-older-than 30d";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?

}
