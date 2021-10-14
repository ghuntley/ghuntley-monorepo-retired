# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

let agents = lib.range 1 48;
in
{
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda"; 

  networking.hostName = "builder-ghuntley-net";

  time.timeZone = "UTC";

  networking.useDHCP = false;
  networking.interfaces.ens18.useDHCP = true;

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  users.users.ghuntley = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  environment.systemPackages = with pkgs; [
    bash
    git
    gnutar
    gzip
    htop
    nix
    vim
  ];

  docker-containers = {

    github-runner = {
      image = "myoung34/github-runner:latest";
      volumes = [
        #
        # nb. careful this allows root access
        "/var/run/docker.sock:/var/run/docker.sock" # nb. careful this allows root access
        # nb. careful this allows root access
        #
        "/nix:/nix"
        "/srv/github-runner:/build"
      ];
      environment = {
        REPO_URL = "secrets.github-runner-repo-url";
        RUNNER_NAME = "secrets.github-runner-name";
        RUNNER_TOKEN = "secrets.github-runner-token";
        RUNNER_WORKDIR = "/build";
      };
      cmd = [
        "start"
      ];
    };

  };

  services.buildkite-agents = lib.listToAttrs (map
    (n: rec {
      name = "builder-${toString n}";
      value = {
        inherit name;
        enable = true;
        tokenPath = "/etc/buildkiteToken";
        privateSshKeyPath = "/etc/buildkiteKey";
        # hooks.post-command = "${buildkiteHooks}/bin/post-command";
      };
    })
    agents);

  systemd.services = lib.listToAttrs (map
    (n: rec {
      name = "buildkite-agent-builder-${toString n}";
      value = {
	confinement.enable = false;
        #confinement.mode = "chroot-only";
        confinement.packages = config.services.buildkite-agents."builder-${toString n}".runtimePackages;
        serviceConfig = {
          BindReadOnlyPaths = [
            config.services.buildkite-agents."builder-${toString n}".tokenPath
            config.services.buildkite-agents."builder-${toString n}".privateSshKeyPath
            "${config.environment.etc."ssl/certs/ca-certificates.crt".source}:/etc/ssl/certs/ca-certificates.crt"
            "/etc/machine-id"
            # channels are dynamic paths in the nix store, therefore we need to bind mount the whole thing
            "/nix/store"
           ];
          BindPaths = [
            #"/var/lib/buildkite-agent-builder-${toString n}"
            config.services.buildkite-agents."builder-${toString n}".dataDir
            "/nix/var/nix/daemon-socket/socket"
            #"/tmp"
          ];
        };
      };
    })
    agents);

  services.tailscale.enable = true;
  services.openssh.enable = true;
  programs.mosh.enable = true;

  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}

