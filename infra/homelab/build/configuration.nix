# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ config, pkgs, ... }:
let secrets = import /etc/nixos/secrets.nix;
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  networking.hostName = "build.wg.ghuntley.net"; # Define your hostname.

  networking.useDHCP = false;
  networking.interfaces.ens192.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  time.timeZone = "Brisbane/Australia";

  environment.systemPackages = with pkgs; [
    git htop vim tmux 
  ];

  services.openssh.enable = true;

  virtualisation.vmware.guest.enable = true;
  virtualisation.docker.enable = true;
  virtualisation.libvirtd.enable = true;

  docker-containers = {

    buildkite = {
      image = "buildkite/agent:3-ubuntu";
      volumes = [
        #
        # nb. careful this allows root access
        "/var/run/docker.sock:/var/run/docker.sock" # nb. careful this allows root access
        # nb. careful this allows root access
        #
        "/nix:/nix"
      ];
      environment = {
        BUILDKITE_AGENT_TOKEN = secrets.buildkite-agent-token;
      };
      cmd = [
        "start"
      ];
    };

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
        REPO_URL = secrets.github-runner-repo-url;
        RUNNER_NAME = secrets.github-runner-name;
        RUNNER_TOKEN = secrets.github-runner-token;
        RUNNER_WORKDIR = "/build";
      };
      cmd = [
        "start"
      ];
    };
  };

  # networking.firewall.allowedTCPPorts = [ 22 ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  networking.firewall.enable = false;

  users.users.mgmt = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; 
  };

  system.autoUpgrade.enable = true;

  # Free up to 4GiB whenever there is less than 1GiB left:
  nix.gc.automatic = true;
  nix.extraOptions = ''
    min-free = ${toString (1024 * 1024 * 1024)}
    max-free = ${toString (4096 * 1024 * 1024)}
  '';

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?

}
