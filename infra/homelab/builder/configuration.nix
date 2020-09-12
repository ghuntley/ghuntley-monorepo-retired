# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ config, pkgs, ... }:
{
  imports =
    [
      ./hardware-configuration.nix
      ../modules/docker.nix
      ../modules/fail2ban.nix
      ../modules/firewall.nix
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
      ../modules/tor-ssh.nix
      ../modules/vmware-guest.nix
      ../users/default.nix
    ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  networking.hostName = "builder.wg.ghuntley.net";

  networking.useDHCP = false;
  networking.interfaces.ens192.useDHCP = true;

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
        "/srv/buildkite:/var/lib/buildkite/builds"
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

  system.stateVersion = "20.03";

}
