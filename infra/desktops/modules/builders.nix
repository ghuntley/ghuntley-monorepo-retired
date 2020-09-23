# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, sops, ... }: {

  sops.secrets.github-runner-name = {
    format = "yaml";
    sopsFile = ../secrets/secrets.yaml;
  };

  sops.secrets.github-runner-repo-url = {
    format = "yaml";
    sopsFile = ../secrets/secrets.yaml;
  };

  sops.secrets.github-runner-token = {
    format = "yaml";
    sopsFile = ../secrets/secrets.yaml;
  };

  sops.secrets.buildkite-agent-token = {
    format = "binary";
    sopsFile = ../secrets/buildkite-agent-token;
  };

  virtualisation.oci-containers.containers = {

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
        REPO_URL = "$(cat ${config.sops.secrets.github-runner-repo-url.path})";
        RUNNER_NAME = "$(cat ${config.sops.secrets.github-runner-name.path})";
        RUNNER_TOKEN = "$(cat ${config.sops.secrets.github-runner-token.path})";
        RUNNER_WORKDIR = "/build";
      };
      cmd = [
        "start"
      ];
    };
  };

  services.buildkite-agents = lib.listToAttrs (map
    (n: rec {
      name = "metabox-${toString n}";
      value = {
        inherit name;
        enable = true;
        tokenPath = config.sops.secrets.buildkite-agent-token.path;
        tags = { hardware = "laptop"; os = "nixos"; docker = "true"; kvm = "true"; nix = "true"; nvidia = "true"; };
      };
    })
    (lib.range 1 32));

}
