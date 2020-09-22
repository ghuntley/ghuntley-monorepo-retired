# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, sops, ... }: {
  
  sops.defaultSopsFile = ../secrets/secrets.yaml;

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
  
  sops.secrets.wpa_supplicant = {
    format = "binary";
    sopsFile = ../secrets/wpa_supplicant;
    path = "/etc/wpa_supplicant.conf";
  };

}
