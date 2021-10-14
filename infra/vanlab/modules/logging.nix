# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {
  services.journaldriver = {
    enable = true;
    logStream = "home";
    googleCloudProject = "ghuntley-infrastructure";
    applicationCredentials = "/etc/gcp/key.json";
  };
}
