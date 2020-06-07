# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{
  services.syncthing = {
    enable = true;
    user = "ghuntley";
    openDefaultPorts = true;
    dataDir = "/home/ghuntley";
  };
}

