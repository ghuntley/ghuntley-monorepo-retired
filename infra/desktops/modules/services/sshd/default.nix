# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ config, pkgs, ... }: {
  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    forwardX11 = true;

    permitRootLogin = "no";

    # TODO: Change to only pubkey auth
    passwordAuthentication = true;
    challengeResponseAuthentication = false;
  };
}

