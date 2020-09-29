# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ config, lib, pkgs, ... }:
{
  programs.ssh = {
    startAgent = false;
    extraConfig = ''
      AddKeysToAgent yes

      VerifyHostKeyDNS yes
    '';
  };
}
