# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {

  boot.loader = {
    cleanTmpDir = true;
    systemd-boot =
      {
        enable = true;
        memtest86.enable = true;
      }
        efi.canTouchEfiVariables = true;
  };

}
