# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {

  networking.wireless.enable = true;
  # networking.wireless.extraConfig = (builtins.readFile config.sops.secrets.wpa_supplicant.path);

}
