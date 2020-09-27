# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {

  networking.wireless.enable = true;

  #sops.secrets.wpa_supplicant = {
  #  format = "binary";
  #  sopsFile = ../secrets/wpa_supplicant;
  #  path = "/etc/wpa_supplicant.conf";
  #};

}
