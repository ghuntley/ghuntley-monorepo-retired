# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, ... }: {

  environment.systemPackages = with pkgs; [
    gnupg
    yubico-pam
    yubikey-manager
    yubikey-personalization
    yubikey-personalization-gui
    yubioath-desktop
  ];

  services.pcscd.enable = true;

  services.udev.packages = with pkgs; [
    libu2f-host
    yubikey-personalization
    yubioath-desktop
  ];

}
