# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, ... }: {

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "qt";
  };

  environment.systemPackages = with pkgs; [
    gnupg
    keychain
    pcsclite
    pinentry
    yubico-pam
    yubico-piv-tool
    yubikey-manager
    yubikey-manager-qt
    yubikey-personalization
    yubikey-personalization-gui
    yubioath-desktop
  ];

  services.pcscd.enable = true;

  services.udev.packages = with pkgs; [
    libu2f-host
    pcsclite
    yubico-piv-tool
    yubikey-personalization
    yubioath-desktop
  ];

  security.pam.yubico = {
    enable = true;
    debug = true;
    #  control = "required";
    mode = "challenge-response";
  };

}
