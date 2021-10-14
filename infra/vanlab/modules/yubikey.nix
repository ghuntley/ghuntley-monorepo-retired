# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, ... }: {

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "curses";
  };

  environment.systemPackages = with pkgs; [
    gnupg
    keychain
    pcsclite
    pinentry
    yubico-pam
    yubico-piv-tool
  ];

  services.pcscd.enable = true;

  services.udev.packages = with pkgs; [
    libu2f-host
    pcsclite
    yubico-piv-tool
  ];

  security.pam.yubico = {
    enable = true;
    debug = true;
    #  control = "required";
    mode = "challenge-response";
  };

}
