# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, ... }: {

  environment.shellInit = ''
    export GPG_TTY="$(tty)"
    gpg-connect-agent /bye
    export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
  '';

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "curses";
  };

  services.pcscd.enable = true;

  services.udev.packages = with pkgs [
    libu2f-host
    yubikey-personalization
  ];

    security.pam.yubico = {
  enable = true;
  debug = true;
  #  control = "required";
  mode = "challenge-response";
};
}
