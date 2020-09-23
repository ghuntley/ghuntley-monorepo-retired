# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, ... }: {

  sops.secrets.personal-imap-password = {
    format = "yaml";
    sopsFile = ../secrets/secrets.yaml;
    owner = "ghuntley";
  };

  nixpkgs.config.permittedInsecurePackages = [
    "adobe-reader-9.5.5-1"
  ];

  environment.systemPackages = with pkgs; [
    adobe-reader
    antiword
    catdoc
    feh
    libreoffice
    ispell
    isync
    neomutt
    notmuch
    #xls2csv  # TODO(busywork): PR xls2csv into nixpkgs - https://github.com/northbright/xls2csv
  ];

  systemd.services.mbsyncd = {
    requiredBy = [ "multi-user.target" ];
    after = [
      "network-online.target"
    ];
    environment.HOME = "/home/ghuntley";
    serviceConfig = {
      User = "ghuntley";
      ExecStart = "${pkgs.isync}/bin/mbsync -a";
      Restart = "always";
      RestartSec = 5;
      TimeoutSec = 10;
    };
    unitConfig = {
      StartLimitIntervalSec = 0; # ensure Restart=always is always honoured
    };
  };
}
