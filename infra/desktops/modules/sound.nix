# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {

  environment.systemPackages = with pkgs; [
    pulseaudio-ctl
  ];

  sound.enable = true;

  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.extraModules = [ pkgs.pulseaudio-modules-bt ];
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
  hardware.pulseaudio.tcp.anonymousClients.allowedIpRanges = [ "127.0.0.1" ];
  hardware.pulseaudio.tcp.enable = true;

  systemd.services.audio-off = { 
    description = "Mute audio before suspend";
    enable = true;
    serviceConfig.ExecStart = "${pkgs.pamixer}/bin/pamixer --mute";
    serviceConfig.RemainAfterExit = true;
    serviceConfig.Type = "oneshot";
    serviceConfig.User = "ghuntley";
    wantedBy = [ "sleep.target" ];
  };
}
