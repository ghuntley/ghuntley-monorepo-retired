# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{
  systemd.services.cachix-push-daemon = {
    requiredBy = [ "multi-user.target" ];
    after = [
      "network-online.target"
    ];
    environment.HOME = "/root";
    path = [ pkgs.nix ]; # cachix shells out to nix binaries
    serviceConfig = {
      ExecStart = "${pkgs.cachix}/bin/cachix push ghuntley --watch-store";
      Restart = "always";
      RestartSec = 5;
      TimeoutSec = 10;
    };
    unitConfig = {
      StartLimitIntervalSec = 0; # ensure Restart=always is always honoured
    };
  };
}
