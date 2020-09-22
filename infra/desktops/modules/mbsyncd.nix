# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, ... }: {
  systemd.user = {
    services.mbsyncd = {
      description = "mbsync";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${pkgs.isync}/bin/mbsync -a";
      };
    };
    timers = {
      mbsyncd = {
        description = "mbsyncd timer";
        timerConfig = {
          OnBootSec = "1m";
          OnUnitActiveSec = "5m";
          Unit = "mbsyncd.service";
        };
        wantedBy = [ "timers.target" ];
      };
    };
  };
}
