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
          OnUnitActiveSec =
            if config.networking.hostName == "nuc" then "20m" else "5m";
          Unit = "mbsyncd.service";
        };
        wantedBy = [ "timers.target" ];
      };
    };
  };
}
