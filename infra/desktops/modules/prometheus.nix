# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {

  environment.systemPackages = with pkgs; [ smartmontools ];

  services.smartd = {
    autodetect = true;
    enable = true;

    notifications.wall.enable = true;
    notifications.x11.enable = true;
  };

  networking.firewall.interfaces."tailscale0".allowedTCPPorts = [ 9100 ];

  services.prometheus.exporters.node = {
    enable = true;
    enabledCollectors = [
      # "netclass" "exec" "edec" "boottime"
      "arp"
      "bonding"
      "conntrack"
      "cpu"
      "diskstats"
      "entropy" # "exec"
      "filefd"
      "filesystem"
      "hwmon"
      "loadavg"
      "mdadm"
      "meminfo"
      "netdev"
      "netstat"
      "sockstat"
      "systemd"
      "textfile"
      "time"
      "vmstat"
      "wifi"
      "zfs"
    ];
    extraFlags = [
      "--collector.textfile.directory=/var/lib/prometheus-node-exporter-text-files"
      ""
    ];
  };

  system.activationScripts.node-exporter-system-version = ''
    mkdir -pm 0775 /var/lib/prometheus-node-exporter-text-files
    (
      cd /var/lib/prometheus-node-exporter-text-files
      (
        echo -n "system_version ";
        readlink /nix/var/nix/profiles/system | cut -d- -f2
      ) > system-version.prom.next
      mv system-version.prom.next system-version.prom
    )
  '';

  systemd.timers.prometheus-smartmon-exporter = {
    description = "Captures smartmon data";
    wantedBy = [ "timers.target" ];
    partOf = [ "prometheus-smartmon-exporter.service" ];
    enable = true;
    timerConfig = {
      OnCalendar = "*:*";
      Unit = "prometheus-smartmon-exporter.service";
      Persistent = "yes";
    };
  };

  systemd.services.prometheus-smartmon-exporter = {
    path = [ pkgs.bash pkgs.gawk pkgs.smartmontools ];
    serviceConfig = {
      Type = "oneshot";
      PrivateTmp = true;
      WorkingDirectory = "/tmp";
    };
    script = ''
      mkdir -pm 0775 /var/lib/prometheus-node-exporter-text-files
      cd /var/lib/prometheus-node-exporter-text-files
      set -euxo pipefail
      ${./smartmon.sh} | ${pkgs.moreutils}/bin/sponge smartmon.prom
    '';

  };

  systemd.timers.prometheus-zfs-snapshot-exporter = {
    description = "Captures snapshot data";
    wantedBy = [ "timers.target" ];
    partOf = [ "prometheus-zfs-snapshot-exporter.service" ];
    enable = true;
    timerConfig = {
      OnCalendar = "*:0/3";
      Unit = "prometheus-zfs-snapshot-exporter.service";
      Persistent = "yes";
    };
  };

  systemd.services.prometheus-zfs-snapshot-exporter = {
    path = with pkgs; [ bash gawk gnused moreutils zfs ];
    serviceConfig = {
      Type = "oneshot";
      PrivateTmp = true;
      WorkingDirectory = "/tmp";
    };
    script = ''
      mkdir -pm 0775 /var/lib/prometheus-node-exporter-text-files
      cd /var/lib/prometheus-node-exporter-text-files
      set -euxo pipefail
      zfs list -Hp -t snapshot -o name,creation \
        | sed -e 's#@.*\s# #' \
        | awk '
            {
              if (last[$1] < $2) {
                last[$1]=$2
              }
            }
            END {
              for (m in last) {
                printf "zfs_snapshot_age_seconds{dataset=\"%s\"} %s\n", m, last[m];
              }
            }
          ' \
        | sponge znapzend-snaps.prom
    '';
  };

}
