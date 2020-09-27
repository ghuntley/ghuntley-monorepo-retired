# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {

  environment.systemPackages = with pkgs; [ keychain ];

  services.physlock = {
    allowAnyUser = false;
    enable = true;
    disableSysRq = true;
    lockOn = {
      hibernate = true;
      suspend = true;
      # start x11 and associated programs in background but require password to unlock workstation
      extraTargets = [ "display-manager.service" ];
    };
#    muteKernelMessages = true;
  };

  security.sudo.extraConfig = ''
    %users ALL= NOPASSWD: ${pkgs.systemd}/bin/systemctl start physlock
  '';

  systemd.services.unload-secrets = {
    path = [
      pkgs.bash
      pkgs.gawk
      pkgs.killall
      pkgs.physlock
      pkgs.ps
      pkgs.stdenv
    ];
    before = [
      "hibernate.target"
      "physlock.service"
      "suspend.target"
    ];
    wantedBy = [
      "physlock.service"
      "systemd-hibernate.service"
      "systemd-suspend-then-hibernate.service"
      "systemd-suspend.service"
    ];
    serviceConfig = {
      User = "root";
      Type = "simple";
    };
    script = ''
      AGENTS=`ps aux | grep gpg | grep -v grep | awk {'print $2'} | wc -l`
      echo Active gpg-agents: $AGENTS
      ps aux | grep gpg | grep -v grep

      echo
      echo Terminating all gpg-agents...
      for pid in `ps aux | grep gpg | grep -v grep | awk {'print $2'}`; do kill -9 $pid;done
      echo

      AGENTS=`ps aux | grep gpg | grep -v grep | awk {'print $2'} | wc -l`
      echo Active gpg-agents: $AGENTS
      ps aux | grep gpg | grep -v grep
      if [ $AGENTS -gt 0 ]
      then
        echo ERROR: Agents are still running:
        ps aux | grep gpg
        exit 1
      fi
    '';
  };


}
