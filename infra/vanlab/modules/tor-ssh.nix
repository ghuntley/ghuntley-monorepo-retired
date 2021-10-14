# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ ... }: {
  services.tor = {
    enable = true;
    hiddenServices."ssh".map = [{
      port = 22;
    }];
    extraConfig = ''
      DNSPort 9053
      AutomapHostsOnResolve 1
      AutomapHostsSuffixes .exit,.onion
      EnforceDistinctSubnets 1
      ExitNodes {au}
      EntryNodes {au}
      NewCircuitPeriod 120
    '';
  };

  imports = [ ./sshd.nix ];
}
