# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ ... }: {
  boot.extraModprobeConfig = ''
    options kvm_intel nested=1
  '';

  virtualisation.libvirtd.enable = true;
  users.extraUsers.mgmt.extraGroups = [ "libvirtd" ];
  networking.firewall.checkReversePath = false;
}
