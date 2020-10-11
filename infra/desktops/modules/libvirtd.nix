# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ ... }: {
  boot.extraModprobeConfig = ''
    options kvm_intel nested=1
  '';

  virtualisation.libvirtd.enable = true;

  networking.firewall.checkReversePath = false;

  networking.networkmanager.unmanaged = [ "interface-name:ve-*" ];

}
