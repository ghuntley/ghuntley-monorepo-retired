# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, ... }: {
  services.openssh.enable = true;
  virtualisation.qemu.networkingOptions = [
    # We need to re-define our usermode network driver
    # since we are overriding the default value.
    # Than we can use qemu's hostfwd option to forward ports.
    "-net nic,netdev=user.0,model=virtio"

    # https://www.nomadproject.io/docs/install/production/requirements
    "-netdev user,id=user.0,hostfwd=tcp::4646-:4646"

    # https://www.consul.io/docs/install/ports
    "-netdev user,id=user.1,hostfwd=tcp::8600-:8600"
    "-netdev user,id=user.2,hostfwd=tcp::8500-:8500"
    "-netdev user,id=user.3,hostfwd=tcp::8501-:8501"
    "-netdev user,id=user.4,hostfwd=tcp::8502-:8502"
  ];

  virtualisation = {
    cores = 2;
    memorySize = "8192";
  };

  services.consul.enable = true;
  services.consul.forceIpv4 = true;
  services.consul.webUi = true;

  virtualisation.docker.enable = true;

  boot.extraModprobeConfig = ''
    options kvm_intel nested=1
  '';

  virtualisation.libvirtd.enable = true;
  networking.firewall.checkReversePath = false;


  environment.systemPackages = with pkgs; [ nomad_0_12 consul docker ];

}
