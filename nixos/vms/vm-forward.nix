# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, ... }: {
  services.openssh.enable = true;
  virtualisation.qemu.networkingOptions = [
    # We need to re-define our usermode network driver
    # since we are overriding the default value.
    "-net nic,netdev=user.0,model=virtio"
    # Than we can use qemu's hostfwd option to forward ports.
    "-netdev user,id=user.0,hostfwd=tcp::2222-:22"
  ];
}
