# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ config, lib, pkgs, ... }:
{
  virtualisation.qemu.options = [ "-bios" "${pkgs.OVMF.fd}/FV/OVMF.fd" ];
}
