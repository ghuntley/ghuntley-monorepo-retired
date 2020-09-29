# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {

  hardware = {
    cpu.amd.updateMicrocode = true;
    cpu.intel.updateMicrocode = true;
    enableRedistributableFirmware = true;
  };

  # https://wiki.archlinux.org/index.php/Fwupd
  environment.systemPackages = with pkgs; [ fwupd ];
  services.fwupd.enable = true;

}
