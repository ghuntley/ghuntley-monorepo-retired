# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {

  # https://wiki.archlinux.org/index.php/CPU_frequency_scaling
  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";

  powerManagement.powertop.enable = true;

  services.upower.enable = true;

}
