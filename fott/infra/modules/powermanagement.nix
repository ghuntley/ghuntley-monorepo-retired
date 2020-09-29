# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {

  environment.systemPackages = with pkgs; [ acpi ];

  # https://wiki.archlinux.org/index.php/CPU_frequency_scaling
  powerManagement.cpuFreqGovernor = "performance";

}
