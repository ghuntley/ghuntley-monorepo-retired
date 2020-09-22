# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {
  programs = {
    iftop.enable = true;
    iotop.enable = true;
    mtr.enable = true;
  };
}
