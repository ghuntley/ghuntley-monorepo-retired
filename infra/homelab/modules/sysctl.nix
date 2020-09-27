# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {
  boot.kernel.sysctl."fs.file-max" = 100000;
  boot.kernel.sysctl."fs.inotify.max_user_watches" = 524288;
}
