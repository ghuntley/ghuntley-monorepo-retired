# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{
  nixos-shell.mounts.extraMounts = {
    "/mnt/examples" = ./.;

    "/mnt/nixos-shell" = {
      target = ./..;
      cache = "none";
    };
  };
}
