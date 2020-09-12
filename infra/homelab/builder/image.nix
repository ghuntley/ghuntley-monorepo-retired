# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary
let
  pkgs = (import ../../../../../../default.nix { });
  machine = import "${pkgs.path}/nixos/lib/eval-config.nix" {
    system = "x86_64-linux";
    modules = [
      ({ config, ... }: { imports = [ ./system.nix ]; })
    ];
  };
in
machine.config.system.build.azureImage
