# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

# This module makes it possible to get at the depot from "proper"
# NixOS modules.
#
# It needs to be included and configured in each system like this:
#
# {
#   imports = [ "${depot.depotPath}/depot.nix" ];
#   inherit depot;
# }
{ lib, ... }:

{
  options.depot = with lib; mkOption {
    description = "ghuntley's imported monorepo";
  };
}

