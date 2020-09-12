# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

# Most of the Nix expressions in this folder are NixOS modules, which
# are not readTree compatible.
#
# Some things (such as system configurations) are, and we import them
# here manually.
#
# TODO(tazjin): Find a more elegant solution for the whole module
# situation.
{ lib, pkgs, depot, ... }@args:
let
  inherit (lib) findFirst isAttrs;
in
rec {
  # System installation

  allSystems = import ./relay/default.nix args;

  nixosFor = configuration: depot.third_party.nixos {
    inherit configuration;
  };

  findSystem = hostname:
    (findFirst
      (system: system.config.networking.hostName == hostname)
      (throw "${hostname} is not a known NixOS host")
      (map nixosFor allSystems));

  rebuild-system = pkgs.writeShellScriptBin "rebuild-system" ''
    set -ue
    if [[ $EUID -ne 0 ]]; then
      echo "Oh no! Only root is allowed to rebuild the system!" >&2
      exit 1
    fi

    echo "Rebuilding NixOS for $HOSTNAME"
    system=$(nix-build -E "((import ${toString depot.depotPath} {}).ops.nixos.findSystem \"$HOSTNAME\").system" --no-out-link --show-trace)

    nix-env -p /nix/var/nix/profiles/system --set $system
    $system/bin/switch-to-configuration switch
  '';

  # Systems that should be built in CI
  #
  # TODO(tazjin): Refactor the whole systems setup, it's a bit
  # inconsistent at the moment.
  whitbySystem = (nixosFor relay).system;
  meta.targets = [ "whitbySystem" ];
}
