# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ depot, pkgs, ... }:

rec {
  compute = import ./compute/configuration.nix;
  computeSystem = (pkgs.nixos {
    configuration = compute;
  }).system;

  builder = import ./builder/configuration.nix;
  builderSystem = (pkgs.nixos {
    configuration = builder;
  }).system;

  relay = import ./relay/configuration.nix;
  relaySystem = (pkgs.nixos {
    configuration = relay;
  }).system;

  # Build relay in CI
  meta.targets = [ "builderSystem" "computeSystem" "relaySystem" ];

  rebuilder =
    let
      depotPath = "/home/grfn/code/depot";

      caseFor = hostname: ''
        ${hostname})
          echo "Rebuilding NixOS for //users/glittershark/nixos/${hostname}"
          system=$(nix-build -E '(import ${depotPath} {}).users.glittershark.system.system.${hostname}' --no-out-link)
          ;;
      '';
    in
    depot.third_party.writeShellScriptBin "rebuilder" ''
      set -ue
      if [[ $EUID -ne 0 ]]; then
        echo "Oh no! Only root is allowed to rebuild the system!" >&2
        exit 1
      fi

      case $HOSTNAME in
      ${caseFor "relay"}
      *)
        echo "$HOSTNAME is not a known NixOS host!" >&2
        exit 1
        ;;
      esac

      nix-env -p /nix/var/nix/profiles/system --set $system
      $system/bin/switch-to-configuration switch
    '';
}
