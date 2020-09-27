# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ lib, config, pkgs, ... }: with lib; {
  nix = {

    nixPath = [
      "sops-nix=/srv/git/third_party/github.com/mic92/sops-nix"
      "nixos-config=/etc/nixos/configuration.nix"
      "nixpkgs=/srv/git/third_party/github.com/nixos/nixpkgs"
    ];

    trustedUsers = [ "mgmt" "root" ];
    useSandbox = true;
    gc.automatic = true;
    gc.dates = "03:15";
    package = pkgs.nixFlakes;

    # should be enough?
    nrBuildUsers = 32;

    # https://github.com/NixOS/nix/issues/719
    extraOptions = ''
      builders-use-substitutes = true
      cores = 0
      experimental-features = nix-command flakes
      fsync-metadata = ${lib.boolToString (config.fileSystems."/".fsType != "zfs")}
      http-connections = 0
      keep-derivations = true
      keep-outputs = true
      max-free = ${toString (4096 * 1024 * 1024)}
      max-jobs = auto
      min-free = ${toString (1024 * 1024 * 1024)}
    '';

    binaryCaches = [
      "https://cache.nixos.org"
      "https://cachix.cachix.org"
      "https://ghuntley.cachix.org"
      "https://nix-community.cachix.org"
    ];
    binaryCachePublicKeys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
      "ghuntley.cachix.org-1:SYPiX2s5cpz9JfySsQD7HkhQkUagmsdrtPFpKazS5a4="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  nixpkgs.config.allowUnfree = true;
}
