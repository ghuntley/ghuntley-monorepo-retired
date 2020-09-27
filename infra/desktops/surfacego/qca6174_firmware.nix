# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ stdenv, pkgs }:
stdenv.mkDerivation {
  name = "surface-wifi-firmware";
  src = ./firmware;
  priority = 1;
  installPhase = ''
    mkdir -p $out/lib/firmware/ath10k
    cp -r $src/* $out/lib/firmware/ath10k
  '';
}
