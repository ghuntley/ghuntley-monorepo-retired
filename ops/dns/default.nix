# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ depot, pkgs, lib, ... }:

with pkgs;

stdenv.mkDerivation rec {
  pname = "ops-dns";
  version = "1.0.0";

  src = ./.;

  buildInputs = [ pulumi-bin ];

  installPhase = ''
    mkdir -p $out
    cp -R ${pulumi-bin}/* $out/
    cp -R $src/* $out/
    ./bin/pulumi check
  '';
}