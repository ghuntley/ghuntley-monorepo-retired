# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ depot, pkgs, lib, ... }:

with pkgs;
stdenv.mkDerivation rec {
  pname = "macos";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "kholia";
    repo = "OSX-KVM";
    rev = "02697820d159f84000523ad19957288603e3d823";
    sha256 = "tnLjpS6M1zRjnH2hBaTWY/57VAlh+Su/tPuAAy5NH4o=";
  };

  buildInputs = [ curl python ];

  buildCommand = ''
    mkdir -p $out
    cp -r $src/* $out
  '';
}
