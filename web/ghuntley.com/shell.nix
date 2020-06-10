# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

with import <nixpkgs> { };

mkShell rec {
  name = "dotnet";
  buildInputs = [
    (with dotnetCorePackages; combinePackages [ sdk_3_1 ])
  ];
}
