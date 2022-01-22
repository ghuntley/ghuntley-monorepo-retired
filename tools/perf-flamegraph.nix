# Copyright (c) 2019-2021 The TVL Authors
# SPDX-License-Identifier: MIT

# Script that collects perf timing for the execution of a command and writes a
# flamegraph to stdout
{ pkgs, ... }:

pkgs.writeShellScriptBin "perf-flamegraph" ''
  set -euo pipefail

  ${pkgs.linuxPackages.perf}/bin/perf record -g --call-graph dwarf -F max "$@"
  ${pkgs.linuxPackages.perf}/bin/perf script \
    | ${pkgs.flamegraph}/bin/stackcollapse-perf.pl \
    | ${pkgs.flamegraph}/bin/flamegraph.pl
''
