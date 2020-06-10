#!/usr/bin/env bash
# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

# This script dispatches invocations transparently to programs instantiated from
# Nix.
#
# To add a new tool, insert it into the case statement below by setting `attr`
# to the key in nixpkgs which represents the program you want to run.
set -ueo pipefail

readonly REPO_ROOT="$(dirname "$0")/.."
TARGET_TOOL="$(basename "$0")"

case "${TARGET_TOOL}" in
  "ack")
    attr="third_party.ack"
    ;;
  "aws")
    attr="third_party.aws"
    ;;
  "azure-cli")
    attr="third_party.azure-cli"
    ;;
  "cachix")
    attr="third_party.cachix"
    ;;
  "git")
    attr="third_party.git"
    ;;
  "git-bug")
    attr="third_party.git-bug"
    ;;
  "git-lfs")
    attr="third_party.git"
    ;;
  "google-cloud-sdk")
    attr="third_party.google-cloud-sdk"
    ;;
  "jq")
    attr="third_party.jq"
    ;;
  "rpl")
    attr="third_party.rpl"
    ;;
  "terraform")
    attr="third_party.terraform"
    ;;
  "tmux")
    attr="third_party.tmux"
    ;;
  "tree")
    attr="third_party.tree"
    ;;
  *)
    echo "The tool '${TARGET_TOOL}' is currently not installed in this repository."
    exit 1
    ;;
esac

result=$(nix-build --no-out-link --attr "${attr}" "${REPO_ROOT}")
PATH="${result}/bin:$PATH"

exec "${TARGET_TOOL}" "${@}"

