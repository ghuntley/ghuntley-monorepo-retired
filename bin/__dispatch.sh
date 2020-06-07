#!/usr/bin/env bash
# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

# This script dispatches invocations transparently to programs instantiated from
# Nix.
#
# To add a new tool, insert it into the case statement below by setting `attr`
# to the key in nixpkgs which represents the program you want to run.
set -ueo pipefail

readonly REPO_ROOT=$(dirname $0)/..
TARGET_TOOL=$(basename $0)

case "${TARGET_TOOL}" in
  ack)
    attr="nixpkgs.ack"
    ;;
  git-bug)
    attr="nixpkgs.git-bug"
    ;;
  jq)
    attr="nixpkgs.jq"
    ;;
  osht)
    attr="nixpkgs.osht"
    ;;
  rpl)
    attr="nixpkgs.rpl"
    ;;
  terraform)
    attr="nixpkgs.terraform"
    ;;
  tmux)
    attr="nixpkgs.tmux"
    ;;
  *)
    echo "The tool '${TARGET_TOOL}' is currently not installed in this repository."
    exit 1
    ;;
esac

result=$(nix-build --no-out-link --attr "${attr}" "${REPO_ROOT}")
PATH="${result}/bin:$PATH"

exec "${TARGET_TOOL}" "${@}"

