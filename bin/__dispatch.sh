#!/usr/bin/env bash
# This script dispatches invocations transparently to programs instantiated from
# Nix.
#
# To add a new tool, insert it into the case statement below by setting `attr`
# to the key in nixpkgs which represents the program you want to run.
set -ueo pipefail

readonly REPO_ROOT=$(dirname $0)/..
TARGET_TOOL=$(basename $0)

case "${TARGET_TOOL}" in
  terraform)
    attr="third_party.terraform-gcp"
    ;;
  kontemplate)
    attr="kontemplate"
    ;;
  stern)
    attr="third_party.stern"
    ;;
  kms_pass)
    attr="ops.kms_pass"
    TARGET_TOOL="pass"
    ;;
  aoc2019)
    attr="fun.aoc2019.${1}"
    ;;
  rink)
    attr="third_party.rink"
    ;;
  age)
    attr="third_party.age"
    ;;
  age-keygen)
    attr="third_party.age"
    ;;
  rebuilder)
    attr="ops.nixos.rebuilder"
    ;;
  meson)
    attr="third_party.meson"
    ;;
  ninja)
    attr="third_party.ninja"
    ;;
  *)
    echo "The tool '${TARGET_TOOL}' is currently not installed in this repository."
    exit 1
    ;;
esac

result=$(nix-build --no-out-link --attr "${attr}" "${REPO_ROOT}")
PATH="${result}/bin:$PATH"

exec "${TARGET_TOOL}" "${@}"
