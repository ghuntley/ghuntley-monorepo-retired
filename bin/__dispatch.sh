#!/usr/bin/env bash
# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

# This script dispatches invocations transparently to programs instantiated from
# Nix.
#
# To add a new tool, insert it into the case statement below by setting `attr`
# to the key in nixpkgs which represents the program you want to run.
#
# If the binary name of the tool has been aliased, define the correct name by
# setting `aka`.
#
# Example:
#
#  "binaryname")
#    attr="third_party.nix-expression"
#    aka="binary-name-in-nix-expression-if-different"
#    ;;

set -ueo pipefail

readonly REPO_ROOT="$(dirname "$0")/.."
TARGET_TOOL="$(basename "$0")"

case "${TARGET_TOOL}" in
  "7z")
    aka=""
    attr="third_party.p7zip"
    ;;
  "ack")
    aka=""
    attr="third_party.ack"
    ;;
  "aws")
    aka=""
    attr="third_party.aws"
    ;;
  "az")
    aka=""
    attr="third_party.azure-cli"
    ;;
  "bat")
    aka=""
    attr="third_party.bat"
    ;;
  "black")
    aka=""
    attr="third_party.black"
    ;;
  "cabal-fmt")
    aka=""
    attr="third_party.cabal-fmt"
    ;;
  "cachix")
    aka=""
    attr="third_party.cachix"
    ;;
  "cookiecutter")
    aka=""
    attr="third_party.cookiecutter"
    ;;
  "ctags")
    aka=""
    attr="third_party.ctags"
    ;;
  "dhall")
    aka=""
    attr="third_party.dhall"
    ;;
  "docker")
    aka=""
    attr="third_party.docker"
    ;;
  "dotnet")
    aka=""
    attr="third_party.dotnet"
    ;;
  "editorconfig-checker")
    aka=""
    attr="third_party.editorconfig-checker"
    ;;
  "flake8")
    aka=""
    attr="third_party.flake8"
    ;;
  "fzf")
    aka=""
    attr="third_party.fzf"
    ;;
  "gcloud")
    aka=""
    attr="third_party.google-cloud-sdk"
    ;;
  "git")
    aka=""
    attr="third_party.git"
    ;;
  "git-bug")
    aka=""
    attr="third_party.git-bug"
    ;;
  "go")
    aka=""
    attr="third_party.go"
    ;;
  "hash-password")
    aka=""
    attr="tools.hash-password"
    ;;
  "hlint")
    aka=""
    attr="third_party.hlint"
    ;;
  "jq")
    aka=""
    attr="third_party.jq"
    ;;
  "rpl")
    aka=""
    attr="third_party.rpl"
    ;;
  "rustfmt")
    aka=""
    attr="third_party.rustfmt"
    ;;
  "shellcheck")
    aka=""
    attr="third_party.shellcheck"
    ;;
  "sops")
    aka=""
    attr="third_party.sops"
    ;;
  "terraform")
    aka=""
    attr="third_party.terraform-with-plugins"
    ;;
  "tmux")
    aka=""
    attr="third_party.tmux"
    ;;
  "tree")
    aka=""
    attr="third_party.tree"
    ;;
  "pip")
    aka=""
    attr="third_party.pip"
    ;;
  "perf-flamegraph")
    aka=""
    attr="tools.perf-flamegraph"
    ;;
  "pwsh")
    aka=""
    attr="third_party.powershell"
    ;;
  "python")
    aka=""
    attr="third_party.python"
    ;;
  "pydocstyle")
    aka=""
    attr="third_party.pydocstyle"
    ;;
  "pylint")
    aka=""
    attr="third_party.pylint"
    ;;
  "hg")
    aka=""
    attr="third_party.mercurialFull"
    ;;
  "isort")
    aka=""
    attr="third_party.isort"
    ;;
  "nix-linter")
    aka=""
    attr="third_party.nix-linter"
    ;;
  "nixos-shell")
    aka=""
    attr="third_party.nixos-shell"
    ;;
  "nixpkgs-fmt")
    aka=""
    attr="third_party.nixpkgs-fmt"
    ;;
  "node")
    aka="nodejs"
    attr="third_party.nodejs"
    ;;
  "nodejs")
    aka=""
    attr="third_party.nodejs"
    ;;
  "nomad")
    aka=""
    attr="third_party.nomad"
    ;;
  "npm")
    aka=""
    attr="third_party.npm"
    ;;
  "nvim")
    aka=""
    attr="third_party.neovim"
    ;;
  "ormolu")
    aka=""
    attr="third_party.ormolu"
    ;;
  "pulumi")
    aka=""
    attr="third_party.pulumi-bin"
    ;;
  "rs-git-fsmonitor")
    aka=""
    attr="third_party.rs-git-fsmonitor"
    ;;
  "vi")
    aka="nvim"
    attr="third_party.neovim"
    ;;
  "vim")
    aka="nvim"
    attr="third_party.neovim"
    ;;
  "watchman")
    aka=""
    attr="third_party.watchman"
    ;;
  "unzip")
    aka=""
    attr="third_party.unzip"
    ;;
  "yarn")
    aka=""
    attr="third_party.yarn"
    ;;
  "yamllint")
    aka=""
    attr="third_party.yamllint"
    ;;
  "youtube-dl")
    aka=""
    attr="third_party.youtube-dl"
    ;;
  *)
    echo "The tool '${TARGET_TOOL}' is currently not installed in this repository."
    exit 1
    ;;
esac

result=$(nix-build --no-out-link --attr "${attr}" "${REPO_ROOT}")
PATH="${result}/bin:$PATH"

if [ ${#aka} -ge 1 ];
then
    exec "${aka}" "${@}"; # aliased
else
    exec "${TARGET_TOOL}" "${@}"; # not aliased
fi
