# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {
  environment.systemPackages = with pkgs; [
    ack
    cachix
    curl
    direnv
    dos2unix
    git-lfs
    gitAndTools.gitFull
    gnupg
    htop
    iftop
    inetutils
    iotop
    lsof
    nixpkgs-fmt
    p7zip
    rpl
    tmux
    tree
    unzip
    wget
    wol
    zip
  ];

  nixpkgs.config.allowUnfree = true;

}
