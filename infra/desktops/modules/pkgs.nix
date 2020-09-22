# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {
  environment.systemPackages = with pkgs; [
    ack
    cachix
    curl
    direnv
    dos2unix
    exfat
    git-lfs
    gitAndTools.gitFull
    gnumake
    htop
    jq
    killall
    iftop
    inetutils
    iotop
    lsof
    muchsync
    nixpkgs-fmt
    notmuch
    p7zip
    ranger
    rpl
    stow
    tmux
    tree
    unzip
    wget
    wol
    zip
  ];

  nixpkgs.config.allowUnfree = true;

}
