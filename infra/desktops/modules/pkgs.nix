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
    figlet
    git-lfs
    gitAndTools.gitFull
    gitAndTools.rs-git-fsmonitor
    gnumake
    htop
    htop
    iftop
    inetutils
    iotop
    jq
    killall
    lsof
    muchsync
    nixpkgs-fmt
    notmuch
    p7zip
    ranger
    rpl
    stow
    sops
    t
    tmux
    tmux
    tree
    unzip
    vim
    watchman
    wget
    wget
    wol
    zip
  ];

  nixpkgs.config.allowUnfree = true;

}
