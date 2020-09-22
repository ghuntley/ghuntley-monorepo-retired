# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {

  environment.systemPackages = with pkgs; [
    alacritty
    arandr
    autorandr
    discord
    dmenu
    feh
    j4-dmenu-desktop
    libnotify
    minitube
    mplayer
    obs-studio
    okular
    pasystray
    pavucontrol
    peek
    pinentry
    polybarFull
    qutebrowser
    restic
    rofi
    signal-cli
    signal-desktop
    silver-searcher
    simplescreenrecorder
    slack
    tdesktop
    terminus
    termite.terminfo
    xclip
    xorg.xbacklight
    xorg.xmodmap
    xorg.xprop
    xorg.xrdb
    xsel
    yank
    zathura
  ];

}
