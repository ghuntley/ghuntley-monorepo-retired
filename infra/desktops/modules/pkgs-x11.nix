# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {

  environment.systemPackages = with pkgs; [
    alacritty
    arandr
    ark
    autorandr
    bluedevil
    discord
    dmenu
    gimp-with-plugins
    gitkraken
    feh
    j4-dmenu-desktop
    libnotify
    kate
    kgpg
    pinentry-qt
    ktorrent
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
    thunderbird
    vlc
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
