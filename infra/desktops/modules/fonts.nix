# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {

  fonts = {
    enableDefaultFonts = true;

    # NOTE: Some fonts may break colour emojis in Chrome
    # cf. https://github.com/NixOS/nixpkgs/issues/69073#issuecomment-621982371
    fonts = with pkgs; [
      fira-code
      fira-code-symbols
      font-awesome-ttf
      hack-font
      source-code-pro
      nerdfonts
      noto-fonts-emoji
      powerline-fonts
    ];
  };
}
