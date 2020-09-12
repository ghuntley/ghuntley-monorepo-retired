# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ config, pkgs, ... }:
{
  nixpkgs.config.vim = {
    ftNixSupport = true;
  };

  environment.variables = {
    EDITOR = [ "${pkgs.neovim}/bin/nvim" ];
  };
}
