# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [ gist ];

  environment.interactiveShellInit = ''
    alias gist='gist --private'
  '';
}
