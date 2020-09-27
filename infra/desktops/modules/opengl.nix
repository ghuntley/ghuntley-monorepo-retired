# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, ... }: {

  # https://nixos.wiki/wiki/Accelerated_Video_Playback
  # You can test acceleration by running: nix-shell -p libva-utils --run vainfo
  
  environment.systemPackages = with pkgs; [ 
    libva-utils
    libva1
   ];

  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };
  
  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      vaapiIntel        
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ 
    vaapiIntel        
    vaapiVdpau
    libvdpau-va-gl
  ];

# https://wiki.archlinux.org/index.php/Firefox#Hardware_video_acceleration
environment.shellInit = ''
  export MOZ_X11_EGL=1
'';

}
