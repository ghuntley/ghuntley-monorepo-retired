{ config, pkgs, lib, ... }: {
  # https://github.com/NixOS/nixpkgs/issues/47932#issuecomment-447566095
  hardware.opengl.driSupport32Bit = true;

  # https://nixos.org/nixpkgs/manual/#sec-steam-play
  hardware.pulseaudio.support32Bit = true;
  hardware.steam-hardware.enable = true;

  environment.systemPackages = with pkgs; [ steam xorg.libxcb ];
}
