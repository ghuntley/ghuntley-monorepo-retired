{ config, pkgs, ... }:

{

  # https://nixos.wiki/wiki/Bluetooth

  environment.systemPackages = with pkgs; [ bluez-tools ];

  hardware.bluetooth = {
    enable = true;
    extraConfig = ''
      [General]
      Enable=Source,Sink,Media,Socket
    '';
  };

  hardware.pulseaudio = {
    enable = true;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    extraConfig = ''
      load-module module-switch-on-connect
    '';
    package = pkgs.pulseaudioFull;
  };

  services.blueman.enable = true;
}
