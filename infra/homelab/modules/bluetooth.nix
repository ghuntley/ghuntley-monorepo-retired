{ pkgs, config, lib, ... }: {

  environment.systemPackages = with pkgs; [ bluez-tools ];

  hardware.bluetooth = {
    enable = true;
    extraConfig = ''
      [General]
      Enable=Source,Sink,Media,Socket
    '';
    powerOnBoot = true;
    package = pkgs.bluezFull
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
