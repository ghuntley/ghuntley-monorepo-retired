{ config, lib, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [ gpsd ];

  services.gpsd = {
    enable = true;
    nowait = true;
  };
}
