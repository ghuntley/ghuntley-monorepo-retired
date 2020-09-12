{ pkgs, config, lib, ... }: {

  environment.systemPackages = with pkgs; [ tailsale ];

  services.tailscale.enable = true;

}
