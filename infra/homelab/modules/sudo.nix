{ pkgs, config, lib, ... }: {
  security.sudo = {
    enable = true;
    extraConfig = "wheel ALL=(ALL:ALL) SETENV: ALL";
    wheelNeedsPassword = false;
  };
}
