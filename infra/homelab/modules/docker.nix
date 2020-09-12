{ pkgs, config, lib, ... }: {
  virtualisation.docker = {
    enable = true;
    autoPrune.enable = true;
  };
}
