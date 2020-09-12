{ pkgs, config, lib, ... }: {
  virtualisation.vmware.guest.enable = true;
  services.haveged.enable = true;
}
