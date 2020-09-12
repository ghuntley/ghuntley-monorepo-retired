{ pkgs, ... }: {

  services.openssh.enable = true;
  services.openssh.passwordAuthentication = true;

  users.users.mgmt = {
    extraGroups = [ "wheel" ];
    isNormalUser = true;
    password = "hunter2";
  };
}
