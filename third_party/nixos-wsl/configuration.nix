{ lib, pkgs, config, ... }:

with lib;

let
  defaultUser = "nixos";
  syschdemd = import ./syschdemd.nix { inherit lib pkgs config defaultUser; };
in
{  nixpkgs.config.allowUnfree = true; 

  imports = [
    <nixpkgs/nixos/modules/profiles/minimal.nix>
  ];

  # WSL is closer to a container than anything else
  boot.isContainer = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
     git rclone treesheets openjdk11 wget vim google-chrome kate vscode netbeans
  ];

  nix.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];


  environment.etc.hosts.enable = false;
  environment.etc."resolv.conf".enable = false;

  networking.dhcpcd.enable = false;

  users.users.${defaultUser} = {
    isNormalUser = true;
    hashedPassword ="$6$f7cdeHLa$rQU7w8/d0PgA16nufz9rHJWmwFJvw3O7vqDjiw3Gw4E5XRglsRCYx6C4XpJTcL0RByMDrUz2KKENjcqh3QIM..";
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };
  
  users.users.root = {
    shell = "${syschdemd}/bin/syschdemd";
    # Otherwise WSL fails to login as root with "initgroups failed 5"
    extraGroups = [ "root" ];
  };

  # Described as "it should not be overwritten" in NixOS documentation,
  # but it's on /run per default and WSL mounts /run as a tmpfs, hence
  # hiding the wrappers.
  security.wrapperDir = "/wrappers";

  security.sudo.wheelNeedsPassword = false;

  # Disable systemd units that don't make sense on WSL
  systemd.services."serial-getty@ttyS0".enable = false;
  systemd.services."serial-getty@hvc0".enable = false;
  systemd.services."getty@tty1".enable = false;
  systemd.services."autovt@".enable = false;

  systemd.services.firewall.enable = false;
  systemd.services.systemd-resolved.enable = false;
  systemd.services.systemd-udevd.enable = false;

  # Don't allow emergency mode, because we don't have a console.
  systemd.enableEmergencyMode = false;
  # Enable the X11 windowing system.
  services.xserver.xkbModel = "pc85";
  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e";

  # Enable touchpad support.
  services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;
  services.udev.packages = [ pkgs.android-udev-rules ];
}
