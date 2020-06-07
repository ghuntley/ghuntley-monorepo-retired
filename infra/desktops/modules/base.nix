# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ config, pkgs, lib, stdenv, ... }: {
  imports = [
    <home-manager/nixos>
    #  ./sys/initrd-ssh.nix
    ./apps/gist
    ./apps/nvim
    ./apps/zsh
    ./users
    ./services/firewall
    ./services/mosh
    ./services/sshd
    ./services/syncthing
    ./services/tailscale
    ./services/xrdp
    ./sys/corsair
  ];

  nixpkgs = {
    config = {
      allowUnfree = true;
      packageOverrides = pkgs: {
        xterm-24bit = pkgs.callPackage ./sys/xterm-24bit.nix { };
      };
    };
  };

  nix = {
    buildCores = 0;
    maxJobs = lib.mkDefault 4;
    autoOptimiseStore = true;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
    nixPath = [ "/nix" "nixos-config=/etc/nixos/configuration.nix" ];
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://all-hies.cachix.org"
      "https://nixcache.reflex-frp.org"
      "https://hercules-ci.cachix.org"
    ];
    binaryCachePublicKeys = [
      "hie-nix.cachix.org-1:EjBSHzF6VmDnzqlldGXbi0RM3HdjfTU3yDRi9Pd0jTY="
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
      "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
    ];
    trustedUsers = [ "root" "ghuntley" ];
  };

  console = { keyMap = "us"; };

  i18n = { defaultLocale = "en_US.UTF-8"; };

  #boot = { kernelPackages = pkgs.linuxPackagesFor pkgs.linux_5_4; };

  hardware = {
    enableRedistributableFirmware = true;
    cpu = {
      amd.updateMicrocode = true;
      intel.updateMicrocode = true;
    };

    pulseaudio = {
      enable = true;
      support32Bit = true;
    };

  };

  environment.systemPackages = with pkgs; [
    # lorri
    # niv
    ack
    alacritty
    arandr
    autorandr
    bind
    cifs-utils
    cryptsetup
    direnv
    dmenu
    emacs
    exfat
    feh
    file
    git-crypt
    gitFull
    gnumake
    gnupg
    gptfdisk
    hdparm
    htop
    i3lock
    iotop
    jnettop
    j4-dmenu-desktop
    ncftp
    nixfmt
    nix-prefetch-scripts
    nmap
    nvme-cli
    okular
    patchelf
    pciutils
    pinentry
    powertop
    psmisc
    python
    rclone
    restic
    samba
    silver-searcher
    smartmontools
    speedtest-cli
    stow
    sysstat
    termite.terminfo
    terminator
    tmux
    tree
    unzip
    upower
    wget
    xorg.xrdb
    xterm-24bit
    wget
    vscode
  ];

  time.timeZone = "Australia/Brisbane";

  services = {

    fwupd.enable = true;

    fstrim.enable = true;

    timesyncd.enable = true;

    xserver = {
      enable = true;
      libinput.enable = true;

      # displayManager.defaultSession = "plasma5";
      # desktopManager.plasma5.enable = true;

      desktopManager = {
        default = "xfce";
        xterm.enable = false;
        xfce = {
          enable = true;
          noDesktop = true;
          enableXfwm = false;
        };
      };

      displayManager.lightdm.enable = true;
      displayManager.lightdm.autoLogin = {
        enable = true;
        user = "ghuntley";
      };
      displayManager.session = [{
        manage = "desktop";
        name = "xsession";
        start = ''
          ${pkgs.runtimeShell} $HOME/.xsession &
          waitPID=$!
        '';
      }];

    };
  };

  programs = {
    ssh = {
      startAgent = true;
      extraConfig = ''
        AddKeysToAgent yes
      '';
    };

    gnupg.agent.enable = true;

    iftop.enable = true;

    iotop.enable = true;

    mtr.enable = true;
  };

  security = {
    sudo = {
      enable = true;
      wheelNeedsPassword = false;
    };
  };

  system.stateVersion = "20.03";
}

