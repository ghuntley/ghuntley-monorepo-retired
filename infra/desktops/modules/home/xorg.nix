# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ pkgs, config, lib, ... }: {
  home-manager.users.ghuntley = {

    home = {
      keyboard = {
        layout = "us(altgr-intl)";
        model = "pc104";
        options = [ "terminate:ctrl_alt_bksp" ];
      };
      sessionVariables = { TERMINAL = "${pkgs.alacritty}/bin/alacritty"; };
    };

    xsession = {
      enable = true;
      initExtra = ''
        ${pkgs.xorg.xrdb}/bin/xrdb -merge $HOME/.Xresources
      '';
      windowManager = {
        i3 = let mod = "Mod4";
        in {
          enable = true;
          config = {
            modifier = mod;
            #fonts = [ "DejaVu Sans Mono" "FontAwesome5Free 10" ];
            fonts = [ "Source Code Pro" "Source Code Pro 10" ];
            keybindings = lib.mkOptionDefault {
              "${mod}+d" = ''
                exec --no-startup-id "${pkgs.j4-dmenu-desktop}/bin/j4-dmenu-desktop'';
              "${mod}+Return" = "exec ${pkgs.alacritty}/bin/alacritty";
              "${mod}+Ctrl+l" =
                "exec --no-startup-id ${pkgs.i3lock}/bin/i3lock";

              "${mod}+j" = "focus left";
              "${mod}+k" = "focus down";
              "${mod}+l" = "focus up";
              "${mod}+semicolon" = "focus right";

              "${mod}+Shift+j" = "move left 40px";
              "${mod}+Shift+k" = "move down 40px";
              "${mod}+Shift+l" = "move up 40px";
              "${mod}+Shift+semicolon" = "move right 40px";

              "${mod}+a" = "focus parent";
              "${mod}+q" = "focus child";

              "${mod}+Shift+e" = "exit";
              "${mod}+apostrophe" = "mode app";
            };

            modes = lib.mkOptionDefault {
              resize = {
                "j" = "resize shrink width 10 px or 10 ppt";
                "k" = "resize grow height 10 px or 10 ppt";
                "l" = "resize shrink height 10 px or 10 ppt";
                "semicolon" = "resize grow width 10 px or 10 ppt";
                "${mod}+r" = "mode default";
              };
              app = {
                "d" = "exec ${pkgs.xfce.thunar}/bin/thunar; mode default";
                "f" = "exec ${pkgs.firefox}/bin/firefox; mode default";
                "c" = "exec ${pkgs.vscode}/bin/code; mode default";
                "v" = "exec ${pkgs.pavucontrol}/bin/pavucontrol; mode default";
                "${mod}+apostrophe" = "mode default";
                "Escape" = "mode default";
                "Return" = "mode default";
              };
            };
          };
        };
      };
    };

    gtk = {
      enable = true;
      theme = {
        package = pkgs.gnome3.gnome-themes-standard;
        name = "Adwaita";
      };
      font = {
        #        package = pkgs.dejavu_fonts;
        #        name = "DejaVu Sans 10";
        name = "Source Code Pro 10";
      };
      iconTheme = {
        package = pkgs.gnome3.adwaita-icon-theme;
        name = "Adwaita";
      };
    };
  };
}

