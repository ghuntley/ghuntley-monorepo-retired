{ pkgs, config, lib, ... }: {
  home-manager.users.ghuntley = {

    home.keyboard = {
      layout = "us(altgr-intl)";
      model = "pc104";
      #      options = ["ctrl:swapcaps" "compose:ralt" "terminate:ctrl_alt_bksp"];
    };

    services = {
      pasystray.enable = true;

      redshift = {
        enable = true;
        provider = "geoclue2";
        tray = true;
      };

      screen-locker = {
        enable = true;
        lockCmd = "${pkgs.i3lock}/bin/i3lock -n -c 000000";
        xssLockExtraOptions = [ "-l" ];
      };

      compton = {
        enable = true;
        fade = true;
        fadeSteps = [ "0.08" "0.08" ];
      };

      network-manager-applet.enable = true;

      blueman-applet.enable = true;

      dunst = {
        enable = true;
        iconTheme = {
          package = pkgs.gnome3.adwaita-icon-theme;
          name = "Adwaita";
        };
        settings = {
          global = {
            geometry = "500x5-40+40";
            font = "DejaVu Sans 8";
            markup = "full";
            format = ''
              <b>%s</b>
              %b'';
            icon_position = "left";
            transparency = 25;
            background = "#222222";
            foreground = "#888888";
            timeout = 3;
            padding = 16;
            frame_width = 0;
            frame_color = "#aaaaaa";
            word_wrap = true;
            stack_duplicates = true;
            show_indicators = false;
          };
          shortcuts = {
            close = "mod4+slash";
            close_all = "mod4+shift+slash";
            history = "mod4+grave";
            context = "mod4+shift+grave";
          };
          urgency_low = {
            background = "#222222";
            foreground = "#888888";
            timeout = 5;
          };
          urgency_normal = {
            background = "#285577";
            foreground = "#ffffff";
            timeout = 5;
          };
          urgency_critical = {
            background = "#900000";
            foreground = "#ffffff";
            frame_color = "#ff0000";
          };
        };
      };

      options.hardware.ckb-next.enable = true;

      udiskie = {
        enable = true;
        automount = true;
        tray = "always";
      };
    };
  };
}
