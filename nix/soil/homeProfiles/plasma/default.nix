{ config, pkgs, ... }:
{
  gtk = {
    enable = true;

    cursorTheme = {
      name = "breeze_cursors";
      package = pkgs.kdePackages.breeze;
      size = 24; # For 125% scaling.
    };

    font = {
      name = config.fonts.fontconfig.english.sansSerif;
      size = 11;
    };

    gtk2 = {
      configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";

      extraConfig = ''
        gtk-enable-animations = 1;
        gtk-primary-button-warps-slider = 1;
        gtk-toolbar-style = 3;
        gtk-menu-images = 1;
        gtk-button-images = 1;
        gtk-sound-theme-name = "ocean";
      '';
    };

    gtk3 = {
      extraConfig = {
        gtk-application-prefer-dark-theme = false;
        gtk-button-images = true;
        gtk-decoration-layout = "icon:minimize,maximize,close";
        gtk-enable-animations = true;
        gtk-menu-images = true;
        gtk-modules = "colorreload-gtk-module:window-decorations-gtk-module";
        gtk-primary-button-warps-slider = true;
        gtk-sound-theme-name = "ocean";
        gtk-toolbar-style = 3;
        gtk-xft-dpi = 122880;
      };
    };

    gtk4 = {
      extraConfig = {
        gtk-application-prefer-dark-theme = false;
        gtk-decoration-layout = "icon:minimize,maximize,close";
        gtk-enable-animations = true;
        gtk-modules = "colorreload-gtk-module:window-decorations-gtk-module";
        gtk-primary-button-warps-slider = true;
        gtk-sound-theme-name = "ocean";
        gtk-xft-dpi = "122880";
      };
    };

    iconTheme = {
      name = "breeze";
      package = pkgs.kdePackages.breeze-icons;
    };

    theme = {
      name = "Breeze";
      package = pkgs.kdePackages.breeze-gtk;
    };
  };

  home = {
    packages = with pkgs; [
      wezterm
    ];
  };

  qt = {
    enable = false;
    platformTheme = "kde";
    style = {
      package = with pkgs; kdePackages.breeze;
      name = "breeze";
    };
  };

  services = {
    gpg-agent = {
      pinentryPackage = pkgs.pinentry-qt;
    };
  };

  xdg = {
    configFile = {
      "gtkrc".text = ''
        include /run/current-system/sw/share/themes/Breeze/gtk-2.0/gtkrc
        include ${config.xdg.configHome}/gtk-2.0/gtkrc

        gtk-theme-name="Breeze"
        gtk-cursor-theme-name="Breeze"
      '';

      # REVIEW ~/.config/gtkrc-2.0 is not in my GTK2_RC_FILES .
      #        Should I include my gtk2 config?
      "gtkrc-2.0".text = ''
        gtk-alternative-button-order = 1
      '';
    };
  };
}
