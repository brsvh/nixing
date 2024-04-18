{
  config,
  lib,
  pkgs,
  ...
}:
with builtins;
with lib;
let
  withGnome3 = false;
  withPlasma5 = false;
  withPlasma6 = true;

  fontSize = config.fonts.size;

  getSansFontName = lang: config.fonts."${lang}".sansFontName;
in
{
  gtk = mkMerge [
    (mkIf withGnome3 {
      enable = true;

      cursorTheme = {
        name = "Vanilla-DMZ";
        package = pkgs.vanilla-dmz;
        size = 16;
      };

      gtk2 = {
        configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
      };
    })
    (mkIf withPlasma6 {
      enable = true;

      cursorTheme = {
        name = "breeze_cursors";
        package = pkgs.kdePackages.breeze;
        size = 24; # For 125% scaling.
      };

      font = {
        name = getSansFontName "english";
        size = fontSize;
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
    })
  ];

  home = mkMerge [
    (mkIf withPlasma6 {
      packages = with pkgs.kdePackages; [

      ];
    })
  ];

  qt = mkIf (withPlasma6 || withPlasma5) {
    enable = false;
    platformTheme = "kde";
    style = {
      package = with pkgs; kdePackages.breeze;
      name = "breeze";
    };
  };
}
