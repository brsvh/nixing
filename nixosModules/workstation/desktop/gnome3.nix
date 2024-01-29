{ config
, lib
, pkgs
, ...
}:
with builtins;
with lib;
let
  cfg = config.workstation.desktop;

  withGnome3 = cfg.flavour == "gnome3";
in
{
  config = mkIf withGnome3 {
    environment = {
      systemPackages = with pkgs; [
        gnome.adwaita-icon-theme
      ];
    };

    services = {
      udev = {
        packages = with pkgs; [
          gnome.gnome-settings-daemon
        ];
      };

      xserver = {
        desktopManager = {
          gnome = {
            enable = true;

            extraGSettingsOverrides = mkDefault ''
              [org.gnome.desktop.interface]
              font-name='${cfg.fonts.sansFontName} ${toString cfg.fonts.size}'
              document-font-name='${cfg.fonts.sansFontName} ${toString cfg.fonts.size}'
              monospace-font-name='${cfg.fonts.monoFontName} ${toString cfg.fonts.size}'
            '';
          };
        };

        displayManager = {
          gdm = {
            enable = true;
          };
        };
      };
    };
  };
}
