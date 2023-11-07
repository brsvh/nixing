{ config
, lib
, pkgs
, ...
}:
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
