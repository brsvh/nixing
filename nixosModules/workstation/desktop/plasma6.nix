{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.workstation.desktop;

  withWayland = cfg.displayServer == "wayland";
  withX11 = cfg.displayServer == "x11";

  withPlasma6 = cfg.flavour == "plasma6";
in
{
  config = mkMerge
    [
      (
        mkIf withPlasma6 {
          environment = {
            systemPackages = with pkgs;
              [
                xdg-desktop-portal-gtk
              ];
          };

          services = {
            desktopManager = {
              plasma6 = {
                enable = true;
              };
            };

            xserver = {
              displayManager = {
                sddm = {
                  enable = true;

                  wayland = {
                    enable = withWayland;
                  };
                };
              };
            };
          };
        }
      )
    ];
}
