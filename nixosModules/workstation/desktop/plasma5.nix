{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.workstation.desktop;

  withWayland = cfg.displayServer == "wayland";
  withX11 = cfg.displayServer == "x11";

  withPlasma5 = cfg.flavour == "plasma5";
in
{
  config = mkMerge [
    (mkIf withPlasma5 {
      services = {
        xserver = {
          desktopManager = {
            plasma5 = {
              enable = true;
            };
          };

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
    })
    (mkIf (withPlasma5 && withWayland) {
      services = {
        xserver = {
          displayManager = {
            defaultSession = mkDefault "plasmawayland";
          };
        };
      };
    })
  ];
}
