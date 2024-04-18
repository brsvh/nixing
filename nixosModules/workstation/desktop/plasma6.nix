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

  withPlasma6 = cfg.flavour == "plasma6";

  extraPackages = with pkgs; [ xdg-desktop-portal-gtk ];

  extraPlasma6Packages = with pkgs.kdePackages; [
    akonadi
    dragon
    kaccounts-integration
    kaccounts-providers
    kdepim-addons
    kdepim-runtime
    kleopatra
    kmail
    kmail-account-wizard
    kontact
    merkuro
  ];
in
{
  config = mkMerge [
    (mkIf withPlasma6 {
      environment = {
        systemPackages = extraPackages ++ extraPlasma6Packages;
      };

      services = {
        desktopManager = {
          plasma6 = {
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
    })
  ];
}
