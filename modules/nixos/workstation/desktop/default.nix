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

  withGnome3 = cfg.flavour == "gnome3";

  withDisplayServer = withWayland || withX11;
  withFlavour = withGnome3;
  withTouchpad = cfg.touchpad;
in
{
  imports =
    [
      ./gnome3.nix
      ./xdg.nix
    ];

  options = {
    workstation = {
      desktop = {
        flavour = mkOption {
          type = types.enum
            [
              "gnome3"
            ];
          default = "gnome3";
          description = ''
            The flavour of desktop environment.
          '';
        };

        displayServer = mkOption {
          type = types.enum
            [
              "wayland"
              "x11"
            ];
          default = "wayland";
          description = ''
            The default display server protocol.
          '';
        };

        keyboardLayout = mkOption {
          type = types.str;
          default = "us";
          description = ''
              The default keyboard layout, or multiple keyboard layouts
            separated by commas.
          '';
        };

        touchpad = mkOption {
          type = types.bool;
          default = true;
          description = ''
            Touchpad support.
          '';
        };
      };
    };
  };

  config = mkMerge
    [
      {
        services = {
          xserver = {
            xkb = {
              layout = config.workstation.desktop.keyboardLayout;
            };
          };
        };
      }
      (
        mkIf withDisplayServer
          {
            services = {
              xserver = {
                enable = true;
              };
            };
          }
      )
      (
        mkIf withTouchpad
          {
            services = {
              xserver = {
                libinput = {
                  enable = true;
                };
              };
            };
          }
      )
    ];
}
