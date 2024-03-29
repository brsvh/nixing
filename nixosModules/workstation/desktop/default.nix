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
      ./fonts.nix
      ./gnome3.nix
      ./gpg.nix
      ./input-method.nix
      ./plasma5.nix
      ./plasma6.nix
      ./xdg.nix
    ];

  options.workstation.desktop = {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether the graphical user interface is used.
      '';
    };

    flavour = mkOption {
      type = types.enum
        [
          "gnome3"
          "plasma5"
          "plasma6"
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

  config = mkMerge
    [
      {
        programs = {
          dconf = {
            enable = true;
          };
        };

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
