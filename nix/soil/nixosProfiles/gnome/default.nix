{
  cell,
  lib,
  pkgs,
  ...
}:
let
  interfaceFontSize = "11";
in
{
  imports = [
    cell.nixosProfiles.dconf
    cell.nixosProfiles.english
    cell.nixosProfiles.xdg
  ];

  environment = {
    systemPackages = with pkgs; [ gnome.adwaita-icon-theme ];
  };

  i18n = {
    inputMethod = {
      enabled = "ibus";
    };
  };

  programs = {
    gnupg = {
      agent = {
        pinentryPackage = pkgs.pinentry-gnome3;
      };
    };
  };

  services = {
    gnome = {
      gnome-initial-setup = {
        enable = lib.mkForce false;
      };
    };

    udev = {
      packages = with pkgs; [ gnome.gnome-settings-daemon ];
    };

    xserver = {
      enable = true;

      desktopManager = {
        gnome = {
          enable = true;

          extraGSettingsOverrides =
            let
              cfg = config.fonts.fontconfig.english.defaultFont;
            in
            lib.mkDefault ''
              [org.gnome.desktop.interface]
              font-name='${cfg.sansSerif} ${interfaceFontSize}'
              document-font-name='${cfg.sansSerif} ${interfaceFontSize}'
              monospace-font-name='${cfg.monospace} ${interfaceFontSize}'
            '';
        };
      };

      displayManager = {
        gdm = {
          enable = true;
          wayland = true;
        };
      };
    };
  };
}
