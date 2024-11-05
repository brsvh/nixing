{
  lib,
  my,
  pkgs,
  ...
}:
let
  inherit (lib)
    mkDefault
    mkForce
    ;
in
{
  imports = [
    my.nixosProfiles.dbus
    my.nixosProfiles.english
    my.nixosProfiles.ibus
    my.nixosProfiles.wayland
    my.nixosProfiles.xdg
  ];

  environment = {
    systemPackages =
      with pkgs;
      (
        [ adwaita-icon-theme ]
        ++ (with gnomeExtensions; [
          appindicator
          kimpanel
        ])
      );
  };

  programs = {
    gnupg = {
      agent = {
        pinentryPackage = mkForce pkgs.pinentry-gnome3;
      };
    };
  };

  qt = {
    enable = true;
    platformTheme = "gnome";
    style = "adwaita";
  };

  services = {
    dbus = {
      packages = with pkgs; [ gcr ];
    };

    gnome = {
      gnome-initial-setup = {
        enable = mkForce false;
      };
    };

    udev = {
      packages = with pkgs; [
        gnome-settings-daemon
      ];
    };

    xserver = {
      desktopManager = {
        gnome = {
          enable = true;

          extraGSettingsOverrides = mkDefault ''
            [org/gnome/shell]
            enabled-extensions=['kimpanel@kde.org', 'appindicatorsupport@rgcjonas.gmail.com', 'user-theme@gnome-shell-extensions.gcampax.github.com']
          '';
        };
      };

      displayManager = {
        gdm = {
          enable = true;
        };
      };

      enable = true;

      excludePackages = with pkgs; [
        xterm
      ];
    };
  };
}
