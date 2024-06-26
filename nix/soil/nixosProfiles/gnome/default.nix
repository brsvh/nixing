{
  cell,
  config,
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
    cell.nixosProfiles.fcitx5
    cell.nixosProfiles.xdg
  ];

  environment = {
    systemPackages =
      (with pkgs.gnome; [ adwaita-icon-theme ])
      ++ (with pkgs.gnomeExtensions; [
        appindicator
        kimpanel
      ]);
  };

  programs = {
    gnupg = {
      agent = {
        pinentryPackage = pkgs.pinentry-gnome3;
      };
    };
  };

  qt = {
    enable = true;
    platformTheme = "gnome";
    style = "adwaita";
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

      excludePackages = with pkgs; [ xterm ];

      desktopManager = {
        gnome = {
          enable = true;

          extraGSettingsOverrides =
            let
              cfg = config.fonts.fontconfig.english;
            in
            lib.mkDefault ''
              [org.gnome.desktop.interface]
              font-name='${cfg.sansSerif} ${interfaceFontSize}'
              document-font-name='${cfg.sansSerif} ${interfaceFontSize}'
              monospace-font-name='${cfg.monospace} ${interfaceFontSize}'

              [org/gnome/mutter]
              dynamic-workspaces=true

              [org.gnome.shell]
              enabled-extensions=['kimpanel@kde.org', 'appindicatorsupport@rgcjonas.gmail.com', 'user-theme@gnome-shell-extensions.gcampax.github.com']
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
