{
  cell,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    cell.nixosProfiles.dconf
    cell.nixosProfiles.xdg
  ];

  environment = {
    systemPackages = with pkgs; [ gnome.adwaita-icon-theme ];
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
        };
      };

      displayManager = {
        gdm = {
          enable = true;
        };
      };
    };
  };
}
