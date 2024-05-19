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
