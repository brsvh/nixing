{
  cell,
  lib,
  pkgs,
  ...
}:
{
  imports = [ cell.nixosProfiles.dconf ];

  environment = {
    systemPackages = with pkgs; [ gnome.adwaita-icon-theme ];
  };

  services = {
    gnome = {
      gnome-initial-setup = {
        enable = lib.mkForce false;
      };
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
