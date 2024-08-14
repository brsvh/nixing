{ cell, pkgs, ... }:
{
  imports = [
    cell.nixosProfiles.dconf
    cell.nixosProfiles.english
    cell.nixosProfiles.fcitx5
    cell.nixosProfiles.xdg
  ];

  environment = {
    sessionVariables = {
      NIXOS_OZONE_WL = "1";
    };

    systemPackages = with pkgs.kdePackages; [
      akonadi
      kaccounts-integration
      kaccounts-providers
      kdepim-addons
      kdepim-runtime
      kleopatra
      xdg-desktop-portal-kde
    ];
  };

  programs = {
    gnupg = {
      agent = {
        pinentryPackage = pkgs.pinentry-qt;
      };
    };
  };

  qt = {
    enable = true;
    platformTheme = "kde";
    style = "breeze";
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
          enable = true;
        };
      };
    };
  };
}
