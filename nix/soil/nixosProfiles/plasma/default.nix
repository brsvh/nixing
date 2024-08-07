{ pkgs, ... }:
{
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
    ];
  };

  i18n = {
    inputMethod = {
      enabled = "fcitx5";
    };
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
