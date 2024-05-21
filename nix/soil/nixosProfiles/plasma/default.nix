{ pkgs, ... }:
{
  environment = {
    systemPackages = with pkgs.kdePackages; [
      akonadi
      kaccounts-integration
      kaccounts-providers
      kdepim-addons
      kdepim-runtime
      kleopatra
    ];
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
          enable = withWayland;
        };
      };
    };
  };
}
