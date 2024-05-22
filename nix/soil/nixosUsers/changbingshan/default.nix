{ cell, pkgs, ... }:
let
  username = "changbingshan";
in
{
  imports = [ cell.nixosProfiles.fish ];

  home-manager = {
    backupFileExtension = "backup";

    users = {
      "${username}" = {
        imports = [
          cell.homeProfiles.chinese
          cell.homeProfiles.direnv
          cell.homeProfiles.english
          cell.homeProfiles.fish
          cell.homeProfiles.gnome
          cell.homeProfiles.gnupg
          cell.homeProfiles.google-chrome
          cell.homeProfiles.japanese
          cell.homeProfiles.korean
          cell.homeProfiles.my-emacs
          cell.homeProfiles.xdg
        ];

        home = {
          inherit username;

          homeDirectory = "/home/${username}";

          packages = with pkgs; [ wemeet ];

          stateVersion = "24.05";
        };

        programs = {
          my-emacs = {
            variant = "x11";
          };
        };
      };
    };
  };

  users = {
    mutableUsers = true;

    users = {
      "${username}" = {
        description = "Bingshan Chang";

        extraGroups = [
          "audio"
          "jackaudio"
          "libvirtd"
          "wheel"
          "networkmanager"
        ];

        initialHashedPassword = "$6$cB3EK3Lynl./0Bio$bgH7P93D1lpgvEIJ3iks7Dk2IyNue7ria2aH8.xkZZ1PPooxkb7p/bEMN1UtJaV0TIeVr/eTY8oAN38vBgMKe0";
        isNormalUser = true;

        openssh = {
          authorizedKeys = {
            keys = [
              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF8kaNm6jre8kIxo3LSWe9Ohz8tXc4jRcSCw1BeoC7tj bsc@eustoma"
            ];
          };
        };

        shell = pkgs.fish;
      };
    };
  };
}
