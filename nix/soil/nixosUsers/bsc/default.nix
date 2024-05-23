{ cell, pkgs, ... }:
let
  username = "bsc";

  fullname = "Burgess Chang";
in
{
  imports = [ cell.nixosProfiles.fish ];

  home-manager = {
    backupFileExtension = "backup";

    users = {
      "${username}" = {
        imports = [
          cell.homeModules.fonts
          cell.homeProfiles.chinese
          cell.homeProfiles.direnv
          cell.homeProfiles.english
          cell.homeProfiles.fish
          cell.homeProfiles.git
          cell.homeProfiles.gnome
          cell.homeProfiles.gnupg
          cell.homeProfiles.google-chrome
          cell.homeProfiles.japanese
          cell.homeProfiles.korean
          cell.homeProfiles.my-emacs
          cell.homeProfiles.obs-studio
          cell.homeProfiles.ssh
          cell.homeProfiles.texlive
          cell.homeProfiles.xdg
        ];

        home = {
          inherit username;

          homeDirectory = "/home/${username}";

          packages = with pkgs; [ wemeet ];

          stateVersion = "24.05";
        };

        programs = {
          git = {
            signing = {
              key = "7B740DB9F2AC6D3B226BC53078D74502D92E0218";
              signByDefault = true;
            };

            userEmail = "bsc@brsvh.org";
            userName = fullname;
          };
        };
      };
    };
  };

  users = {
    mutableUsers = true;

    users = {
      "${username}" = {
        description = fullname;

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
              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA4jP/6P5pGF7sTTUSB3TZfONcvuVnQj+L794Q5sUsKn changbingshan@lilac"
            ];
          };
        };

        shell = pkgs.fish;
      };
    };
  };
}