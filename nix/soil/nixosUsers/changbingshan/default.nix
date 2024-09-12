{
  cell,
  inputs,
  pkgs,
  ...
}:
let
  fullname = "Bingshan Chang";
  usermail = "changbingshan@iscas.ac.cn";
  username = "changbingshan";
in
{
  imports = [ cell.nixosProfiles.fish ];

  home-manager = {
    backupFileExtension = "backup";

    users = {
      "${username}" =
        { config, ... }:
        {
          imports = [
            cell.homeProfiles.browser
            cell.homeProfiles.cachix
            cell.homeProfiles.chinese
            cell.homeProfiles.direnv
            cell.homeProfiles.english
            cell.homeProfiles.fish
            cell.homeProfiles.git
            cell.homeProfiles.gnome
            cell.homeProfiles.gnupg
            cell.homeProfiles.graphics
            cell.homeProfiles.japanese
            cell.homeProfiles.korean
            cell.homeProfiles.modules
            cell.homeProfiles.my-emacs
            cell.homeProfiles.node
            cell.homeProfiles.obs-studio
            cell.homeProfiles.office
            cell.homeProfiles.ssh
            cell.homeProfiles.texlive
            cell.homeProfiles.tools
            cell.homeProfiles.xdg
          ];

          accounts = {
            email = {
              accounts = {
                "${fullname}" =
                  let
                    address = "changbingshan@iscas.ac.cn";
                  in
                  rec {
                    inherit address;

                    gpg = {
                      key = "78D74502D92E0218";
                      signByDefault = true;
                    };

                    imap = {
                      host = "mail.cstnet.cn";
                      port = 993;

                      tls = {
                        enable = true;
                      };
                    };

                    passwordCommand = ''
                      pass ${imap.host}/${userName}"
                    '';

                    primary = true;
                    realName = fullname;

                    smtp = {
                      host = "mail.cstnet.cn";
                      port = 465;

                      tls = {
                        enable = true;
                      };
                    };

                    thunderbird = {
                      enable = true;
                      profiles = [ "${fullname}" ];
                    };

                    userName = address;
                  };
              };
            };
          };

          home = {
            inherit username;

            homeDirectory = "/home/${username}";

            packages = with pkgs; [
              feishu
              wemeet
            ];

            stateVersion = "24.05";
          };

          programs = {
            git = {
              signing = {
                key = "7B740DB9F2AC6D3B226BC53078D74502D92E0218";
                signByDefault = true;
              };

              userEmail = usermail;
              userName = fullname;
            };

            my-emacs = {
              userMail = usermail;
              userName = fullname;
            };

            password-store = {
              enable = true;
              settings = {
                PASSWORD_STORE_DIR = "${config.xdg.dataHome}/password-store";
              };
            };

            thunderbird = {
              profiles = {
                "${fullname}" = {
                  isDefault = true;
                  withExternalGnupg = true;
                };
              };
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
          "docker"
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
