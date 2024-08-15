{ cell, pkgs, ... }:
let
  fullname = "Burgess Chang";
  usermail = "bsc@brsvh.org";
  username = "bsc";
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
            cell.homeProfiles.email
            cell.homeProfiles.english
            cell.homeProfiles.fish
            cell.homeProfiles.git
            cell.homeProfiles.plasma
            cell.homeProfiles.gnupg
            cell.homeProfiles.graphics
            cell.homeProfiles.japanese
            cell.homeProfiles.korean
            cell.homeProfiles.modules
            cell.homeProfiles.my-emacs
            cell.homeProfiles.node
            cell.homeProfiles.obs-studio
            cell.homeProfiles.password
            cell.homeProfiles.ssh
            cell.homeProfiles.texlive
            cell.homeProfiles.tools
            cell.homeProfiles.xdg
            cell.homeSecrets.bsc
          ];

          accounts = {
            email = {
              accounts = {
                "Burgess Chang" =
                  let
                    address = "bsc@brsvh.org";
                  in
                  {
                    inherit address;

                    aliases = [
                      "open@brsvh.org"
                      "register@brsvh.org"
                    ];

                    gpg = {
                      key = "78D74502D92E0218";
                      signByDefault = true;
                    };

                    imap = {
                      host = "imappro.zoho.com";
                      port = 993;

                      tls = {
                        enable = true;
                      };
                    };

                    smtp = {
                      host = "smtppro.zoho.com";
                      port = 465;

                      tls = {
                        enable = true;
                      };
                    };

                    realName = fullname;
                    userName = address;
                  };

                "Bingshan Chang" =
                  let
                    address = "changbingshan@iscas.ac.cn";
                  in
                  {
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

                    smtp = {
                      host = "mail.cstnet.cn";
                      port = 465;

                      tls = {
                        enable = true;
                      };
                    };

                    realName = "Bingshan Chang";
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
            firefox = {
              profiles = {
                "${username}" = {
                  id = 0;
                  name = fullname;
                };
              };
            };

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
              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA4jP/6P5pGF7sTTUSB3TZfONcvuVnQj+L794Q5sUsKn changbingshan@lilac"
            ];
          };
        };

        shell = pkgs.fish;
      };
    };
  };
}
