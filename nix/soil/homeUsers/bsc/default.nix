{
  cell,
  config,
  pkgs,
  ...
}:
let
  fullname = "Burgess Chang";
  usermail = "bsc@brsvh.org";
  username = "bsc";
in
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
        "${fullname}" = rec {
          address = usermail;

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

          passwordCommand = ''
            pass ${imap.host}/${userName}"
          '';

          primary = true;
          realName = fullname;

          smtp = {
            host = "smtppro.zoho.com";
            port = 465;

            tls = {
              enable = true;
            };
          };

          thunderbird = {
            enable = true;
            profiles = [ "${fullname}" ];
          };

          userName = usermail;
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

    stateVersion = "24.11";
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

    thunderbird = {
      profiles = {
        "${fullname}" = {
          isDefault = true;
          withExternalGnupg = true;
        };
      };
    };
  };
}
