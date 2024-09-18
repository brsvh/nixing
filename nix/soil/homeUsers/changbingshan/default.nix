{
  cell,
  config,
  pkgs,
  ...
}:
let
  fullname = "Bingshan Chang";
  usermail = "changbingshan@iscas.ac.cn";
  username = "changbingshan";
in
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
    cell.homeProfiles.password
    cell.homeProfiles.office
    cell.homeProfiles.ssh
    cell.homeProfiles.texlive
    cell.homeProfiles.tools
    cell.homeProfiles.xdg
  ];

  accounts = {
    email = {
      accounts = {
        "${fullname}" = rec {
          address = usermail;

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

          signature = {
            text = ''
              ---
              Bingshan Chang
              Software Engineer, Institute of Software, Chinese Academy of Sciences

              Pronoun: He/Him/His
              Homepage: https://bsc@brsvh.org
              GPG: 7B74 0DB9 F2AC 6D3B 226B  C530 78D7 4502 D92E 0218
            '';
          };

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
      extraConfig =
        let
          mailProfile = config.accounts.email.accounts."${fullname}";
        in
        ''
          (setup emacs
            (:set
             user-full-name "${mailProfile.realName}"
             user-mail-address "${mailProfile.address}"))

          (setup message
            (:when-loaded
             (:set
              message-signature "${mailProfile.signature.text}")))
        '';
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
