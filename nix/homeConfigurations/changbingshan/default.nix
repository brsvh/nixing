{
  config,
  my,
  ...
}:
{
  imports = [
    my.homeProfiles.cachix
    my.homeProfiles.chinese
    my.homeProfiles.direnv
    my.homeProfiles.emacs
    my.homeProfiles.email
    my.homeProfiles.english
    my.homeProfiles.envvars
    my.homeProfiles.feishu
    my.homeProfiles.fish
    my.homeProfiles.git
    my.homeProfiles.gnome
    my.homeProfiles.google-chrome
    my.homeProfiles.obs-studio
    my.homeProfiles.password
    my.homeProfiles.rnote
    my.homeProfiles.ssh
    my.homeProfiles.texlive
    my.homeProfiles.tools
    my.homeProfiles.wemeet
    my.homeProfiles.wps
  ];

  accounts = {
    email = {
      accounts = {
        "${config.home.username}" = rec {
          address = "changbingshan@iscas.ac.cn";

          gpg = {
            key = "7B740DB9F2AC6D3B226BC53078D74502D92E0218";
            signByDefault = true;
          };

          imap = {
            host = "mail.cstnet.cn";
            port = 993;

            tls = {
              enable = true;
            };
          };

          msmtp = {
            enable = true;
          };

          mu = {
            enable = true;
          };

          offlineimap = {
            enable = true;

            extraConfig = {
              account = {
                autorefresh = 20;
              };

              local = {
                sync_deletes = true;
              };
            };
          };

          passwordCommand = "pass show ${imap.host}/${userName}";

          primary = true;
          realName = "Bingshan Chang";

          signature = {
            text = ''
              ${realName}
              Software Engineer, Institute of Software, Chinese Academy of Sciences
              Pronoun: He/Him/His
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

          userName = address;
        };
      };
    };
  };

  home = {
    homeDirectory = "/home/${config.home.username}";
    stateVersion = "24.11";
    username = "changbingshan";
  };

  programs = {
    emacs = {
      extraInitConfig =
        let
          user = config.accounts.email.accounts."${config.home.username}";
        in
        ''
          (setup emacs
            (:set
             user-full-name "${user.realName}"
             user-mail-address "${user.address}"))

          (setup message
            (:when-loaded
              (:set
               message-signature "${user.signature.text}")))

          (setup mu4e
            (:autoload mu4e)
            (:with-map ctl-c-a-map
              (:keymap-set
               "m" #'mu4e))
            (:when-loaded
              (:set
               mu4e-sent-folder   "/${config.home.username}/Sent Items"
               mu4e-drafts-folder "/${config.home.username}/Drafts"
               mu4e-trash-folder  "/${config.home.username}/Trash")))

          (setup mu4e-update
            (:when-loaded
              (:set mu4e-get-mail-command "offlineimap -u basic -o || true")))

          (setup sendmail
            (:when-loaded
              (:set
               send-mail-function 'sendmail-send-it
               sendmail-program "msmtp")))

          (setup simple
            (:when-loaded
              (:set
               mail-user-agent 'mu4e-user-agent)))
        '';
    };

    git = {
      signing = {
        key = "7B740DB9F2AC6D3B226BC53078D74502D92E0218";
        signByDefault = true;
      };

      userEmail = "changbingshan@iscas.ac.cn";
      userName = "Bingshan Chang";
    };
  };
}