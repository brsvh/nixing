{ config
, lib
, pkgs
, ...
}:
with builtins;
with lib;
let
  user = config.home.username;
  profile = config.accounts.email.accounts."${user}";
  host = elemAt (strings.split "@" profile.address) 2;
in
{
  accounts = {
    email = {
      accounts = {
        "${user}" = {
          primary = true;
          realName = "Burgess Chang";
          address = "bsc@brsvh.org";
          userName = "bsc@brsvh.org";
          passwordCommand =
            "pass ${host}/mail/${profile.userName}";

          aliases =
            [
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

          offlineimap = {
            enable = true;
            extraConfig = {
              account = {
                autorefresh = 5;
                quick = 10;
              };

              local = {
                sync_deletes = true;
              };

              remote = {
                maxconnections = 2;
              };
            };
          };

          mu = {
            enable = true;
          };
        };
      };

      maildirBasePath = "${config.xdg.dataHome}/Mail/";
    };
  };

  home = {
    sessionVariables = {
      MAILDIR = "${config.accounts.email.maildirBasePath}";
    };
  };

  programs = {
    offlineimap = {
      enable = profile.offlineimap.enable;
    };
  };

  services = {

    offlineimap = {
      enable = profile.offlineimap.enable;
    };
  };
}
