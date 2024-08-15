{ config, pkgs, ... }:
{
  accounts = {
    email = {
      accounts = {
        "Burgess Chang" = {
          passwordCommand = ''
            pass ${config.accounts.email.accounts."Burgess Chang".imap.host}/${
              config.accounts.email.accounts."Burgess Chang".userName
            }"
          '';
          primary = true;

          thunderbird = {
            enable = true;
            profiles = [ "Burgess Chang" ];
          };
        };

        "Bingshan Chang" = {
          passwordCommand = ''
            pass ${config.accounts.email.accounts."Bingshan Chang".imap.host}/${
              config.accounts.email.accounts."Bingshan Chang".userName
            }"
          '';

          thunderbird = {
            enable = true;
            profiles = [ "Bingshan Chang" ];
          };
        };
      };

      maildirBasePath = "${config.xdg.dataHome}/Mail";
    };
  };

  home = {
    packages = with pkgs; [ mailutils ];

    sessionVariables = {
      MAILDIR = "${config.accounts.email.maildirBasePath}";
    };
  };

  programs = {
    thunderbird = {
      enable = true;

      profiles = {
        "Burgess Chang" = {
          isDefault = true;
          withExternalGnupg = true;
        };

        "Bingshan Chang" = {
          withExternalGnupg = true;
        };
      };

      settings = {
        "app.update.auto" = false;
        "widget.use-xdg-desktop-portal.file-picker" = 1;
      };
    };
  };
}
