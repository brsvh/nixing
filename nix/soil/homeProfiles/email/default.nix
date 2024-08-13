{ config, pkgs, ... }:
{
  accounts = {
    email = {
      accounts = {
        "${config.home.username}" = {
          passwordCommand = ''
            pass ${config.accounts.email.accounts."${config.home.username}".imap.host}/${
              config.accounts.email.accounts."${config.home.username}".userName
            }"
          '';
          primary = true;

          thunderbird = {
            enable = true;
            profiles = [ "${config.home.username}" ];
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
      enable = config.accounts.email.accounts."${config.home.username}".thunderbird.enable;

      profiles = {
        "${config.home.username}" = {
          isDefault = true;
          withExternalGnupg = true;
        };
      };
    };
  };
}
