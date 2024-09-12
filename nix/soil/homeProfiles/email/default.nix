{ config, pkgs, ... }:
{
  accounts = {
    email = {
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

      settings = {
        "app.update.auto" = false;
        "widget.use-xdg-desktop-portal.file-picker" = 1;
      };
    };
  };
}
