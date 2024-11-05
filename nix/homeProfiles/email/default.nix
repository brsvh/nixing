{
  config,
  pkgs,
  ...
}:
{
  accounts = {
    email = {
      maildirBasePath = "${config.xdg.dataHome}/Mail";
    };
  };

  home = {
    packages = with pkgs; [
      mailutils
    ];

    sessionVariables = {
      MAILDIR = "${config.accounts.email.maildirBasePath}";
    };
  };

  programs = {
    msmtp = {
      enable = true;
    };

    mu = {
      enable = true;
    };

    offlineimap = {
      enable = true;
    };
  };

  systemd = {
    user = {
      services = {
        offlineimap = {
          Unit = {
            Description = "OfflineIMAP sync";
          };

          Service = {
            Type = "oneshot";
            ExecStart = "${config.programs.offlineimap.package}/bin/offlineimap -u basic -o";
          };
        };
      };

      timers = {
        offlineimap = {
          Unit = {
            Description = "OfflineIMAP sync";
          };

          Timer = {
            Unit = "offlineimap.service";
            OnCalendar = "*:0/5";
          };

          Install = {
            WantedBy = [
              "timers.target"
            ];
          };
        };
      };
    };
  };
}
