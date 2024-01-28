{ config
, lib
, pkgs
, ...
}:
with lib;
let
  prog = config.programs.offlineimap;
  cfg = config.services.offlineimap;
in
{
  options = {
    services = {
      offlineimap = {
        enable = mkOption {
          type = types.bool;
          default = false;
          description = ''
            Whether to use offlineimap service.
          '';
        };

        timeoutStartSec = mkOption {
          type = types.str;
          default = "120sec";
          description = ''
            How long waiting for offlineimap before killing it.
            Default is '120sec' meaning every 2 minutes.  See
            systemd.time(7) for more information about the format.
          '';
        };

        onCalendar = mkOption {
          type = types.str;
          default = "*:0/3";
          description = ''
            How often is offlineimap started.
            Default is '*:0/3' meaning every 3 minutes.  See
            systemd.time(7) for more information about the format.
          '';
        };
      };
    };
  };

  config = mkIf cfg.enable
    {
      programs = {
        offlineimap = {
          enable = true;
        };
      };

      systemd = {
        user = {
          services = {
            offlineimap = {
              Unit = {
                Description = "Offlineimap: a software to dispose your mailbox(es) as a local Maildir(s)";
              };
              Service = {
                Type = "oneshot";
                ExecStart = "${prog.package}/bin/offlineimap -u syslog -o -1";
                TimeoutStartSec = cfg.timeoutStartSec;
              };
            };
          };

          timers = {
            offlineimap = {
              Unit = {
                Description = "offlineimap timer";
              };

              Timer = {
                Unit = "offlineimap.service";
                OnCalendar = cfg.onCalendar;
                Persistent = "true";
              };

              Install = {
                WantedBy = [ "timers.target" ];
              };
            };
          };
        };
      };
    };
}
