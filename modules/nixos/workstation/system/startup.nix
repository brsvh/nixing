{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.workstation.system;
in
{
  options = {
    workstation = {
      system = {
        startup = {
          quiet = mkOption {
            type = types.bool;
            default = false;
            description = ''
              Limit the verbosity of startup process.
            '';
          };

          plymouth = mkOption {
            type = types.bool;
            default = false;
            description = ''
              Enable plymouth support.

              Use BGRT theme anyway.
            '';
          };
        };
      };
    };
  };

  config = mkMerge
    [
      (
        mkIf cfg.startup.quiet
          {
            workstation = {
              system = {
                console = {
                  quiet = true;
                };
                initrd = {
                  quiet = true;
                };
                kernel = {
                  quiet = true;
                };
                startup = {
                  plymouth = true;
                };
              };
            };
          }
      )
      (
        mkIf cfg.startup.plymouth
          {
            boot = {
              plymouth = {
                enable = true;
                theme = "bgrt";
              };
            };
          }
      )
    ];
}
