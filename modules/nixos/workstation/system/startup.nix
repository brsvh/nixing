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
