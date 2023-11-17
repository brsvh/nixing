{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.workstation.security;
in
{
  options = {
    workstation = {
      security = {
        polkit = {
          enable = mkOption {
            type = types.bool;
            default = true;
            description = ''
              Enable polkit support.
            '';
          };
        };
      };
    };
  };

  config = mkIf
    cfg.polkit.enable
    {
      security = {
        polkit = {
          enable = true;
        };
      };
    };
}
