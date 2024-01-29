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
  options.workstation.security = {
    rtkit = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Enable rtkit support.
        '';
      };
    };
  };

  config = mkIf
    cfg.rtkit.enable
    {
      security = {
        rtkit = {
          enable = true;
        };
      };
    };
}
