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
        firewall = {
          enable = mkOption {
            type = types.bool;
            default = true;
            description = ''
              Enable firewall support.
            '';
          };
        };
      };
    };
  };

  config = mkIf
    cfg.firewall.enable
    {
      workstation = {
        networking = {
          firewall = {
            enable = true;
          };
        };
      };
    };
}
