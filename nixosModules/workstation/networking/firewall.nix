{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.workstation.networking;
in
{
  options.workstation.networking = {
    firewall = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether or not the default firewall is used.
        '';
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.firewall.enable {
      networking = {
        firewall = {
          enable = true;
        };
      };
    })
  ];
}
