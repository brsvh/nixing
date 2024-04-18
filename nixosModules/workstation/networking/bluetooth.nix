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
    bluetooth = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = ''
          The bluetooth support.
        '';
      };

      boot = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Whether to powers up the default Bluetooth controller on boot.
        '';
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.bluetooth.enable {
      hardware = {
        bluetooth = {
          enable = true;
        };
      };
    })
    (mkIf (cfg.bluetooth.enable && cfg.bluetooth.boot) {
      hardware = {
        bluetooth = {
          powerOnBoot = true;
        };
      };
    })
  ];
}
