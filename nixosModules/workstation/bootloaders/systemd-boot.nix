{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.workstation.bootloaders;

  withLanzaboote = config.boot.lanzaboote.enable;
in
{
  options.workstation.bootloaders = {
    systemd-boot = {
      enable = mkOption {
        type = types.bool;
        default = false;
        example = "true";
        description = ''
          Use systemd-boot as bootloader.
        '';
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.systemd-boot.enable && withLanzaboote) {
      boot = {
        loader = {
          systemd-boot = {
            enable = mkForce false;
          };
        };
      };
    })
    (mkIf (cfg.systemd-boot.enable && (!withLanzaboote)) {
      boot = {
        loader = {
          systemd-boot = {
            enable = true;
          };
        };
      };
    })
  ];
}
