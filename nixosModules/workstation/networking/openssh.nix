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
    openssh = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether or not the openssh services is used.
        '';
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.openssh.enable {
      services = {
        openssh = {
          enable = true;
        };
      };
    })
  ];
}
