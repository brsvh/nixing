{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.workstation.video;
in
{
  options = {
    workstation = {
      video = {
        drivers = mkOption {
          type = types.listOf types.str;
          default =
            [
              "modesetting"
              "fbdev"
            ];
          description = ''
            The names of the video drivers will be used.
          '';
        };
      };
    };
  };

  config = mkMerge
    [
      {
        services = {
          xserver = {
            videoDrivers = cfg.drivers;
          };
        };
      }
    ];
}
