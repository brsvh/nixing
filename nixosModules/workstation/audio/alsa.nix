{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.workstation.audio;

  withJack = cfg.enableJack;

  withPipewire = cfg.system == "pipewire";
  withPulseaudio = cfg.system == "pulseaudio";
in
{
  options.workstation.audio = {
    enableAlsa = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Use alsa or not.
      '';
    };
  };

  config = mkMerge [
    (mkIf cfg.enableAlsa {
      sound = {
        enable = true;
      };
    })
    (mkIf (cfg.enableAlsa && withPipewire) {
      services = {
        pipewire = {
          alsa = {
            enable = true;
            support32Bit = true;
          };
        };
      };
    })
    (mkIf (withPulseaudio && withJack) {
      services = {
        jack = {
          alsa = {
            enable = true;
          };
        };
      };
    })
  ];
}
