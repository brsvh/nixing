{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.workstation.audio;

  withPipewire = cfg.system == "pipewire";
  withPulseaudio = cfg.system == "pulseaudio";
in
{
  options.workstation.audio = {
    enableJack = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Use jack or not.
      '';
    };
  };

  config = mkMerge
    [
      (
        mkIf (withPipewire && cfg.enableJack)
          {
            services = {
              pipewire = {
                jack = {
                  enable = true;
                };
              };
            };
          }
      )
      (
        mkIf (withPulseaudio && cfg.enableJack)
          {
            services = {
              jack = {
                jackd = {
                  enable = true;
                };
                loopback = {
                  enable = true;
                };
              };
            };
          }
      )
    ];
}
