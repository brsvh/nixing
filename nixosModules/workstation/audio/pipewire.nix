{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.workstation.audio;

  withPipewire = cfg.system == "pipewire";
in
{
  config = mkIf withPipewire {
    hardware = {
      pulseaudio = {
        enable = mkForce false;
      };
    };

    services = {
      pipewire = {
        enable = true;
        pulse = {
          enable = true;
        };
      };
    };
  };
}
