{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.workstation.audio;
in
{
  imports = [
    ./alsa.nix
    ./jack.nix
    ./pipewire.nix
  ];

  options.workstation.audio = {
    system = mkOption {
      type = types.enum [
        "pipewire"
        "pulseaudio"
      ];
      default = "pipewire";
      description = ''
        The flavour of audio system.
      '';
    };
  };
}
