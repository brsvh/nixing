{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.workstation.shell;

  withFish = cfg.flavour == "fish";
in
{
  config = mkIf withFish
    {
      programs = {
        fish = {
          enable = true;
          interactiveShellInit = ''
            set fish_greeting
          '';
        };
      };
    };
}
