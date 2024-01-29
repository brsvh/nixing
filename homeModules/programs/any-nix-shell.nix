{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.programs.any-nix-shell;
in
{
  options.programs.any-nix-shell = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to use any-nix-shell.
      '';
    };

    enableFishIntegration = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Fish integration.
      '';
    };
  };

  config = mkIf cfg.enable
    {
      home = {
        packages =
          [
            pkgs.any-nix-shell
          ];
      };

      programs = {
        fish.interactiveShellInit =
          mkIf cfg.enableFishIntegration
            ''
              any-nix-shell fish --info-right | source
            '';
      };
    };
}
