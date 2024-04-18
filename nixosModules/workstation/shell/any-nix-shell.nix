{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.programs.any-nix-shell;

  initOption = if cfg.interactiveOnly then "interactiveShellInit" else "shellInit";
in
{
  options = {
    programs = {
      any-nix-shell = {
        enable = mkOption {
          type = types.bool;
          default = false;
          description = ''
            Whether use any-nix-shell.
          '';
        };

        interactiveOnly = mkOption {
          default = true;
          example = false;
          type = types.bool;
          description = ''
            Whether to enable any-nix-shell only in interactive shell.
          '';
        };
      };
    };
  };

  config = mkIf cfg.enable {
    environment = {
      systemPackages = [ pkgs.any-nix-shell ];
    };

    programs = {
      fish.${initOption} = ''
        any-nix-shell fish --info-right | source
      '';
    };
  };
}
