{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.workstation.shell;
in
{
  imports = [
    ./any-nix-shell.nix
    ./bash.nix
    ./fish.nix
  ];

  options.workstation.shell = {
    any-nix-shell = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether the any-nix-shell is used.
      '';
    };

    flavour = mkOption {
      type = types.enum [
        "bash"
        "fish"
      ];
      default = "bash";
      description = ''
        The flavour of system shell.
      '';
    };

    starship = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether the starship is used as shell prompt.
      '';
    };
  };

  config = mkMerge [
    {
      users = {
        defaultUserShell = pkgs."${cfg.flavour}";
      };
    }

    (mkIf cfg.starship {
      programs = {
        starship = {
          enable = true;
        };
      };
    })

    (mkIf cfg.any-nix-shell {
      programs = {
        any-nix-shell = {
          enable = true;
        };
      };
    })
  ];
}
