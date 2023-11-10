{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.workstation.desktop;
in
{
  options = {
    workstation = {
      desktop = {
        xdg = {
          baseDirectories = mkOption {
            type = types.bool;
            default = true;
            description = ''
              Follow XDG Base Directory specification or not.
            '';
          };
        };
      };
    };
  };

  config = mkMerge
    [
      (
        mkIf cfg.xdg.baseDirectories
          {
            environment = {
              sessionVariables = {
                XDG_BIN_HOME = "$HOME/.local/bin";
                XDG_CACHE_HOME = "$HOME/.cache";
                XDG_CONFIG_HOME = "$HOME/.config";
                XDG_DATA_HOME = "$HOME/.local/share";
                XDG_STATE_HOME = "$HOME/.local/state";
              };
            };
          }
      )
    ];
}
