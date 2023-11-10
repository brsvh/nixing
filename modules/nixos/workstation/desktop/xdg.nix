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
        XDGBaseDirectory = {
          enable = mkOption {
            type = types.bool;
            default = true;
            description = ''
              Follow XDG Base Directory specification or not.
            '';
          };
        };

        binHome = mkOption {
          type = types.path;
          defaultText = "~/.local/bin";
          apply = toString;
          description = ''
            Absolute path to directory holding application binary files.
          '';
        };

        cacheHome = mkOption {
          type = types.path;
          defaultText = "~/.cache";
          apply = toString;
          description = ''
            Absolute path to directory holding application caches.
          '';
        };

        configHome = mkOption {
          type = types.path;
          defaultText = "~/.config";
          apply = toString;
          description = ''
            Absolute path to directory holding application configurations.
          '';
        };

        dataHome = mkOption {
          type = types.path;
          defaultText = "~/.local/share";
          apply = toString;
          description = ''
            Absolute path to directory holding application data.
          '';
        };

        stateHome = mkOption {
          type = types.path;
          defaultText = "~/.local/state";
          apply = toString;
          description = ''
            Absolute path to directory holding application states.
          '';
        };
      };
    };
  };

  config = mkMerge
    [
      (
        let
          defaultBinHome = "$HOME/.local/bin";
          defaultCacheHome = "$HOME/.cache";
          defaultConfigHome = "$HOME/.config";
          defaultDataHome = "$HOME/.local/share";
          defaultStateHome = "$HOME/.local/state";

          sessionVariables = {
            XDG_BIN_HOME = cfg.XDGBaseDirectory.binHome;
            XDG_CACHE_HOME = cfg.XDGBaseDirectory.cacheHome;
            XDG_CONFIG_HOME = cfg.XDGBaseDirectory.configHome;
            XDG_DATA_HOME = cfg.XDGBaseDirectory.dataHome;
            XDG_STATE_HOME = cfg.XDGBaseDirectory.stateHome;
          };
        in
        mkIf cfg.XDGBaseDirectory.enable
          {
            environment = {
              inherit sessionVariables;
            };

            workstation = {
              desktop = {
                XDGBaseDirectory = {
                  binHome = mkDefault defaultBinHome;
                  cacheHome = mkDefault defaultCacheHome;
                  configHome = mkDefault defaultConfigHome;
                  dataHome = mkDefault defaultDataHome;
                  stateHome = mkDefault defaultStateHome;
                };
              };
            };
          }
      )
    ];
}
