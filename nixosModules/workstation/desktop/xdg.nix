{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.workstation.desktop;
in
{
  options.workstation.desktop = {
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
      type = types.str;
      default = "$HOME/.local/bin";
      description = ''
        Absolute path to directory holding application binary files.
      '';
    };

    cacheHome = mkOption {
      type = types.str;
      default = "$HOME/.cache";
      description = ''
        Absolute path to directory holding application caches.
      '';
    };

    configHome = mkOption {
      type = types.str;
      default = "$HOME/.config";
      description = ''
        Absolute path to directory holding application configurations.
      '';
    };

    dataHome = mkOption {
      type = types.str;
      default = "$HOME/.local/share";
      description = ''
        Absolute path to directory holding application data.
      '';
    };

    stateHome = mkOption {
      type = types.str;
      default = "$HOME/.local/state";
      description = ''
        Absolute path to directory holding application states.
      '';
    };
  };

  config = mkMerge [
    (
      let
        sessionVariables = {
          XDG_BIN_HOME = cfg.binHome;
          XDG_CACHE_HOME = cfg.cacheHome;
          XDG_CONFIG_HOME = cfg.configHome;
          XDG_DATA_HOME = cfg.dataHome;
          XDG_STATE_HOME = cfg.stateHome;
        };
      in
      mkIf cfg.XDGBaseDirectory.enable {
        environment = {
          inherit sessionVariables;
        };
      }
    )
  ];
}
