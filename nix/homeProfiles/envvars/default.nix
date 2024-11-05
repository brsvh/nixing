{
  config,
  lib,
  ...
}:
let
  inherit (lib)
    mkAfter
    ;
in
{
  home = {
    sessionPath = [
      "${config.home.sessionVariables.XDG_BIN_HOME}"
    ];

    sessionVariables = {
      CARGO_HOME = "${config.xdg.dataHome}/cargo";
      CUDA_CACHE_PATH = "${config.xdg.cacheHome}/nv";
      FFMPEG_DATADIR = "${config.xdg.configHome}/ffmpeg";
      GOMODCACHE = "${config.xdg.cacheHome}/go";
      GOPATH = "${config.xdg.dataHome}/go";
      GRIP_HOME = "${config.xdg.configHome}/grip";
      NPM_CONFIG_USERCONFIG = "${config.xdg.configHome}/npm/npmrc";
      PLTUSERHOME = "${config.xdg.dataHome}/racket";
      PYTHONPYCACHEPREFIX = "${config.xdg.cacheHome}/python";
      PYTHONUSERBASE = "${config.xdg.dataHome}/python";
      PYTHON_HISTORY = "${config.xdg.stateHome}/python/history";
      XDG_BIN_HOME = "${config.home.homeDirectory}/.local/bin";
    };
  };

  systemd = {
    user = {
      sessionVariables = mkAfter config.home.sessionVariables;
    };
  };

  xdg = {
    enable = true;

    configFile = {
      "npm/npmrc".text = ''
        prefix=''${XDG_DATA_HOME}/npm
        cache=''${XDG_CACHE_HOME}/npm
        init-module=''${XDG_CONFIG_HOME}/npm/config/npm-init.js
        logs-dir=''${XDG_STATE_HOME}/npm/logs
      '';
    };
  };
}
