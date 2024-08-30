{ config, lib, ... }:
{
  home = {
    preferXdgDirectories = true;

    sessionPath = [ "${config.home.sessionVariables.XDG_BIN_HOME}" ];

    sessionVariables = {
      CARGO_HOME = "${config.xdg.dataHome}/cargo";
      CUDA_CACHE_PATH = "${config.xdg.cacheHome}/nv";
      FFMPEG_DATADIR = "${config.xdg.configHome}/ffmpeg";
      GOMODCACHE = "${config.xdg.cacheHome}/go";
      GOPATH = "${config.xdg.dataHome}/go";
      GRIP_HOME = "${config.xdg.configHome}/grip";
      PLTUSERHOME = "${config.xdg.dataHome}/racket";
      PYTHONPYCACHEPREFIX = "${config.xdg.cacheHome}/python";
      PYTHONUSERBASE = "${config.xdg.dataHome}/python";
      PYTHON_HISTORY = "${config.xdg.stateHome}/python/history";
      XDG_BIN_HOME = "${config.home.homeDirectory}/.local/bin";
    };
  };

  systemd = {
    user = {
      sessionVariables = lib.mkDefault config.home.sessionVariables;
    };
  };

  xdg = {
    enable = true;

    userDirs = {
      enable = true;
      createDirectories = true;
      desktop = "${config.xdg.dataHome}/Desktop";
      documents = "${config.xdg.dataHome}/Documents";
      download = "${config.xdg.dataHome}/Downloads";
      music = "${config.xdg.dataHome}/Music";
      pictures = "${config.xdg.dataHome}/Pictures";
      publicShare = "${config.xdg.dataHome}/Public";
      templates = "${config.xdg.dataHome}/Templates";
      videos = "${config.xdg.dataHome}/Videos";
    };
  };
}
