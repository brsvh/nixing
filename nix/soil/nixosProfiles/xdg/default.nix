{
  environment = {
    sessionVariables = {
      XDG_CACHE_HOME = "$HOME/.cache";
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_DATA_HOME = "$HOME/.local/share";
      XDG_STATE_HOME = "$HOME/.local/state";

      GNUPGHOME = "$XDG_STATE_HOME/gnupg";
      XCOMPOSECACHE = "$XDG_CACHE_HOME/X11/xcompose";
    };
  };

  xdg = {
    portal = {
      enable = true;
    };
  };
}
