{
  environment = {
    sessionVariables = {
      QT_IM_MODULE = "ibus";
      SDL_IM_MODULE = "ibus";
      XMODIFIERS = "@im=ibus";
    };
  };

  i18n = {
    inputMethod = {
      enable = true;
      type = "ibus";
    };
  };
}
