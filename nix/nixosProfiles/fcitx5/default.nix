{
  environment = {
    sessionVariables = {
      QT_IM_MODULE = "fcitx";
      SDL_IM_MODULE = "fcitx";
      XMODIFIERS = "@im=fcitx";
    };
  };

  i18n = {
    inputMethod = {
      enable = true;
      type = "fcitx5";
    };
  };
}
