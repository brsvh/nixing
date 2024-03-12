{ config
, lib
, pkgs
, ...
}:
with builtins;
with lib;
let
  fontName = lang:
    config.fonts."${lang}".monoFontName;

  fontSize = toString config.fonts.size;

  mailDir = config.accounts.email.maildirBasePath;

  user = config.home.username;

  profile = config.accounts.email.accounts."${user}";

  host = elemAt (strings.split "@" profile.address) 2;
in
{
  programs = {
    my-emacs = {
      enable = true;
      localConfig = ''
        (use-package emacs
          :no-require t
          :when (display-graphic-p)
          :init
          (set-fontset-font t 'cjk-misc "${fontName "chinese"} ${fontSize}")
          (set-fontset-font t 'han "${fontName "chinese"} ${fontSize}")
          (set-fontset-font t 'hangul "${fontName "korean"} ${fontSize}")
          (set-fontset-font t 'kana "${fontName "japanese"} ${fontSize}")
          (set-fontset-font t 'latin "${fontName "english"} ${fontSize}")
          (set-fontset-font t 'symbol "${fontName "english"} ${fontSize}"))
      '';
      serviceIntegration = true;
      windowSystem = "pgtk";
    };
  };
}
