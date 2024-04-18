{
  config,
  lib,
  pkgs,
  ...
}:
with builtins;
with lib;
let
  fontName = lang: config.fonts."${lang}".monoFontName;

  fontSize = toString config.fonts.size;

  mailDir = config.accounts.email.maildirBasePath;

  user = config.home.username;

  profile = config.accounts.email.accounts."${user}";

  host = elemAt (strings.split "@" profile.address) 2;
in
{
  home = {
    sessionVariables = {
      EDITOR = "emacsclient -c";
    };
  };

  programs = {
    my-emacs = {
      enable = true;
      localConfig = ''
        (use-package emacs
          :no-require t
          :when (display-graphic-p)
          :init
          (set-face-attribute 'default nil :font "${fontName "english"} ${fontSize}")

          (set-fontset-font t 'cjk-misc "${fontName "chinese"} ${fontSize}")
          (set-fontset-font t 'han "${fontName "chinese"} ${fontSize}")
          (set-fontset-font t 'hangul "${fontName "korean"} ${fontSize}")
          (set-fontset-font t 'kana "${fontName "japanese"} ${fontSize}")
          (set-fontset-font t 'latin "${fontName "english"} ${fontSize}")
          (set-fontset-font t 'symbol "${fontName "english"} ${fontSize}"))

        (use-package startup
          :no-require t
          :init
          (setq mail-host-address "${host}"))

        (use-package smtpmail
          :config
          (setq send-mail-function 'smtpmail-send-it
                smtpmail-smtp-server "${profile.smtp.host}"
                smtpmail-smtp-service ${toString profile.smtp.port}
                smtpmail-smtp-user "${profile.userName}"
                smtpmail-stream-type ${if profile.smtp.tls.enable then "'ssl" else "nil"}
                smtpmail-local-domain "localdmain"
                smtpmail-queue-dir "${mailDir}/queued-mail/"))

        (use-package smime
          :config
          (setq smime-certificate-directory "${mailDir}/certs/"))
      '';
      mail.signature = ''
        Burgess Chang
        Pronouns: He/Him/His
        OpenPGP: 7B740DB9F2AC6D3B226BC53078D74502D92E0218
      '';
      serviceIntegration = true;
      windowSystem = "pgtk";
    };
  };
}
