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
  brsvh = {
    emacs = {
      enable = true;
      platform = "wayland";

      extraInitConfig = ''
        (use-package emacs
          :no-require t
          :init
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

        (use-package mail-source
          :config
          (setq mail-source-directory
                "${mailDir}"))

        (use-package message
          :init
          (setq message-directory "${mailDir}"))

        (use-package smtpmail
          :config
          (setq
           send-mail-function 'smtpmail-send-it
           smtpmail-smtp-server "${profile.smtp.host}"
           smtpmail-smtp-service ${toString profile.smtp.port}
           smtpmail-smtp-user "${profile.userName}"
           smtpmail-stream-type ${
             if profile.smtp.tls.enable
             then "'ssl"
             else "nil"
           }
           smtpmail-local-domain "localdmain"
           smtpmail-queue-dir "${mailDir}/queued-mail/"))

        (use-package smime
          :config
          (setq smime-certificate-directory "${mailDir}/certs/"))

        (use-package gnus
          :config
          (push '(nnimap "${profile.address}"
                         (nnimap-address "${profile.imap.host}")
                         (nnimap-server-port ${toString profile.imap.port})
                         (nnimap-user "${profile.userName}")
                         (nnimap-stream ${
                           if profile.smtp.tls.enable
                           then "ssl"
                           else "undecided"
                         }))
                gnus-secondary-select-methods))

        (use-package mu4e
          :config
          (setq mu4e-refile-folder "${user}/Archives.2024"))
      '';

      mail = {
        signature = ''
          Burgess Chang
          Pronouns: He/Him/His
        '';
      };

      overrides = {
        mu4e = _: _: {
          src = pkgs.mu.mu4e;
        };
      };
    };
  };
}
