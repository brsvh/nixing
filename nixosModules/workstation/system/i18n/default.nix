{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.workstation.system.i18n;
in
{
  imports =
    [
      ./chinese.nix
      ./english.nix
      ./japanese.nix
      ./korean.nix
    ];

  options.workstation.system.i18n = {
    default = mkOption {
      type = types.enum
        [
          "chinese"
          "english"
          "japanese"
          "korean"
        ];
      default = "english";
      example = "chinese";
      description = ''
        The default language.
      '';
    };

    settings = mkOption {
      type = types.attrsOf types.str;
      default = { };
      example = {
        LC_MESSAGES = "en_US.UTF-8";
        LC_TIME = "de_DE.UTF-8";
      };
      description = ''
        A set of additional system-wide locale settings other than
        `LANG` which can be configured with
        `config.i18n.extraLocaleSettings`.
      '';
    };
  };

  config = mkMerge
    [
      {
        i18n = {
          extraLocaleSettings = cfg.settings;
          supportedLocales =
            [
              "C.UTF-8/UTF-8"
            ];
        };
      }
    ];
}
