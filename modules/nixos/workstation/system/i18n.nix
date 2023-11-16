{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.workstation.system;
in
{
  options = {
    workstation = {
      system = {
        i18n = {
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

          defaultLocales = mkOption {
            type = types.attrsOf types.str;
            default = {
              chinese = "zh_CN.UTF-8";
              english = "en_US.UTF-8";
              japanese = "ja_JP.UTF-8";
              korean = "ko_KR.UTF-8";
            };
            example = { };
            description = ''
              The default locales of languages.
            '';
          };

          chinese = {
            enable = mkOption {
              type = types.bool;
              default = false;
              example = "true";
              description = ''
                Whether to support Chinese locales.

                Default enable all Chinese UTF-8 locales supported by
                Glibc.
              '';
            };

            BIG-5 = mkOption {
              type = types.bool;
              default = cfg.i18n.chinese.enable;
              example = "true";
              description = ''
                Whether to support Chinese BIG-5 locales.
              '';
            };

            EUC = mkOption {
              type = types.bool;
              default = cfg.i18n.chinese.enable;
              example = "true";
              description = ''
                Whether to support Chinese EUC locales.
              '';
            };

            GB18030 = mkOption {
              type = types.bool;
              default = cfg.i18n.chinese.enable;
              example = "true";
              description = ''
                Whether to support Chinese GB18030 locales.
              '';
            };

            GB2312 = mkOption {
              type = types.bool;
              default = cfg.i18n.chinese.enable;
              example = "true";
              description = ''
                Whether to support Chinese GB2312 locales.
              '';
            };

            GBK = mkOption {
              type = types.bool;
              default = cfg.i18n.chinese.enable;
              example = "true";
              description = ''
                Whether to support Chinese GBK locales.
              '';
            };
          };

          english = {
            enable = mkOption {
              type = types.bool;
              default = true;
              example = "true";
              description = ''
                Whether to support English locales.

                Default enable all English UTF-8 locales supported by
                Glibc.
              '';
            };

            ISO-8859-1 = mkOption {
              type = types.bool;
              default = cfg.i18n.english.enable;
              example = "true";
              description = ''
                Whether to support English ISO-8859-1 locales.
              '';
            };

            ISO-8859-15 = mkOption {
              type = types.bool;
              default = cfg.i18n.english.enable;
              example = "true";
              description = ''
                Whether to support English ISO-8859-15 locales.
              '';
            };
          };

          japanese = {
            enable = mkOption {
              type = types.bool;
              default = false;
              example = "true";
              description = ''
                Whether to support Japanese locales.

                Default enable all Japanese UTF-8 locales supported by Glibc.
              '';
            };

            default = mkOption {
              type = types.bool;
              default = false;
              example = "true";
              description = ''
                As the system default locale.
              '';
            };

            EUC = mkOption {
              type = types.bool;
              default = cfg.i18n.japanese.enable;
              example = "true";
              description = ''
                Whether to support Japanese EUC locales.
              '';
            };
          };

          korean = {
            enable = mkOption {
              type = types.bool;
              default = false;
              example = "true";
              description = ''
                Whether to support Korean locales.

                Default enable all Korean UTF-8 locales supported by Glibc.
              '';
            };

            EUC = mkOption {
              type = types.bool;
              default = cfg.i18n.korean.enable;
              example = "true";
              description = ''
                Whether to support Korean EUC locales.
              '';
            };
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
      };
    };
  };

  config = {
    i18n =
      mkMerge
        [
          {
            extraLocaleSettings = cfg.i18n.settings;
            supportedLocales =
              [
                "C.UTF-8/UTF-8"
              ]
              ++ optionals cfg.i18n.chinese.enable [
                "zh_CN.UTF-8/UTF-8"
                "zh_HK.UTF-8/UTF-8"
                "zh_SG.UTF-8/UTF-8"
                "zh_TW.UTF-8/UTF-8"
              ]
              ++ optionals cfg.i18n.chinese.BIG-5 [
                "zh_HK/BIG5-HKSCS"
                "zh_TW/BIG5"
              ]
              ++ optionals cfg.i18n.chinese.EUC [
                "zh_TW.EUC-TW/EUC-TW"
              ]
              ++ optionals cfg.i18n.chinese.GB18030 [
                "zh_CN.GB18030/GB18030"
              ]
              ++ optionals cfg.i18n.chinese.GB2312 [
                "zh_CN/GB2312"
                "zh_SG/GB2312"
              ]
              ++ optionals cfg.i18n.chinese.GBK [
                "zh_CN.GBK/GBK"
                "zh_SG.GBK/GBK"
              ]
              ++ optionals cfg.i18n.english.enable [
                "en_AU.UTF-8/UTF-8"
                "en_BW.UTF-8/UTF-8"
                "en_CA.UTF-8/UTF-8"
                "en_DK.UTF-8/UTF-8"
                "en_GB.UTF-8/UTF-8"
                "en_HK.UTF-8/UTF-8"
                "en_IE.UTF-8/UTF-8"
                "en_IL/UTF-8"
                "en_IN/UTF-8"
                "en_NG/UTF-8"
                "en_NZ.UTF-8/UTF-8"
                "en_PH.UTF-8/UTF-8"
                "en_SC.UTF-8/UTF-8"
                "en_SG.UTF-8/UTF-8"
                "en_US.UTF-8/UTF-8"
                "en_ZA.UTF-8/UTF-8"
                "en_ZM/UTF-8"
                "en_ZW.UTF-8/UTF-8"
              ]
              ++ optionals cfg.i18n.english.ISO-8859-1 [
                "en_AU/ISO-8859-1"
                "en_BW/ISO-8859-1"
                "en_CA/ISO-8859-1"
                "en_DK/ISO-8859-1"
                "en_GB/ISO-8859-1"
                "en_HK/ISO-8859-1"
                "en_IE/ISO-8859-1"
                "en_NZ/ISO-8859-1"
                "en_PH/ISO-8859-1"
                "en_SG/ISO-8859-1"
                "en_US/ISO-8859-1"
                "en_ZA/ISO-8859-1"
                "en_ZW/ISO-8859-1"
              ]
              ++ optionals cfg.i18n.english.ISO-8859-15 [
                "en_IE@euro/ISO-8859-15"
              ]
              ++ optionals cfg.i18n.japanese.enable [
                "ja_JP.UTF-8/UTF-8"
              ]
              ++ optionals cfg.i18n.japanese.EUC [
                "ja_JP.EUC-JP/EUC-JP"
              ]
              ++ optionals cfg.i18n.korean.enable [
                "ko_KR.UTF-8/UTF-8"
              ]
              ++ optionals cfg.i18n.korean.EUC [
                "ko_KR.EUC-KR/EUC-KR"
              ];
          }
          (
            mkIf (cfg.i18n.default == "english")
              {
                defaultLocale = cfg.i18n.defaultLocales.english;
              }
          )
          (
            mkIf (cfg.i18n.default == "chinese")
              {
                defaultLocale = cfg.i18n.defaultLocales.chinese;
              }
          )
          (
            mkIf (cfg.i18n.default == "japanese")
              {
                defaultLocale = cfg.i18n.defaultLocales.japanese;
              }
          )
          (
            mkIf (cfg.i18n.default == "korean")
              {
                defaultLocale = cfg.i18n.defaultLocales.korean;
              }
          )
        ];
  };
}
