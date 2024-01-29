{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.workstation.system.i18n;

  primary = cfg.chinese.enable && cfg.chinese.primary;
in
{
  options.workstation.system.i18n = {
    chinese = {
      enable = mkOption {
        type = types.bool;
        default = true;
        example = "true";
        description = ''
          Whether to support Chinese locales.

          Default enable all Chinese UTF-8 locales supported by
          Glibc.
        '';
      };

      defaultLocale = mkOption {
        type = types.str;
        default = "zh_CN.UTF-8";
        description = ''
          The default Locale of Chinese. 
        '';
      };

      BIG-5 = mkOption {
        type = types.bool;
        default = cfg.chinese.enable;
        example = "true";
        description = ''
          Whether to support Chinese BIG-5 locales.
        '';
      };

      EUC = mkOption {
        type = types.bool;
        default = cfg.chinese.enable;
        example = "true";
        description = ''
          Whether to support Chinese EUC locales.
        '';
      };

      GB18030 = mkOption {
        type = types.bool;
        default = cfg.chinese.enable;
        example = "true";
        description = ''
          Whether to support Chinese GB18030 locales.
        '';
      };

      GB2312 = mkOption {
        type = types.bool;
        default = cfg.chinese.enable;
        example = "true";
        description = ''
          Whether to support Chinese GB2312 locales.
        '';
      };

      GBK = mkOption {
        type = types.bool;
        default = cfg.chinese.enable;
        example = "true";
        description = ''
          Whether to support Chinese GBK locales.
        '';
      };

      primary = mkOption {
        type = types.bool;
        default = false;
        example = "true";
        description = ''
          Whether to support Chinese locales as default.
        '';
      };
    };
  };

  config = mkMerge
    [
      (
        mkIf cfg.chinese.enable
          {
            i18n = {
              supportedLocales =
                [
                  "zh_CN.UTF-8/UTF-8"
                  "zh_HK.UTF-8/UTF-8"
                  "zh_SG.UTF-8/UTF-8"
                  "zh_TW.UTF-8/UTF-8"
                ] ++ (
                  optionals cfg.chinese.BIG-5
                    [
                      "zh_HK/BIG5-HKSCS"
                      "zh_TW/BIG5"
                    ]
                ) ++ (
                  optionals cfg.chinese.EUC
                    [
                      "zh_TW.EUC-TW/EUC-TW"
                    ]
                ) ++ (
                  optionals cfg.chinese.GB18030
                    [
                      "zh_CN.GB18030/GB18030"
                    ]
                ) ++ (
                  optionals cfg.chinese.GB2312
                    [
                      "zh_CN/GB2312"
                      "zh_SG/GB2312"
                    ]
                ) ++ (
                  optionals cfg.chinese.GBK
                    [
                      "zh_CN.GBK/GBK"
                      "zh_SG.GBK/GBK"
                    ]
                );
            };
          }
      )
      (
        mkIf primary
          {
            i18n = {
              defaultLocale = cfg.chinese.defaultLocale;
            };
          }
      )
    ];
}
