{
  config,
  lib,
  ...
}:
let
  inherit (builtins)
    attrValues
    filter
    length
    ;

  inherit (lib)
    mdDoc
    mkIf
    mkMerge
    mkOption
    types
    ;

  mkLangOptions = lang: locale: {
    enable = mkOption {
      default = false;

      description = mdDoc ''
        Whether to enable ${lang} support.
      '';

      type = types.bool;
    };

    isDefault = mkOption {
      default = false;

      description = mdDoc ''
        Whether to set ${lang} as the default language of system.
      '';

      type = types.bool;
    };

    defaultLocale = mkOption {
      type = types.str;
      default = "${locale}";
      example = "nl_NL.UTF-8";
      description = ''
        The default locale of ${lang}.

        It determines the language for program messages, the format for dates
        and times, sort order, and so on. It also determines the character
        set, such as UTF-8.
      '';
    };
  };
in
{
  options = {
    i18n = {
      languages = {
        chinese = mkLangOptions "chinese" "zh_CN.UTF-8";

        english = mkLangOptions "english" "en_US.UTF-8";
      };
    };
  };

  config = mkMerge [
    {
      assertions =
        let
          defaultLanguages =
            let
              ifDefault = lang: lang.enable && lang.isDefault;

              langs = attrValues config.i18n.languages;
            in
            length (filter ifDefault langs);
        in
        [
          {
            assertion = defaultLanguages == 1;
            message = "Must have exactly one default one `config.i18n.languages.<lang>.isDefault` have be set.";
          }
        ];
    }
    (mkIf config.i18n.languages.chinese.enable {
      i18n = {
        supportedLocales = [
          # BIG-5
          "zh_HK/BIG5-HKSCS"
          "zh_TW/BIG5"

          # EUC
          "zh_TW.EUC-TW/EUC-TW"

          # GB18030
          "zh_CN.GB18030/GB18030"

          # GB2312
          "zh_CN/GB2312"
          "zh_SG/GB2312"

          # GBK
          "zh_CN.GBK/GBK"
          "zh_SG.GBK/GBK"

          # UTF-8
          "zh_CN.UTF-8/UTF-8"
          "zh_HK.UTF-8/UTF-8"
          "zh_SG.UTF-8/UTF-8"
          "zh_TW.UTF-8/UTF-8"
        ];
      };
    })
    (mkIf config.i18n.languages.chinese.isDefault {
      i18n = {
        defaultLocale = config.i18n.languages.chinese.defaultLocale;
      };
    })
    (mkIf config.i18n.languages.english.enable {
      i18n = {
        supportedLocales = [
          # ISO-8859-1
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

          # ISO-8859-15
          "en_IE@euro/ISO-8859-15"

          # UTF-8
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
        ];
      };
    })
    (mkIf config.i18n.languages.english.isDefault {
      i18n = {
        defaultLocale = config.i18n.languages.english.defaultLocale;
      };
    })
  ];
}
