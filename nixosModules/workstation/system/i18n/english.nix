{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.workstation.system.i18n;

  primary = cfg.english.enable && cfg.english.primary;
in
{
  options.workstation.system.i18n = {
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

      defaultLocale = mkOption {
        type = types.str;
        default = "en_US.UTF-8";
        description = ''
          The default Locale of English. 
        '';
      };

      ISO-8859-1 = mkOption {
        type = types.bool;
        default = cfg.english.enable;
        example = "true";
        description = ''
          Whether to support English ISO-8859-1 locales.
        '';
      };

      ISO-8859-15 = mkOption {
        type = types.bool;
        default = cfg.english.enable;
        example = "true";
        description = ''
          Whether to support English ISO-8859-15 locales.
        '';
      };

      primary = mkOption {
        type = types.bool;
        default = true;
        example = "true";
        description = ''
          Whether to support English locales as default.
        '';
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.english.enable {
      i18n = {
        supportedLocales =
          [
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
          ++ (optionals cfg.english.ISO-8859-1 [
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
          ])
          ++ (optionals cfg.english.ISO-8859-15 [ "en_IE@euro/ISO-8859-15" ]);
      };
    })
    (mkIf primary {
      i18n = {
        defaultLocale = cfg.english.defaultLocale;
      };
    })
  ];
}
