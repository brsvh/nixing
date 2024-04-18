{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.workstation.system.i18n;

  primary = cfg.korean.enable && cfg.korean.primary;
in
{
  options.workstation.system.i18n = {
    korean = {
      enable = mkOption {
        type = types.bool;
        default = true;
        example = "true";
        description = ''
          Whether to support Korean locales.

          Default enable all Korean UTF-8 locales supported by
          Glibc.
        '';
      };

      defaultLocale = mkOption {
        type = types.str;
        default = "ko_KR.UTF-8";
        description = ''
          The default Locale of Korean. 
        '';
      };

      EUC = mkOption {
        type = types.bool;
        default = cfg.korean.enable;
        example = "true";
        description = ''
          Whether to support Korean EUC locales.
        '';
      };

      primary = mkOption {
        type = types.bool;
        default = false;
        example = "true";
        description = ''
          Whether to support Korean locales as default.
        '';
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.korean.enable {
      i18n = {
        supportedLocales = [ "ko_KR.UTF-8/UTF-8" ] ++ (optionals cfg.korean.EUC [ "ko_KR.EUC-KR/EUC-KR" ]);
      };
    })
    (mkIf primary {
      i18n = {
        defaultLocale = cfg.korean.defaultLocale;
      };
    })
  ];
}
