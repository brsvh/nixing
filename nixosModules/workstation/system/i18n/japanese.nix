{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.workstation.system.i18n;

  primary = cfg.japanese.enable && cfg.japanese.primary;
in
{
  options.workstation.system.i18n = {
    japanese = {
      enable = mkOption {
        type = types.bool;
        default = true;
        example = "true";
        description = ''
          Whether to support Japanese locales.

          Default enable all Japanese UTF-8 locales supported by
          Glibc.
        '';
      };

      defaultLocale = mkOption {
        type = types.str;
        default = "ja_JP.UTF-8";
        description = ''
          The default Locale of Japanese. 
        '';
      };

      EUC = mkOption {
        type = types.bool;
        default = cfg.japanese.enable;
        example = "true";
        description = ''
          Whether to support Japanese EUC locales.
        '';
      };

      primary = mkOption {
        type = types.bool;
        default = false;
        example = "true";
        description = ''
          Whether to support Japanese locales as default.
        '';
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.japanese.enable {
      i18n = {
        supportedLocales = [
          "ja_JP.UTF-8/UTF-8"
        ] ++ (optionals cfg.japanese.EUC [ "ja_JP.EUC-JP/EUC-JP" ]);
      };
    })
    (mkIf primary {
      i18n = {
        defaultLocale = cfg.japanese.defaultLocale;
      };
    })
  ];
}
