{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.workstation.desktop;

  languageIsEnable = lang: config.workstation.system.i18n."${lang}".enable;

  languageIsDefault =
    lang: (languageIsEnable lang) && config.workstation.system.i18n."${lang}".primary;
in
{
  options.workstation.desktop.fonts = {
    sansFontName = mkOption {
      type = types.str;
      default = "Source Sans Pro";
      description = ''
        The font name of default Sans fonts.
      '';
    };

    serifFontName = mkOption {
      type = types.str;
      default = "Source Serif Pro";
      description = ''
        The font name of default Serif fonts.
      '';
    };

    monoFontName = mkOption {
      type = types.str;
      default = "Source Code Pro";
      description = ''
        The font name of default monospace fonts.
      '';
    };

    size = mkOption {
      type = types.int;
      default = 11;
      description = ''
        The default font size of fonts.
      '';
    };
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf (languageIsEnable "english") {
      environment = {
        systemPackages = with pkgs; [
          source-sans-pro
          source-serif-pro
          source-code-pro
        ];
      };
    })
    (mkIf ((languageIsEnable "chinese") || (languageIsEnable "japanese") || (languageIsEnable "korean"))
      {
        environment = {
          systemPackages = with pkgs; [
            source-han-sans
            source-han-serif
            source-han-mono
          ];
        };
      }
    )
    (mkIf
      ((languageIsDefault "chinese") || (languageIsDefault "japanese") || (languageIsDefault "korean"))
      {
        workstation = {
          desktop = {
            fonts = {
              sansFontName = mkDefault "Source Han Sans";
              serifFontName = mkDefault "Source Han Seif";
              monoFontName = mkDefault "Source Han Mono";
            };
          };
        };
      }
    )
  ]);
}
