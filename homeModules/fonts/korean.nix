{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.fonts.korean;

  enableFonts = config.fonts.enable;

  enableKorean = enableFonts && cfg.enable;
in
{
  options.fonts.korean = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to use Korean Fonts.
      '';
    };

    flavour = mkOption {
      type = types.enum
        [
          "Noto"
          "Plex"
          "Source"
        ];
      default = "Source";
      description = ''
        The flavour of sans, serif and monospace fonts.
      '';
    };

    sansFontName = mkOption {
      type = types.str;
      default = "Source Han Sans KR";
      description = ''
        The font name of default Sans fonts.
      '';
    };

    serifFontName = mkOption {
      type = types.str;
      default = "Source Han Serif KR";
      description = ''
        The font name of default Serif fonts.
      '';
    };

    monoFontName = mkOption {
      type = types.str;
      default = "Source Han Mono KR";
      description = ''
        The font name of default monospace fonts.
      '';
    };
  };

  config = mkMerge
    [
      (
        mkIf (enableKorean && cfg.flavour == "Noto")
          {
            fonts = {
              korean = {
                sansFontName = mkDefault "Noto Sans CJK KR";
                serifFontName = mkDefault "Noto Seif CJK KR";
                monoFontName = mkDefault "Noto Sans Mono CJK KR";
              };
            };

            home = {
              packages = with pkgs;
                [
                  noto-fonts
                  noto-fonts-cjk-sans
                  noto-fonts-cjk-serif
                ];
            };
          }
      )
      (
        mkIf (enableKorean && cfg.flavour == "Source")
          {
            fonts = {
              korean = {
                sansFontName = mkDefault "Source Han Sans KR";
                serifFontName = mkDefault "Source Han Seif KR";
                monoFontName = mkDefault "Source Han Mono KR";
              };
            };

            home = {
              packages = with pkgs;
                [
                  source-han-sans
                  source-han-serif
                  source-han-mono
                ];
            };
          }
      )
      (
        mkIf enableKorean
          {
            xdg = {
              configFile = {
                "fontconfig/conf.d/50-korean-fonts.conf".text = ''
                  <?xml version='1.0'?>
                  <!DOCTYPE fontconfig SYSTEM 'urn:fontconfig:fonts.dtd'>
                  <fontconfig>
                    <description>Korean fonts</description>

                    <match target="pattern">
                      <test name="lang" compare="contains">
                        <string>ko</string>
                      </test>
                      <test name="family">
                        <string>serif</string>
                      </test>
                      <edit name="family" mode="prepend">
                        <string>${cfg.serifFontName}</string>
                      </edit>
                    </match>

                    <match target="pattern">
                      <test name="lang" compare="contains">
                        <string>ko</string>
                      </test>
                      <test name="family">
                        <string>sans-serif</string>
                      </test>
                      <edit name="family" mode="prepend">
                        <string>${cfg.sansFontName}</string>
                      </edit>
                    </match>

                    <match target="pattern">
                      <test name="lang" compare="contains">
                        <string>ko</string>
                      </test>
                      <test name="family">
                        <string>monospace</string>
                      </test>
                      <edit name="family" mode="prepend">
                        <string>${cfg.monoFontName}</string>
                      </edit>
                    </match>

                  </fontconfig>
                '';
              };
            };
          }
      )
    ];
}
