{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.fonts.japanese;

  enableFonts = config.fonts.enable;

  enableJapanese = enableFonts && cfg.enable;
in
{
  options.fonts.japanese = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to use Japanese Fonts.
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
      default = "Source Han Sans JP";
      description = ''
        The font name of default Sans fonts.
      '';
    };

    serifFontName = mkOption {
      type = types.str;
      default = "Source Han Serif JP";
      description = ''
        The font name of default Serif fonts.
      '';
    };

    monoFontName = mkOption {
      type = types.str;
      default = "Source Han Mono JP";
      description = ''
        The font name of default monospace fonts.
      '';
    };
  };

  config = mkMerge
    [
      (
        mkIf (enableJapanese && cfg.flavour == "Noto")
          {
            fonts = {
              japanese = {
                sansFontName = mkDefault "Noto Sans CJK JP";
                serifFontName = mkDefault "Noto Seif CJK JP";
                monoFontName = mkDefault "Noto Sans Mono CJK JP";
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
        mkIf (enableJapanese && cfg.flavour == "Source")
          {
            fonts = {
              japanese = {
                sansFontName = mkDefault "Source Han Sans JP";
                serifFontName = mkDefault "Source Han Seif JP";
                monoFontName = mkDefault "Source Han Mono JP";
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
        mkIf enableJapanese
          {
            xdg = {
              configFile = {
                "fontconfig/conf.d/50-japanese-fonts.conf".text = ''
                  <?xml version='1.0'?>
                  <!DOCTYPE fontconfig SYSTEM 'urn:fontconfig:fonts.dtd'>
                  <fontconfig>
                    <description>Japanese fonts</description>

                    <match target="pattern">
                      <test name="lang" compare="contains">
                        <string>ja</string>
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
                        <string>ja</string>
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
                        <string>ja</string>
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
