{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.fonts.chinese;

  enableFonts = config.fonts.enable;

  enableChinese = enableFonts && cfg.enable;
in
{
  options.fonts.chinese = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to use Chinese Fonts.
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

    variant = mkOption {
      type = types.enum
        [
          "SC"
          "TC"
        ];
      default = "SC";
      description = ''
        The flavour of variant Chinese characters.

        The meaning of SC is Simplified Chinese.
        The meaning of TC is Traditional Chinese.
      '';
    };

    sansFontName = mkOption {
      type = types.str;
      default = "Source Han Sans SC";
      description = ''
        The font name of default Sans fonts.
      '';
    };

    serifFontName = mkOption {
      type = types.str;
      default = "Source Han Serif SC";
      description = ''
        The font name of default Serif fonts.
      '';
    };

    monoFontName = mkOption {
      type = types.str;
      default = "Source Han Mono SC";
      description = ''
        The font name of default monospace fonts.
      '';
    };
  };

  config = mkMerge
    [
      (
        mkIf (enableChinese && cfg.flavour == "Noto")
          {
            fonts = {
              chinese = {
                sansFontName = mkDefault "Noto Sans CJK ${cfg.variant}";
                serifFontName = mkDefault "Noto Seif CJK ${cfg.variant}";
                monoFontName = mkDefault "Noto Sans Mono CJK ${cfg.variant}";
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
        mkIf (enableChinese && cfg.flavour == "Source")
          {
            fonts = {
              chinese = {
                sansFontName = mkDefault "Source Han Sans ${cfg.variant}";
                serifFontName = mkDefault "Source Han Seif ${cfg.variant}";
                monoFontName = mkDefault "Source Han Mono ${cfg.variant}";
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
        mkIf enableChinese
          {
            xdg = {
              configFile = {
                "fontconfig/conf.d/50-chinese-fonts.conf".text = ''
                  <?xml version='1.0'?>
                  <!DOCTYPE fontconfig SYSTEM 'urn:fontconfig:fonts.dtd'>
                  <fontconfig>
                    <description>${cfg.flavour} fonts</description>

                    <match target="pattern">
                      <test name="lang" compare="contains">
                        <string>zh</string>
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
                        <string>zh</string>
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
                        <string>zh</string>
                      </test>
                      <test name="family">
                        <string>monospace</string>
                      </test>
                      <edit name="family" mode="prepend">
                        <string>${cfg.monoFontName}</string>
                      </edit>
                    </match>

                    <match target="pattern">
                      <test qual="any" name="family">
                        <string>WenQuanYi Zen Hei</string>
                      </test>
                      <edit name="family" mode="assign" binding="same">
                        <string>${cfg.sansFontName}</string>
                      </edit>
                    </match>

                    <match target="pattern">
                      <test qual="any" name="family">
                        <string>WenQuanYi Micro Hei</string>
                      </test>
                      <edit name="family" mode="assign" binding="same">
                        <string>${cfg.sansFontName}</string>
                      </edit>
                    </match>

                    <match target="pattern">
                      <test qual="any" name="family">
                        <string>WenQuanYi Micro Hei Light</string>
                      </test>
                      <edit name="family" mode="assign" binding="same">
                        <string>${cfg.sansFontName}</string>
                      </edit>
                    </match>

                    <match target="pattern">
                      <test qual="any" name="family">
                        <string>Microsoft YaHei</string>
                      </test>
                      <edit name="family" mode="assign" binding="same">
                        <string>${cfg.sansFontName}</string>
                      </edit>
                    </match>

                    <match target="pattern">
                      <test qual="any" name="family">
                        <string>SimHei</string>
                      </test>
                      <edit name="family" mode="assign" binding="same">
                        <string>${cfg.sansFontName}</string>
                      </edit>
                    </match>

                    <match target="pattern">
                      <test qual="any" name="family">
                        <string>SimSun</string>
                      </test>
                      <edit name="family" mode="assign" binding="same">
                        <string>${cfg.serifFontName}</string>
                      </edit>
                    </match>

                    <match target="pattern">
                      <test qual="any" name="family">
                        <string>SimSun-18030</string>
                      </test>
                      <edit name="family" mode="assign" binding="same">
                        <string>${cfg.serifFontName}</string>
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
