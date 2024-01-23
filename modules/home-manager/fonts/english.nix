{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.fonts.english;

  enableFonts = config.fonts.enable;

  enableEnglish = enableFonts && cfg.enable;
in
{
  options = {
    fonts = {
      english = {
        enable = mkOption {
          type = types.bool;
          default = true;
          description = ''
            Whether to use English Fonts.
          '';
        };

        enableNerdFontIntegration = mkOption {
          type = types.bool;
          default = false;
          description = ''
            Whether to use Nerd Fonts.
          '';
        };

        flavour = mkOption {
          type = types.enum
            [
              "Plex"
              "Source"
            ];
          default = "Source";
          description = ''
            The flavour of sans, serif and monospace fonts.
          '';
        };

        monoFontName = mkOption {
          type = types.str;
          default = "Source Code Pro";
          description = ''
            The font name of default monospace fonts.
          '';
        };

        nerdFontName = mkOption {
          type = types.str;
          default = "";
          description = ''
            Which Nerd Font will be used.

            See https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/data/fonts/nerdfonts/shas.nix.
          '';
        };

        sansFontName = mkOption {
          type = types.str;
          default = "Source Sans Pro";
          description = ''
            The font name of default sans-serif fonts.
          '';
        };

        serifFontName = mkOption {
          type = types.str;
          default = "Source Serif Pro";
          description = ''
            The font name of default serif fonts.
          '';
        };

        systemUIFontName = mkOption {
          type = types.str;
          default = "Source Sans Pro";
          description = ''
            The font name of default system-ui fonts.
          '';
        };

        uiSansFontName = mkOption {
          type = types.str;
          default = "Source Sans Pro";
          description = ''
            The font name of default ui-sans-serif fonts.
          '';
        };
      };
    };
  };

  config = mkMerge
    [
      (
        mkIf (enableEnglish && cfg.flavour == "Source")
          {
            home = {
              packages = with pkgs;
                [
                  source-sans-pro
                  source-serif-pro
                  source-code-pro
                ];
            };

            fonts = {
              english = {
                monoFontName = "Source Code Pro";
                nerdFontName = "SourceCodePro";
                sansFontName = "Source Sans Pro";
                serifFontName = "Source Serif Pro";
                systemUIFontName = "Source Sans Pro";
                uiSansFontName = "Source Sans Pro";
              };
            };
          }
      )
      (
        mkIf (enableEnglish && cfg.flavour == "Plex")
          {
            home = {
              packages = with pkgs;
                [
                  ibm-plex
                ];
            };

            fonts = {
              english = {
                monoFontName = "IBM Plex Mono";
                nerdFontName = "IBMPlexMono";
                sansFontName = "IBM Plex Sans";
                serifFontName = "IBM Plex Serif";
                systemUIFontName = "IBM Plex Sans";
                uiSansFontName = "IBM Plex Sans";
              };
            };
          }
      )
      (
        mkIf (enableEnglish && cfg.enableNerdFontIntegration)
          {
            home = {
              packages = with pkgs;
                [
                  (
                    nerdfonts.override {
                      fonts =
                        [
                          cfg.nerdFontName
                          "NerdFontsSymbolsOnly"
                        ];
                    }
                  )
                ];
            };
          }
      )
      (
        mkIf enableEnglish
          {
            xdg = {
              configFile = {
                "fontconfig/conf.d/50-english-fonts.conf".text = ''
                  <?xml version='1.0'?>
                  <!DOCTYPE fontconfig SYSTEM 'urn:fontconfig:fonts.dtd'>
                  <fontconfig>
                    <description>${cfg.flavour} fonts</description>

                    <match>
                      <test qual="any" name="family">
                        <string>serif</string>
                      </test>
                      <edit name="family" mode="prepend" binding="strong">
                        <string>${cfg.serifFontName}</string>
                      </edit>
                    </match>

                    <match target="pattern">
                      <test qual="any" name="family">
                        <string>sans-serif</string>
                      </test>
                      <edit name="family" mode="prepend" binding="strong">
                        <string>${cfg.sansFontName}</string>
                      </edit>
                    </match>

                    <match target="pattern">
                      <test qual="any" name="family">
                        <string>monospace</string>
                      </test>
                      <edit name="family" mode="prepend" binding="strong">
                        <string>${cfg.monoFontName}</string>
                      </edit>
                    </match>

                    <match target="pattern">
                      <test qual="any" name="family">
                        <string>system-ui</string>
                      </test>
                      <edit name="family" mode="prepend" binding="strong">
                        <string>${cfg.systemUIFontName}</string>
                      </edit>
                    </match>

                    <match target="pattern">
                      <test qual="any" name="family">
                        <string>ui-sans-serif</string>
                      </test>
                      <edit name="family" mode="prepend" binding="strong">
                        <string>${cfg.uiSansFontName}</string>
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
