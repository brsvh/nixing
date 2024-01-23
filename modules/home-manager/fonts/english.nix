{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.fonts.english;
in
{
  options = {
    fonts = {
      english = {
        enable = mkOption {
          type = types.bool;
          default = true;
          description = ''
            Whether to use Chinese Fonts.
          '';
        };

        enableNerdFontIntegration = mkOption {
          type = types.bool;
          default = false;
          description = ''
            Whether to use Nerd Fonts.
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
      };
    };
  };

  config = mkMerge
    [
      (
        mkIf (cfg.enable && cfg.flavour == "Source")
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
              };
            };
          }
      )
      (
        mkIf (cfg.enable && cfg.flavour == "Plex")
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
              };
            };
          }
      )
      (
        mkIf (cfg.enable && cfg.enableNerdFontIntegration)
          {
            home = {
              packages = with pkgs;
                [
                  (
                    nerdfonts.override {
                      fonts =
                        [
                          cfg.nerdFontName
                        ];
                    }
                  )
                ];
            };
          }
      )
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

              </fontconfig>
            '';
          };
        };
      }
    ];
}
