{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.fonts.emoji;
in
{
  options.fonts.emoji = {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to use Emoji Fonts.
      '';
    };

    flavour = mkOption {
      type = types.enum
        [
          "Noto"
          "Twitter"
        ];
      default = "Noto";
      description = ''
        The flavour of emoji fonts.
      '';
    };

    fontName = mkOption {
      type = types.str;
      default = "Noto Color Emoji";
      description = ''
        The font name of default monospace fonts.
      '';
    };
  };

  config = mkMerge
    [
      (
        mkIf (cfg.enable && cfg.flavour == "Noto")
          {
            fonts = {
              emoji = {
                fontName = mkDefault "Noto Color Emoji";
              };
            };

            home = {
              packages = with pkgs;
                [
                  noto-fonts-color-emoji
                ];
            };
          }
      )
      (
        mkIf (cfg.enable && cfg.flavour == "Twitter")
          {
            fonts = {
              emoji = {
                fontName = mkDefault "Twitter Color Emoji";
              };
            };

            home = {
              packages = with pkgs;
                [
                  twitter-color-emoji
                ];
            };
          }
      )
      (
        mkIf cfg.enable
          {
            xdg = {
              configFile = {
                "fontconfig/conf.d/50-emoji-fonts.conf".text = ''
                  <?xml version='1.0'?>
                  <!DOCTYPE fontconfig SYSTEM 'urn:fontconfig:fonts.dtd'>
                  <fontconfig>
                    <description>${cfg.flavour} emoji fonts</description>

                    <match>
                      <test qual="any" name="family">
                        <string>emoji</string>
                      </test>
                      <edit name="family" mode="prepend" binding="strong">
                        <string>${cfg.fontName}</string>
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
