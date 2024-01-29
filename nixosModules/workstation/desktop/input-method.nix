{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.workstation.desktop;

  withGnome3 = cfg.flavour == "gnome3";
  withPlasma5 = cfg.flavour == "plasma5";

  withFcitx5 = cfg.inputMethod == "fcitx5";
  withIbus = cfg.inputMethod == "ibus";

  withChinese = config.workstation.system.i18n.chinese.enable;
in
{
  options.workstation.desktop = {
    inputMethod = mkOption {
      type = types.enum
        [
          "ibus"
          "fcitx5"
        ];
      default = "fcitx5";
      description = ''
        The default input method.
      '';
    };
  };

  config =
    let
      im =
        if withGnome3
        then "ibus"
        else
          if withPlasma5
          then "fcitx5"
          else "ibus";
    in
    mkIf cfg.enable
      (
        mkMerge
          [
            {
              i18n = {
                inputMethod = {
                  enabled = cfg.inputMethod;
                };
              };

              workstation = {
                desktop = {
                  inputMethod = im;
                };
              };
            }
            (
              mkIf (withIbus && withChinese)
                {
                  i18n = {
                    inputMethod = {
                      ibus = {
                        engines = with pkgs.ibus-engines;
                          [
                            rime
                          ];
                      };
                    };
                  };
                }
            )
            (
              mkIf (withFcitx5 && withChinese)
                {
                  i18n = {
                    inputMethod = {
                      fcitx5 = {
                        addons = with pkgs;
                          [
                            fcitx5-rime
                          ];
                      };
                    };
                  };
                }
            )
          ]
      );
}
