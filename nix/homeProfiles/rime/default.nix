{
  config,
  inputs,
  lib,
  osConfig,
  pkgs,
  ...
}:
let
  inherit (builtins)
    typeOf
    ;

  inherit (lib)
    findFirst
    mkIf
    mkMerge
    ;

  inherit (inputs)
    oh-my-rime
    ;

  withFcitx5 =
    let
      hmFcitx = config.i18n.inputMethod.enabled == "fcitx5";

      hmAddon =
        typeOf (findFirst (p: p == pkgs.fcitx5-rime) null config.i18n.inputMethod.fcitx5.addons) != "null";

      osFcitx = osConfig.i18n.inputMethod.enable && osConfig.i18n.inputMethod.type == "fcitx5";

      osAddon =
        typeOf (findFirst (p: p == pkgs.fcitx5-rime) null osConfig.i18n.inputMethod.fcitx5.addons) != null;
    in
    (hmFcitx && hmAddon) || (osFcitx && osAddon);

  withIbus =
    let
      hmIbus = false;

      hmEngine = false;

      osIbus = osConfig.i18n.inputMethod.enable && osConfig.i18n.inputMethod.type == "ibus";

      osEngine =
        typeOf (findFirst (p: p == pkgs.ibus-engines.rime) null osConfig.i18n.inputMethod.ibus.engines)
        != "null";
    in
    (hmIbus && hmEngine) || (osIbus && osEngine);
in
{
  config = mkMerge [
    (mkIf withFcitx5 {
      xdg = {
        dataFile = {
          "fcitx5/rime" = {
            source = oh-my-rime.outPath;
            recursive = true;
          };
        };
      };
    })
    (mkIf withIbus {
      xdg = {
        configFile = {
          "ibus/rime" = {
            source = oh-my-rime.outPath;
            recursive = true;
          };
        };
      };
    })
  ];
}
