{
  cell,
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [ cell.nixosProfiles.fontconfig ];

  fonts = {
    packages = with pkgs; [ noto-fonts-cjk ];
  };

  i18n = {
    inputMethod = lib.mkMerge [
      (lib.mkIf (config.i18n.inputMethod.enabled == "fcitx5") {
        fcitx5 = {
          addons = with pkgs; [ fcitx5-rime ];
        };
      })
      (lib.mkIf (config.i18n.inputMethod.enabled == "ibus") {
        ibus = {
          engines = with pkgs.ibus-engines; [ rime ];
        };
      })
    ];

    supportedLocales = [
      # BIG-5
      "zh_HK/BIG5-HKSCS"
      "zh_TW/BIG5"

      # EUC
      "zh_TW.EUC-TW/EUC-TW"

      # GB18030
      "zh_CN.GB18030/GB18030"

      # GB2312
      "zh_CN/GB2312"
      "zh_SG/GB2312"

      # GBK
      "zh_CN.GBK/GBK"
      "zh_SG.GBK/GBK"

      # UTF-8
      "zh_CN.UTF-8/UTF-8"
      "zh_HK.UTF-8/UTF-8"
      "zh_SG.UTF-8/UTF-8"
      "zh_TW.UTF-8/UTF-8"
    ];
  };
}
