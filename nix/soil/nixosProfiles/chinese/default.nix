{ cell, pkgs, ... }:
{
  imports = [ cell.nixosProfiles.fontconfig ];

  fonts = {
    packages = with pkgs; [ noto-fonts-cjk ];
  };

  i18n = {
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
