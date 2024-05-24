{
  cell,
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [ cell.nixosProfiles.fonts ];

  fonts = {
    fontconfig = {
      chinese = {
        defaultFont = {
          sansSerif = "TsangerXuanSanM W03";
          serif = "FZShusong-Z01";
          monospace = "LXGW WenKai Mono";
        };

        defaultFonts = {
          sansSerif = [
            "TsangerXuanSanM W03"
            "Source Han Sans SC"
            "Source Han Sans HC"
            "Source Han Sans TC"
          ];

          serif = [
            "FZShusong-Z01"
            "Source Han Serif SC"
            "Source Han Serif HC"
            "Source Han Serif TC"
          ];

          monospace = [
            "LXGW WenKai Mono"
            "Source Han Mono SC"
            "Source Han Mono HC"
            "Source Han Mono TC"
          ];
        };

        enable = true;

        fonts = with pkgs; [
          foundertype-fonts
          lxgw-wenkai
          source-han-sans
          source-han-serif
          source-han-mono
        ];
      };
    };

    tsangertype-fonts = {
      enable = true;
    };
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
