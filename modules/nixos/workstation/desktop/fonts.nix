{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.workstation.desktop;

  withEnglish = config.workstation.system.i18n.english.enable;
  withChinese = config.workstation.system.i18n.chinese.enable;
  withJapanese = config.workstation.system.i18n.japanese.enable;
  withKorean = config.workstation.system.i18n.korean.enable;
in
{
  config =
    mkIf cfg.enable
      (
        mkMerge
          [
            {
              environment = {
                systemPackages = with pkgs;
                  [
                    source-sans-pro
                    source-serif-pro
                    source-code-pro
                  ];
              };
            }
            (
              mkIf withEnglish
                {
                  environment = {
                    systemPackages = with pkgs;
                      [
                        libertinus
                      ];
                  };
                }
            )
            (
              mkIf (withChinese || withJapanese || withKorean)
                {
                  environment = {
                    systemPackages = with pkgs;
                      [
                        source-han-sans
                        source-han-serif
                        source-han-mono
                      ];
                  };
                }
            )
          ]
      );
}
