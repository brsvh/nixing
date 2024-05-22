{ cell, pkgs, ... }:
{
  imports = [ cell.nixosProfiles.fontconfig ];

  fonts = {
    packages = with pkgs; [
      noto-fonts-cjk-sans
      noto-fonts-cjk-serif
    ];
  };

  i18n = {
    supportedLocales = [
      # EUC
      "ko_KR.EUC-KR/EUC-KR"

      # UTF-8
      "ko_KR.UTF-8/UTF-8"
    ];
  };
}
