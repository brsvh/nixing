{ cell, pkgs, ... }:
{
  imports = [ cell.nixosProfiles.fontconfig ];

  fonts = {
    packages = with pkgs; [ noto-fonts-cjk ];
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
