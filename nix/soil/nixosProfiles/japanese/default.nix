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
      "ja_JP.EUC-JP/EUC-JP"

      # UTF-8
      "ja_JP.UTF-8/UTF-8"
    ];
  };
}
