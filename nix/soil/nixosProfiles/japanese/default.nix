{ cell, pkgs, ... }:
{
  imports = [ cell.nixosProfiles.fonts ];

  fonts = {
    fontconfig = {
      japanese = {
        enable = true;
      };
    };
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
