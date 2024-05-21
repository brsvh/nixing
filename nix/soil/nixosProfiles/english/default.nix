{ cell, pkgs, ... }:
{
  imports = [ cell.nixosProfiles.fontconfig ];

  fonts = {
    packages = with pkgs; [ noto-fonts ];
  };

  i18n = {
    supportedLocales = [
      # ISO-8859-1
      "en_AU/ISO-8859-1"
      "en_BW/ISO-8859-1"
      "en_CA/ISO-8859-1"
      "en_DK/ISO-8859-1"
      "en_GB/ISO-8859-1"
      "en_HK/ISO-8859-1"
      "en_IE/ISO-8859-1"
      "en_NZ/ISO-8859-1"
      "en_PH/ISO-8859-1"
      "en_SG/ISO-8859-1"
      "en_US/ISO-8859-1"
      "en_ZA/ISO-8859-1"
      "en_ZW/ISO-8859-1"

      # ISO-8859-15
      "en_IE@euro/ISO-8859-15"

      # UTF-8
      "en_AU.UTF-8/UTF-8"
      "en_BW.UTF-8/UTF-8"
      "en_CA.UTF-8/UTF-8"
      "en_DK.UTF-8/UTF-8"
      "en_GB.UTF-8/UTF-8"
      "en_HK.UTF-8/UTF-8"
      "en_IE.UTF-8/UTF-8"
      "en_IL/UTF-8"
      "en_IN/UTF-8"
      "en_NG/UTF-8"
      "en_NZ.UTF-8/UTF-8"
      "en_PH.UTF-8/UTF-8"
      "en_SC.UTF-8/UTF-8"
      "en_SG.UTF-8/UTF-8"
      "en_US.UTF-8/UTF-8"
      "en_ZA.UTF-8/UTF-8"
      "en_ZM/UTF-8"
      "en_ZW.UTF-8/UTF-8"
    ];
  };
}
