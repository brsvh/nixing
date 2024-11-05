{
  my,
  pkgs,
  ...
}:
{
  imports = [
    my.homeModules.fonts
    my.homeProfiles.fonts
    my.homeProfiles.rime
  ];

  fonts = {
    fontconfig = {
      emoji = {
        enable = true;
      };

      languages = {
        chinese = {
          enable = true;

          fonts = with pkgs; [
            alibabaFonts.puhuiti-2
            alibabaFonts.puhuiti-3
            lxgw-neoxihei
            lxgw-wenkai
            trionestypeFonts.ZhuqueFangsong
            windows-fonts
          ];

          sansSerif = "LXGW Neo XiHei";
          serif = "Zhuque Fangsong (technical preview)";
          monospace = "LXGW WenKai Mono";
        };
      };

      symbol = {
        enable = true;
      };
    };
  };
}
