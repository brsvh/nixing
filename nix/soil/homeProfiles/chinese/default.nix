{
  cell,
  pkgs,
  ...
}:
{
  imports = [
    cell.homeProfiles.fcitx
    cell.homeProfiles.fonts
    cell.homeProfiles.rime
  ];

  fonts = {
    fontconfig = {
      chinese = {
        sansSerif = "FZHei-B01";
        serif = "FZShusong-Z01";
        monospace = "LXGW WenKai Mono";

        enable = true;

        fonts = with pkgs; [
          alibaba-puhuiti
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
}
