{ cell, ... }:
{
  imports = [
    cell.homeProfiles.fonts
    cell.homeProfiles.rime
  ];

  fonts = {
    fontconfig = {
      chinese = {
        defaultFont = {
          sansSerif = "Source Han Sans SC";
          serif = "Source Han Serif SC";
          monospace = "Source Han Mono SC";
        };

        defaultFonts = {
          sansSerif = [
            "Source Han Sans SC"
            "Source Han Sans HC"
            "Source Han Sans TC"
          ];

          serif = [
            "Source Han Serif SC"
            "Source Han Serif HC"
            "Source Han Serif TC"
          ];

          monospace = [
            "Source Han Mono SC"
            "Source Han Mono HC"
            "Source Han Mono TC"
          ];
        };

        enable = true;

        fonts = with pkgs; [
          source-han-sans
          source-han-serif
          source-han-mono
        ];
      };
    };
  };
}
