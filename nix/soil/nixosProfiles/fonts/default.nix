{ cell, pkgs, ... }:
{
  imports = [ cell.nixosModules.fonts ];

  fonts = {
    fontconfig = {
      emoji = {
        defaultFont = "Noto Color Emoji";

        defaultFonts = [
          "Noto Color Emoji"
          "Twitter Color Emoji"
        ];

        enable = true;

        fonts = with pkgs; [
          noto-fonts-color-emoji
          twitter-color-emoji
        ];
      };

      symbol = {
        enable = true;

        nerdFonts = {
          includePatches = [
            "IBMPlexMono"
            "NerdFontsSymbolsOnly"
          ];
        };
      };
    };
  };
}
