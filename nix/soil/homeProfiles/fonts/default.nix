{ cell, pkgs, ... }:
{
  fonts = {
    fontconfig = {
      emoji = {
        defaultFont = "Noto Color Emoji";
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
