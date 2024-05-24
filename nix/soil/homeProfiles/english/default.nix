{ cell, ... }:
{
  imports = [ cell.homeProfiles.fonts ];

  fonts = {
    fontconfig = {
      english = {
        defaultFont = {
          sansSerif = "IBM Plex Sans";
          serif = "IBM Plex Serif";
          monospace = "IBM Plex Mono";
        };

        enable = true;

        fonts = with pkgs; [
          ibm-plex
          libertine
        ];
      };
    };
  };
}
