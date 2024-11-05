{
  my,
  pkgs,
  ...
}:
{
  imports = [
    my.homeModules.fonts
    my.homeProfiles.fonts
  ];

  fonts = {
    fontconfig = {
      emoji = {
        enable = true;
      };

      languages = {
        english = {
          enable = true;

          fonts = with pkgs; [
            ibm-plex
          ];

          monospace = "IBM Plex Mono";
          sansSerif = "IBM Plex Sans";
          serif = "IBM Plex Serif";
        };
      };

      symbol = {
        enable = true;
      };
    };
  };
}
