{ config, ... }:
{
  environment = {
    systemPackages = with pkgs; [ (nerdfonts.override { fonts = [ "NerdFontsSymbolsOnly" ]; }) ];
  };
  programs = {
    starship = {
      enable = true;

      presets = [ "nerd-font-symbols" ];

      settings = {
        username = {
          show_always = true;
          disabled = false;
        };
      };
    };
  };
}
