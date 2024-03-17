{ config
, lib
, pkgs
, ...
}:
with builtins;
with lib;
let
  withGnome3 = false;
  withPlasma5 = false;
  withPlasma6 = false;
in
{
  gtk = mkIf withGnome3
    {
      enable = true;

      cursorTheme = {
        name = "Vanilla-DMZ";
        package = pkgs.vanilla-dmz;
        size = 16;
      };

      gtk2 = {
        configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
      };
    };

  qt = mkIf (withPlasma6 || withPlasma5) {
    enable = true;
    platformTheme = "kde";
    style = {
      package = with pkgs; kdePackages.breeze;
      name = "breeze";
    };
  };
}
