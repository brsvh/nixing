{ config
, lib
, pkgs
, ...
}:
with builtins;
with lib;
{
  gtk = {
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
}
