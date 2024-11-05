{
  gtk = {
    gtk2 = {
      extraConfig = ''
        gtk-im-module = "ibus";
      '';
    };

    gtk3 = {
      extraConfig = {
        gtk-im-module = "ibus";
      };
    };

    gtk4 = {
      extraConfig = {
        gtk-im-module = "ibus";
      };
    };
  };
}
