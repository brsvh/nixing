{
  config,
  my,
  ...
}:
let
  inherit (config.termcol)
    dark
    ;
in
{
  imports = [
    my.homeProfiles.termcol
  ];

  programs = {
    foot = {
      enable = true;

      server = {
        enable = true;
      };

      settings = {
        colors = {
          background = dark.background;
          foreground = dark.foreground;

          regular0 = dark.termcol0;
          regular1 = dark.termcol1;
          regular2 = dark.termcol2;
          regular3 = dark.termcol3;
          regular4 = dark.termcol4;
          regular5 = dark.termcol5;
          regular6 = dark.termcol6;
          regular7 = dark.termcol7;

          bright0 = dark.termcol8;
          bright1 = dark.termcol9;
          bright2 = dark.termcol10;
          bright3 = dark.termcol11;
          bright4 = dark.termcol12;
          bright5 = dark.termcol13;
          bright6 = dark.termcol14;
          bright7 = dark.termcol15;
        };

        main =
          let
            cfg = config.fonts.fontconfig.languages.english;
            size = toString config.fonts.size;
          in
          {
            font = "${cfg.monospace}:${size}";
            term = "xterm-256color";
          };

        mouse = {
          hide-when-typing = "yes";
        };
      };
    };
  };
}
