{
  config,
  lib,
  ...
}:
let
  inherit (lib)
    filterAttrs
    mapAttrsToList
    mkIf
    optional
    ;
in
{
  imports = [
    ../../modules/fonts
  ];

  config =
    let
      cfg = config.fonts.fontconfig;

      getLangFontNames =
        type:
        let
          languages = filterAttrs (n: v: v.enable) cfg.languages;
        in
        mapAttrsToList (_n: v: v.${type}) languages;

      getFontName =
        name: type:
        let
          isEnable = cfg.${name}.enable;
        in
        optional isEnable cfg.${name}.${type};

    in
    mkIf cfg.enable {
      fonts = {
        fontconfig = {
          defaultFonts = {
            emoji = (getFontName "emoji" "defaultFont") ++ (getFontName "symbol" "defaultFont");
            monospace = (getFontName "symbol" "defaultMonoFont") ++ (getLangFontNames "monospace");
            sansSerif = (getFontName "symbol" "defaultFont") ++ (getLangFontNames "sansSerif");
            serif = (getFontName "symbol" "defaultFont") ++ (getLangFontNames "serif");
          };
        };
      };

      home = {
        packages = cfg._fonts;
      };

      xdg = {
        configFile = {
          "fontconfig/conf.d/51-local-fontconfig.conf" = {
            source = "${cfg._configPackage}/etc/fonts/conf.d/51-local-fontconfig.conf";
          };
        };

        enable = true;
      };
    };
}
