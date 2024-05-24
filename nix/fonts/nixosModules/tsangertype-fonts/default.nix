{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.fonts.tsangertype-fonts;
in
{
  options.fonts.tsangertype-fonts = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = lib.mdDoc ''
        Whether use TsangerType fonts.
      '';
    };

    excludedFonts = lib.mkOption {
      type = with lib.types; listOf package;
      default = with pkgs.tsangertypeFonts; [ ];
      description = lib.mdDoc ''
        Fonts will be excluded from config.fonts.tsangertype-fonts.fonts.
      '';
    };

    fonts = lib.mkOption {
      type = with lib.types; listOf package;
      default = pkgs.tsangertypeFonts.listAllFonts;
      description = lib.mdDoc ''
        Available fonts.

        For example, filters out all fonts in a family (jinkai here).

        ```nix
        with lib;
        with pkgs.tsangertypeFonts;
        let
          pname = drv: attrByPath [ "pname" ] "unknown" drv;
          cond = drv: hasPrefix "tsangertype-jinkai" (pname drv);
        in
        (listOfFontsWithCond gratisProPersona cond)
        ```

        Or simply combine multiple specified fonts.

        ```nix
        with pkgs.tsangertypeFonts;
        [
          tsangertype-jinkai-01-w01-font
          tsangertype-jinkai-01-w02-font
          # ...
        ]
        ```
      '';
    };

    package = lib.mkOption {
      type = lib.types.package;
      description = lib.mdDoc ''
        Fonts will be  installed.
      '';
      default = pkgs.tsangertypeFonts.combine (
        with builtins; filter (font: !(elem font cfg.excludedFonts)) cfg.fonts
      );
    };
  };

  config = lib.mkIf cfg.enable {
    fonts = {
      packages = [ cfg.package ];
    };
  };
}
