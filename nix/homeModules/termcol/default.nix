{
  lib,
  ...
}:
let
  inherit (builtins)
    listToAttrs
    map
    toString
    ;

  inherit (lib)
    mdDoc
    mkOption
    nameValuePair
    range
    types
    ;

  mkTermcols =
    variant:
    let
      mkCol =
        n:
        mkOption {
          default = "ffffff";

          description = mdDoc ''
            The color of ${variant} termcol${n}.
          '';

          type = types.str;
        };
    in
    (listToAttrs (
      map (
        n:
        let
          colNum = toString n;
        in
        nameValuePair "termcol${colNum}" (mkCol colNum)
      ) (range 0 15)
    ))
    // {
      background = mkOption {
        default = "ffffff";

        description = mdDoc ''
          The color of ${variant} background.
        '';

        type = types.str;
      };

      foreground = mkOption {
        default = "ffffff";

        description = mdDoc ''
          The color of ${variant} foreground.
        '';

        type = types.str;
      };
    };
in
{
  options = {
    termcol = {
      dark = mkTermcols "dark";
      light = mkTermcols "light";
    };
  };
}
