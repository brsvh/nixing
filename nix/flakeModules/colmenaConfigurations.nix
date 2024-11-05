{
  flake-parts-lib,
  lib,
  ...
}:
let
  inherit (flake-parts-lib)
    mkSubmoduleOptions
    ;

  inherit (lib)
    mkOption
    types
    ;
in
{
  options = {
    flake = mkSubmoduleOptions {
      colmenaConfigurations = mkOption {
        type = types.lazyAttrsOf types.raw;

        default = { };

        description = ''
          Colmena configurations. Used by `colmena`.
        '';
      };
    };
  };
}
