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
      diskoConfigurations = mkOption {
        type = types.lazyAttrsOf types.raw;

        default = { };

        description = ''
          Disko configurations. Used by `disko`.
        '';
      };
    };
  };
}
