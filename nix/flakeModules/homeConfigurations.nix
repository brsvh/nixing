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
    literalExpression
    mkOption
    types
    ;
in
{
  options = {
    flake = mkSubmoduleOptions {
      homeConfigurations = mkOption {
        type = types.lazyAttrsOf types.raw;

        default = { };

        description = ''
          Instantiated HomeManager configurations. Used by `home-manager`.
        '';

        example = literalExpression ''
          {
            my-user = inputs.home-manager.lib.homeManagerConfiguration {
              ...
            };
          }
        '';
      };
    };
  };
}
