{
  config,
  flake-parts-lib,
  inputs,
  lib,
  ...
}:
with builtins;
with lib;
let
  inherit (flake-parts-lib) mkPerSystemOption;
in
{
  options = {
    perSystem = mkPerSystemOption (
      { config, system, ... }:
      let
        inherit (inputs.nixago) engines;

        cfg = config.nixago;

        nixagoOpts =
          { name, ... }@opts:
          let
            cfg' = config;
          in
          {
            options = {
              data = mkOption {
                type = types.anything;
                description = ''
                  Data of the configuration file.
                '';
                apply = data: data // { creation_rules = attrValues data.creation_rules; };
              };
              output = mkOption {
                type = types.str;
                description = ''
                  Name of output file.
                '';
              };
              format = mkOption {
                type = types.str;
                description = ''
                  Format of the configuration file.
                '';
              };
              engine = mkOption {
                type = types.unspecified;
                default = engines."${system}".nix { };
                description = ''
                  Engine used to generate configuration file.
                '';
              };
            };
          };
      in
      {
        options = {
          nixago = {
            configs = mkOption {
              type = types.attrsOf (types.submodule nixagoOpts);
              default = { };
              description = ''
                List of nixago configurations.
              '';
            };
          };
        };
      }
    );
  };
}
