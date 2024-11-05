{
  flake-parts-lib,
  lib,
  inputs,
  ...
}:
let
  inherit (flake-parts-lib)
    mkPerSystemOption
    ;

  inherit (lib)
    mapAttrs
    mkOption
    types
    ;

  devshell =
    if inputs ? devshell then
      inputs.devshell
    else
      throw ''
        devshell input not found, please add a devshell input to your flake.
      '';

  nixago =
    if inputs ? nixago then
      inputs.nixago
    else
      throw ''
        nixago input not found, please add a nixago input to your flake.
      '';
in
{
  options = {
    perSystem = mkPerSystemOption (
      {
        config,
        lib,
        pkgs,
        system,
        ...
      }:
      let
        inherit (builtins)
          listToAttrs
          map
          ;

        inherit (lib)
          flatten
          nameValuePair
          ;

        devshellSubmodule =
          let
            nixagoSubmodule =
              {
                config,
                ...
              }:
              let
                inherit (builtins)
                  elemAt
                  getEnv
                  length
                  ;

                inherit (lib)
                  splitString
                  ;

                engines = import "${nixago}/engines/default.nix" {
                  inherit
                    lib
                    pkgs
                    ;
                };
              in
              {
                options = {
                  apply = mkOption {
                    default = x: x;

                    description = ''
                      Apply this transformation to `data`
                    '';

                    type = with types; functionTo anything;
                  };

                  data = mkOption {
                    description = ''
                      The raw configuration data
                    '';

                    type = types.anything;
                  };

                  engine = mkOption {
                    default = engines.nix { };

                    description = ''
                      The engine to use for generating the derivation
                    '';

                    type = with types; functionTo package;
                  };

                  format = mkOption {
                    default = (
                      let
                        parts = splitString "." config.output;
                      in
                      elemAt parts ((length parts) - 1)
                    );

                    description = ''
                      The format of the output file
                    '';

                    type = types.str;

                  };

                  hook = mkOption {
                    default = { };

                    description = ''
                      Additional options for controlling hook generation
                    '';

                    type = types.submodule (
                      {
                        config,
                        ...
                      }:
                      {
                        options = {
                          extra = mkOption {
                            default = "";

                            description = ''
                              Shell code to run when the file is updated
                            '';

                            type = with types; either str (functionTo str);
                          };

                          mode = mkOption {
                            default = "link";

                            description = ''
                              The output mode to use (copy or link)
                            '';

                            type = types.str;
                          };
                        };
                      }
                    );
                  };

                  output = mkOption {
                    type = types.str;
                    description = ''
                      The relative path to link the generated file.
                    '';
                  };

                  packages = mkOption {
                    default = [ ];

                    description = ''
                      Dependencies of this request.
                    '';

                    type = with types; listOf package;
                  };

                  root = mkOption {
                    default = if getEnv "PRJ_ROOT" == "" then ./. else /. + (getEnv "PRJ_ROOT");

                    description = ''
                      The root path from which the relative path is derived
                    '';

                    type = types.path;

                  };
                };
              };

            mkNixago = nixago.lib.${system}.make;
          in
          (import "${devshell}/modules/modules.nix" {
            inherit
              lib
              pkgs
              ;
          })
          ++ [
            (
              {
                config,
                ...
              }:
              {
                options = {
                  nixago = mkOption {
                    default = [ ];

                    description = ''
                      A list of nixago configurations.
                    '';

                    type =
                      with types;
                      listOf (submoduleWith {
                        modules = [ nixagoSubmodule ];
                      });
                  };
                };

                config = {
                  devshell = {
                    packages = flatten (map (n: n.packages) config.nixago);
                    startup = listToAttrs (
                      map (
                        n:
                        nameValuePair "${n.output}" {
                          text = (mkNixago n).shellHook;
                        }
                      ) config.nixago
                    );
                  };
                };
              }
            )
          ];
      in
      {
        options = {
          devshells = mkOption {
            default = { };

            description = ''
              Configure devshells with flake-parts.

              Not to be confused with `devShells`, with a capital S. Yes, this
              is unfortunate.

              Each devshell will also configure an equivalent `devShells`.

              Used to define devshells. not to be confused with `devShells`
            '';

            type = types.lazyAttrsOf (types.submoduleWith { modules = devshellSubmodule; });
          };
        };

        config = {
          devShells = mapAttrs (_name: devshell: devshell.devshell.shell) config.devshells;
        };
      }
    );
  };
}
