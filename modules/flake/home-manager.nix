{ config
, flake-parts-lib
, lib
, self
, ...
}:
with lib;
let
  inherit (flake-parts-lib) mkSubmoduleOptions;
in
{
  options = {
    flake = mkSubmoduleOptions {
      homeManagerConfigurations = mkOption {
        type = types.lazyAttrsOf types.raw;
        default = { };
        description = ''
          Instantiated home-manager configurations.

           Used by `home-manager`.

          `homeManagerConfigurations` is for specific users.
        '';
        example = literalExpression ''
          {
            my-home = inputs.home-manager.homeManagerConfiguration {
              inherit pkgs;

              # Specify your home configuration modules here,
              # for example, the path to your home.nix.
              modules = [ ./home.nix ];

              # Optionally use extraSpecialArgs
              # to pass through arguments to home.nix
            };
          }
        '';
      };

      homeManagerModules = mkOption {
        type = types.lazyAttrsOf types.unspecified;
        default = { };
        apply =
          mapAttrs
            (k: v:
              {
                _file = "${toString self.outPath}/flake.nix#homeManagerModules.${k}";
                imports = [ v ];
              }
            );
        description = ''
          Home Manager modules.

          You may use this for reusable pieces of configuration, service modules, etc.
        '';
      };
    };
  };
}
