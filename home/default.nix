{ inputs
, lib
, self
, withSystem
, ...
}:
with lib;
let
  inherit (inputs.haumea.lib) load loaders;
  inherit (inputs.home-manager.lib) homeManagerConfiguration;

  profiles = load {
    src = ../home;
    loader = loaders.path;
  };
in
{
  # imports =
  #   [
  #     self.flakeModules.home-manager
  #   ];

  flake = {
    homeConfigurations = {
      "bsc@eustoma" = withSystem "x86_64-linux" (
        ctx @
        { system
        , ...
        }:
        homeManagerConfiguration {
          extraSpecialArgs =
            { inherit (inputs) home-manager; };
          modules =
            [
              profiles.bsc.home
            ];
          pkgs = nixpkgs.legacyPackages."${system}";
        }
      );
    };
  };
}
