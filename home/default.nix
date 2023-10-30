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
  inherit (inputs) nixpkgs;

  profiles = load {
    src = ../home;
    loader = loaders.path;
  };
in
{
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
