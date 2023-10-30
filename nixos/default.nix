{ inputs
, lib
, withSystem
, ...
}:
with builtins;
with lib;
let
  inherit (inputs.haumea.lib) load loaders;

  hosts = load {
    src = ../nixos;
    loader = loaders.path;
  };
in
{
  flake = {
    nixosConfigurations = {
      thymus = withSystem "x86_64-linux" (
        { system
        , ...
        }:
        nixosSystem {
          inherit system;
          modules =
            [
              inputs.disko.nixosModules.disko
              hosts.thymus.configuration
            ];
          specialArgs = {
            inherit (inputs) hardware;
          };
        }
      );
    };
  };
}
