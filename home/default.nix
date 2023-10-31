{ config
, inputs
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
  configurations = {
    default = {
      home = {
        inherit (inputs) home-manager nixpkgs;
        system = "x86_64-linux";
        stateVersion = "23.05";
      };
    };

    global = {
      home = {
        modules =
          [ ];
        specialArgs =
          { inherit (inputs) home-manager; };
      };
    };

    home = {
      "bsc@eustoma" = {
        modules =
          [
            profiles.bsc.home
          ];
      };
    };
  };

  flake = {
    homeConfigurations =
      mapAttrs
        (_: cfg: cfg.finalHomeConfiguration)
        config.configurations.home;
  };
}
