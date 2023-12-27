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
        stateVersion = "23.11";
      };
    };

    global = {
      home = {
        modules =
          [
            inputs.sops.homeManagerModules.sops
            self.homeManagerModules.home-manager
            {
              nixpkgs = {
                overlays =
                  [
                    inputs.emacs-overlay.overlays.default
                    inputs.rust-overlay.overlays.default
                  ];
              };
            }
          ];
        specialArgs =
          { inherit (inputs) home-manager; };
      };
    };

    home = {
      "bsc@eustoma" = {
        home-manager = inputs.home-manager-unstable;
        modules =
          [
            profiles.bsc.home
          ];
        nixpkgs = inputs.nixpkgs-unstable;
        specialArgs =
          { home-manager = inputs.home-manager-unstable; };
        stateVersion = "23.11";
      };
    };
  };

  flake = {
    homeManagerConfigurations =
      mapAttrs
        (_: cfg: cfg.finalHomeManagerConfiguration)
        config.configurations.home;
  };
}
