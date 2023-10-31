{ config
, inputs
, lib
, self
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
  configurations = {
    default = {
      nixos = {
        inherit (inputs) nixpkgs;
        system = "x86_64-linux";
        stateVersion = "23.05";
      };
    };

    global = {
      nixos = {
        modules =
          [
            inputs.disko.nixosModules.disko
            inputs.home-manager.nixosModules.home-manager
            inputs.lanzaboote.nixosModules.lanzaboote
            {
              nixpkgs = {
                overlays =
                  [
                    inputs.emacs-overlay.overlays.default
                  ];
              };
            }
          ];
        specialArgs = {
          inherit (inputs) hardware;
        };
      };
    };

    nixos = {
      "eustoma" = {
        domain = "brsvh.org";
        modules =
          [
            hosts.eustoma.configuration
            hosts.eustoma.disko
          ];
      };
    };
  };

  flake = {
    nixosConfigurations =
      mapAttrs
        (_: cfg: cfg.finalNixOSConfiguration)
        config.configurations.nixos;
  };
}
