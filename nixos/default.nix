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
        stateVersion = "23.11";
      };
    };

    global = {
      nixos = {
        modules =
          [
            inputs.disko.nixosModules.disko
            inputs.home-manager.nixosModules.home-manager
            inputs.lanzaboote.nixosModules.lanzaboote
            inputs.sops.nixosModules.sops
            self.nixosModules.workstation
            {
              home-manager = {
                useGlobalPkgs = mkDefault true;
                useUserPackages = mkDefault true;
              };
              nixpkgs = {
                overlays =
                  [
                    inputs.emacs-overlay.overlays.default
                    inputs.rust-overlay.overlays.default
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
      "eustoma" = rec {
        domain = "brsvh.org";
        modules =
          [
            hosts.eustoma.configuration
            hosts.eustoma.disko
            {
              nixpkgs = {
                config = {
                  allowUnfree = true;
                };

                overlays =
                  [
                    self.overlays.unfree
                  ];
              };
            }
          ];
        nixpkgs = inputs.nixpkgs-unstable;
        specialArgs = {
          pkgs-stable = import inputs.nixpkgs-stable {
            inherit system;
          };
        };
        stateVersion = "23.11";
        system = "x86_64-linux";
        zone = "Asia/Shanghai";
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
