{
  config,
  inputs,
  lib,
  self,
  withSystem,
  ...
}:
with builtins;
with lib;
let
  inherit (inputs)
    disko
    emacs-overlay
    home-manager-stable
    home-manager-unstable
    lanzaboote
    nixago
    nixpkgs-stable
    nixpkgs-unstable
    rust-overlay
    sops
    ;

  nixing = self;
in
{
  configurations = {
    default = {
      nixos = {
        nixpkgs = nixpkgs-stable;
        system = "x86_64-linux";
        stateVersion = "23.11";
      };
    };

    global = {
      nixos = {
        modules = [
          disko.nixosModules.disko
          lanzaboote.nixosModules.lanzaboote
          sops.nixosModules.sops
          nixing.nixosModules.workstation
          {
            home-manager = {
              useGlobalPkgs = mkDefault true;
              useUserPackages = mkDefault true;
            };
            nixpkgs = {
              overlays = [
                emacs-overlay.overlays.default
                rust-overlay.overlays.default
                nixing.overlays.free
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
        modules = [
          home-manager-unstable.nixosModules.home-manager
          ./eustoma
          {
            nixpkgs = {
              config = {
                allowUnfree = true;
              };

              overlays = [ nixing.overlays.unfree ];
            };
          }
        ];
        nixpkgs = nixpkgs-unstable;
        specialArgs = {
          pkgs-stable = import nixpkgs-stable { inherit system; };
        };
        stateVersion = "23.11";
        system = "x86_64-linux";
        zone = "Asia/Shanghai";
      };
    };
  };

  perSystem =
    { config, system, ... }:
    {
      nixago = {
        configs = mkMerge [
          {
            ".sops.yaml" = {
              output = ".sops.yaml";
              format = "yaml";
              data =
                let
                  eustoma = {
                    age = "age1lgy77wf7vxlvvv8lzsgmq6wgf43c4hl93ls2mw8pspmdcuzqvems7svu6t";
                  };
                in
                {
                  creation_rules = {
                    "nixos/eustoma/secrets.yaml" = {
                      path_regex = "^nixos/eustoma/secrets\.yaml$";
                      key_groups = [ { age = [ eustoma.age ]; } ];
                    };
                  };
                };
            };
          }
        ];
      };
    };
}
