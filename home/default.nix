{ config
, inputs
, lib
, self
, withSystem
, ...
}:
with lib;
let
  inherit (inputs)
    my-emacs
    emacs-overlay
    home-manager-stable
    home-manager-unstable
    nixago
    nixpkgs-stable
    nixpkgs-unstable
    rust-overlay
    sops
    tsangertype-fonts;

  nixing = self;
in
{
  configurations = {
    default = {
      home = {
        home-manager = home-manager-stable;
        nixpkgs = nixpkgs-stable;
        system = "x86_64-linux";
        stateVersion = "23.11";
      };
    };

    global = {
      home = {
        modules =
          [
            sops.homeManagerModules.sops
            nixing.homeModules.fonts
            nixing.homeModules.home
            nixing.homeModules.programs
            nixing.homeModules.services
            {
              nixpkgs = {
                overlays =
                  [
                    emacs-overlay.overlays.default
                    rust-overlay.overlays.default
                    nixing.overlays.free
                  ];
              };
            }
          ];
      };
    };

    home = {
      "bsc@eustoma" = {
        home-manager = home-manager-unstable;
        modules =
          [
            ./bsc
            my-emacs.homeModules.my-emacs
            {
              nixpkgs = {
                config = {
                  allowUnfree = true;
                };

                overlays =
                  [
                    my-emacs.overlays.default
                    nixing.overlays.unfree
                    tsangertype-fonts.overlays.default
                  ];
              };
            }
          ];
        nixpkgs = nixpkgs-unstable;
        stateVersion = "23.11";
      };
    };
  };

  perSystem =
    { config
    , system
    , ...
    }:
    {
      nixago = {
        configs = mkMerge
          [
            {
              ".sops.yaml" = {
                output = ".sops.yaml";
                format = "yaml";
                data =
                  let
                    bsc = {
                      age = "age1h8jgr473q6vj9e8kannr0ljzreu7whc46qhjfpjxxkl4w38ny5esz6mk0v";
                      pgp = "7B740DB9F2AC6D3B226BC53078D74502D92E0218";
                    };
                  in
                  {
                    creation_rules = {
                      "secrets/bsc.yaml" = {
                        path_regex = "^secrets/bsc\.yaml$";
                        key_groups = [
                          {
                            pgp = [ bsc.pgp ];
                            age = [ bsc.age ];
                          }
                        ];
                      };
                    };
                  };
              };
            }
          ];
      };
    };
}
