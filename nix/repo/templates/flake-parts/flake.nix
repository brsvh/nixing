{
  description = ''
    TODO DESCRIBE PROJECT HERE
  '';

  nixConfig = {
    experimental-features = [
      "flakes"
      "nix-command"
    ];

    extra-substituters = [
      # Add extra substituters here.
    ];

    extra-trusted-public-keys = [
      # Add the public key of extra substituters here.
    ];
  };

  # Nix packages
  inputs = {
    nixpkgs = {
      follows = "nixpkgs-unstable";
    };
    nixpkgs-stable = {
      url = "github:NixOS/nixpkgs/nixos-24.05";
    };
    nixpkgs-unstable = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
  };

  # Nix libraries
  inputs = {
    devshell = {
      url = "github:numtide/devshell/main";
      inputs = {
        flake-utils = {
          follows = "flake-utils";
        };
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
    flake-compat = {
      url = "github:edolstra/flake-compat/master";
      flake = false;
    };
    flake-utils = {
      url = "github:numtide/flake-utils/main";
      inputs = {
        systems = {
          follows = "nix-systems";
        };
      };
    };
    flake-parts = {
      url = "github:hercules-ci/flake-parts/main";
      inputs = {
        nixpkgs-lib = {
          follows = "nixpkgs";
        };
      };
    };
    nix-systems = {
      url = "github:nix-systems/default/main";
    };
    treefmt = {
      url = "github:numtide/treefmt-nix/main";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
  };

  outputs =
    {
      devshell,
      flake-parts,
      nix-systems,
      nixpkgs,
      self,
      treefmt,
      ...
    }@inputs:
    let
      systems = import nix-systems;
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      inherit systems;

      imports = [
        devshell.flakeModule
        treefmt.flakeModule
      ];

      perSystem =
        { config, pkgs, ... }:
        {
          devshells = {
            default = {
              commands = [
                # Add commands here.
              ];

              packages = [
                # Add packages here.
              ];
            };
          };

          packages = {
            default = pkgs.stdenv.mkDerivation {
              pname = "PROJECT";
              src = ../.;
              version = self.shortRev or "0000000";

              nativeBuildInputs = [
                # Add build dependencies here.
              ];

              buildPhase = ''
                # Add build script here.
              '';
            };
          };

          treefmt = {
            flakeFormatter = true;
            projectRootFile = "flake.nix";

            programs = {
              nixfmt = {
                enable = true;
                package = pkgs.nixfmt-rfc-style;
              };
            };
          };
        };
    };
}
