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
      flake-utils,
      nix-systems,
      nixpkgs,
      self,
      treefmt,
      ...
    }@inputs:
    let
      inherit (nixpkgs.lib) genAttrs;

      lib = nixpkgs.lib // builtins;

      systems = import nix-systems;
    in
    flake-utils.lib.eachSystem systems (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;

          overlays = [ devshell.overlays.default ];
        };

        treefmt' = treefmt.lib.evalModule pkgs {
          projectRootFile = "flake.nix";

          programs = {
            nixfmt = {
              enable = true;
              package = pkgs.nixfmt-rfc-style;
            };
          };
        };
      in
      {
        checks = {
          formatting = treefmt'.config.build.check self;
        };

        devShells = {
          default = pkgs.devshell.mkShell {
            commands = [
              # Add commands here.
            ];

            packages = [
              # Add packages here.
            ];
          };
        };

        formatter = treefmt'.config.build.wrapper;

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
      }
    );
}
