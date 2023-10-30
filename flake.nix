{
  description = "Nixing - A place to collect things about Nix.";

  nixConfig = {
    experimental-features =
      [
        "ca-derivations"
        "flakes"
        "nix-command"
        "repl-flake"
      ];
  };

  inputs = {
    devshell = {
      url = "github:numtide/devshell/main";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
        systems = {
          follows = "systems";
        };
      };
    };
    flake-compat = {
      url = "github:edolstra/flake-compat/master";
      flake = false;
    };
    flake-parts = {
      url = "github:hercules-ci/flake-parts/main";
      inputs = {
        nixpkgs-lib = {
          follows = "nixpkgs";
        };
      };
    };
    flake-utils = {
      url = "github:numtide/flake-utils/main";
      inputs = {
        systems = {
          follows = "systems";
        };
      };
    };
    gitignore = {
      url = "github:hercules-ci/gitignore.nix/master";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
    nixpkgs = {
      follows = "nixpkgs-stable";
    };
    nixpkgs-stable = {
      url = "github:NixOS/nixpkgs/nixos-23.05";
    };
    nixpkgs-unstable = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
    pre-commit = {
      url = "github:cachix/pre-commit-hooks.nix/master";
      inputs = {
        flake-compat = {
          follows = "flake-compat";
        };
        flake-utils = {
          follows = "flake-utils";
        };
        gitignore = {
          follows = "gitignore";
        };
        nixpkgs = {
          follows = "nixpkgs";
        };
        nixpkgs-stable = {
          follows = "nixpkgs-stable";
        };
      };
    };
    systems = {
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

  outputs = inputs @ { flake-parts, ... }:
    flake-parts.lib.mkFlake
      {
        inherit inputs;
      }
      {
        imports =
          [
            ./parts
          ];

        systems =
          [
            "x86_64-linux"
          ];
      };
}
