{
  description = ''
    nixing - a place to collect things about Nix/NixOS
  '';

  nixConfig = {
    experimental-features = [
      "flakes"
      "nix-command"
    ];
  };

  # Nix packages
  inputs = {
    nixpkgs = {
      follows = "nixpkgs-unstable";
    };
    nixpkgs-stable = {
      url = "github:NixOS/nixpkgs/nixos-23.11";
    };
    nixpkgs-unstable = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
  };

  # NixOS packages
  inputs = {
    nixos = {
      follows = "nixos-unstable";
    };
    nixos-stable = {
      url = "github:NixOS/nixpkgs/nixos-23.11";
    };
    nixos-unstable = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
  };

  # Nix libraries
  inputs = {
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
          follows = "nix-systems";
        };
      };
    };
    nix-systems = {
      url = "github:nix-systems/x86_64-linux/main";
    };
  };

  # Nix tools
  inputs = {
    blank = {
      follows = "std/blank";
    };
    haumea = {
      follows = "std/haumea";
    };
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
    nixago = {
      url = "github:nix-community/nixago/master";
      inputs = {
        flake-utils = {
          follows = "flake-utils";
        };
        nixpkgs = {
          follows = "nixpkgs";
        };
        nixago-exts = {
          follows = "nixago-extensions";
        };
      };
    };
    nixago-extensions = {
      url = "github:nix-community/nixago-extensions/master";
      inputs = {
        flake-utils = {
          follows = "nixago/flake-utils";
        };
        nixago = {
          follows = "nixago";
        };
        nixpkgs = {
          follows = "nixago/nixpkgs";
        };
      };
    };
    paisano = {
      follows = "std/paisano";
    };
    std = {
      url = "github:divnix/std/main";
      inputs = {
        devshell = {
          follows = "devshell";
        };
        lib = {
          follows = "nixpkgs";
        };
        nixago = {
          follows = "nixago";
        };
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
  };

  outputs =
    {
      flake-parts,
      nix-systems,
      nixpkgs,
      std,
      ...
    }@inputs:
    let
      lib = nixpkgs.lib // builtins;

      systems = import nix-systems;
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      inherit systems;

      imports = [ std.flakeModule ];

      perSystem =
        { pkgs, ... }:
        {
          formatter = pkgs.treefmt;
        };

      std = {
        grow = {
          cellsFrom = ./nix;

          cellBlocks = with std.blockTypes; [
            (devshells "devshells")
            (nixago "nixago")
          ];
        };

        harvest = {
          devShells = [
            [
              "repo"
              "devshells"
            ]
          ];
        };
      };
    };
}
