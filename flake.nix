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
    crane = {
      url = "github:ipetkov/crane/master";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
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
    disko = {
      url = "github:nix-community/disko/master";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay/master";
      inputs = {
        flake-utils = {
          follows = "flake-utils";
        };
        nixpkgs = {
          follows = "nixpkgs-unstable";
        };
        nixpkgs-stable = {
          follows = "nixpkgs-stable";
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
    hardware = {
      url = "github:NixOS/nixos-hardware/master";
    };
    haumea = {
      url = "github:nix-community/haumea/main";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
    home-manager = {
      follows = "home-manager-stable";
    };
    home-manager-stable = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs-stable";
        };
      };
    };
    home-manager-unstable = {
      url = "github:nix-community/home-manager/master";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs-unstable";
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
    lanzaboote = {
      url = "github:nix-community/lanzaboote/v0.3.0";
      inputs = {
        crane = {
          follows = "crane";
        };
        flake-compat = {
          follows = "flake-compat";
        };
        flake-utils = {
          follows = "flake-utils";
        };
        nixpkgs = {
          follows = "nixpkgs";
        };
        pre-commit-hooks-nix = {
          follows = "pre-commit";
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
      url = "github:nix-community/nixago-extensions";
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
    rust-overlay = {
      url = "github:oxalica/rust-overlay/master";
      inputs = {
        flake-utils = {
          follows = "flake-utils";
        };
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
    sops = {
      url = "github:Mic92/sops-nix/master";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs-unstable";
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
            ./flakes
            ./modules
            ./nixos
            ./home
          ];

        systems =
          [
            "x86_64-linux"
          ];
      };
}
