{
  description = ''
    nixing - a place to collect things about Nix/NixOS
  '';

  nixConfig = {
    experimental-features = [
      "flakes"
      "nix-command"
    ];

    extra-substituters = [
      "https://brsvh.cachix.org"
      "https://colmena.cachix.org"
      "https://nix-community.cachix.org"
    ];

    extra-trusted-public-keys = [
      "brsvh.cachix.org-1:DqtlvqnpP9g39l8Eo74AXRftGx1KJLid/ViADTNgDNE="
      "colmena.cachix.org-1:7BzpDnjjH8ki2CT3f6GdOk7QAzPOl+1t3LvTLXqYcSg="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  # Applications
  inputs = {
    hercules-ci-agent = {
      url = "github:hercules-ci/hercules-ci-agent/master";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
        flake-parts = {
          follows = "flake-parts";
        };
      };
    };
  };

  # Home Manager
  inputs = {
    home-manager = {
      follows = "home-manager-unstable";
    };
    home-manager-stable = {
      url = "github:nix-community/home-manager/release-24.05";
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
      url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    };
  };

  # NixOS packages
  inputs = {
    nixos = {
      follows = "nixos-unstable";
    };
    nixos-stable = {
      url = "github:NixOS/nixpkgs/nixos-24.05";
    };
    nixos-unstable = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
  };

  # Nix libraries
  inputs = {
    filter = {
      url = "github:numtide/nix-filter/main";
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
    flake-parts-haskell = {
      url = "github:srid/haskell-flake/0.4.0";
    };
    flake-utils = {
      url = "github:numtide/flake-utils/main";
      inputs = {
        systems = {
          follows = "systems";
        };
      };
    };
    systems = {
      url = "github:nix-systems/x86_64-linux/main";
    };
  };

  # Nix tools
  inputs = {
    blank = {
      follows = "std/blank";
    };
    crane = {
      url = "github:ipetkov/crane/master";
    };
    devshell = {
      url = "github:numtide/devshell/main";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
    git-hooks = {
      url = "github:cachix/git-hooks.nix/master";
      inputs = {
        flake-compat = {
          follows = "flake-compat";
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
    gitignore = {
      url = "github:hercules-ci/gitignore.nix/master";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
    haumea = {
      follows = "std/haumea";
    };
    hive = {
      url = "github:divnix/hive/main";
      inputs = {
        colmena = {
          follows = "colmena";
        };
        devshell = {
          follows = "devshell";
        };
        nixago = {
          follows = "nixago";
        };
        nixpkgs = {
          follows = "nixpkgs";
        };
        paisano = {
          follows = "std/paisano";
        };
        std = {
          follows = "std";
        };
      };
    };
    nix-alien = {
      url = "github:thiagokokada/nix-alien/master";
      inputs = {
        flake-compat = {
          follows = "flake-compat";
        };
        flake-utils = {
          follows = "flake-utils";
        };
        nix-filter = {
          follows = "filter";
        };
        nix-index-database = {
          follows = "nix-index-database";
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
    nix-index-database = {
      url = "github:nix-community/nix-index-database/main";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs-unstable";
        };
      };
    };
    paisano = {
      follows = "std/paisano";
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

  # NixOS tools
  inputs = {
    colmena = {
      url = "github:zhaofengli/colmena/main";
      inputs = {
        flake-compat = {
          follows = "flake-compat";
        };
        flake-utils = {
          follows = "flake-utils";
        };
        nixpkgs = {
          follows = "nixos";
        };
        stable = {
          follows = "nixos-stable";
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
    hardware = {
      url = "github:NixOS/nixos-hardware/master";
    };
    lanzaboote = {
      url = "github:nix-community/lanzaboote/master";
      inputs = {
        crane = {
          follows = "crane";
        };
        flake-compat = {
          follows = "flake-compat";
        };
        flake-parts = {
          follows = "flake-parts";
        };
        nixpkgs = {
          follows = "nixpkgs";
        };
        pre-commit-hooks-nix = {
          follows = "git-hooks";
        };
        rust-overlay = {
          follows = "rust-overlay";
        };
      };
    };
  };

  # Overlays
  inputs = {
    emacs-overlay = {
      url = "github:brsvh/emacs-overlay/aa788863a1cb7fa28d4869097d83cf37603e3ae2";
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
    rust-overlay = {
      url = "github:oxalica/rust-overlay/master";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
  };

  outputs =
    {
      hive,
      nixpkgs,
      self,
      std,
      ...
    }@inputs:
    let
      inherit (hive)
        collect
        growOn
        harvest
        pick
        ;

      collect' = collect // {
        renamer = _: target: target;
      };

      lib = nixpkgs.lib // builtins;

      systems = import inputs.systems;
    in
    growOn
      {
        inherit systems;

        cellsFrom = ./nix;

        cellBlocks =
          (with std.blockTypes; [
            (devshells "devshells")
            (functions "homeModules")
            (functions "homeProfiles")
            (functions "homeUsers")
            (functions "nixosModules")
            (functions "nixosProfiles")
            (functions "nixosSecrets")
            (functions "nixosSuites")
            (functions "nixosUsers")
            (functions "overlays")
            (functions "packages")
            (functions "templates")
            (functions "homeSecrets")
            (nixago "nixago")
            (runnables "formatter")
          ])
          ++ (with hive.blockTypes; [
            colmenaConfigurations
            diskoConfigurations
            homeConfigurations
            nixosConfigurations
          ]);

        inputs = inputs // {
          inherit lib;
        };

        nixpkgsConfig = {
          allowUnfree = true;
        };
      }
      {
        colmenaHive = collect' self "colmenaConfigurations";
        diskoConfigurations = collect' self "diskoConfigurations";
        homeConfigurations = collect' self "homeConfigurations";
        nixosConfigurations = collect' self "nixosConfigurations";
      }
      {
        devShells = harvest self [
          [
            "local"
            "devshells"
          ]
        ];

        formatter = harvest self [
          [
            "local"
            "formatter"
          ]
        ];

        homeModules = pick self [
          [
            "fonts"
            "homeModules"
          ]
          [
            "my-emacs"
            "homeModules"
          ]
          [
            "home-manager"
            "homeModules"
          ]
        ];

        nixosModules = pick self [
          [
            "fonts"
            "nixosModules"
          ]
          [
            "my-emacs"
            "nixosModules"
          ]
          [
            "nixos"
            "nixosModules"
          ]
        ];

        overlays = pick self [
          [
            "fonts"
            "overlays"
          ]
          [
            "my-emacs"
            "overlays"
          ]
          [
            "apps"
            "overlays"
          ]
        ];

        packages = harvest self [
          [
            "fonts"
            "packages"
          ]
          [
            "my-emacs"
            "packages"
          ]
          [
            "apps"
            "packages"
          ]
        ];

        templates = pick self [
          [
            "repo"
            "templates"
          ]
        ];
      };
}
