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

    substituters = [
      "https://mirrors.cernet.edu.cn/nix-channels/store"
      "https://mirror.nju.edu.cn/nix-channels/store"
      "https://mirror.sjtu.edu.cn/nix-channels/store"
      "https://cache.nixos.org"
    ];
  };

  # Home Manager
  inputs = {
    home-manager = {
      follows = "home-manager-unstable";
    };
    home-manager-stable = {
      url = "github:nix-community/home-manager/release-23.11";
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
        std = {
          follows = "std";
        };
      };
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
  };

  outputs =
    {
      hive,
      nix-systems,
      nixpkgs,
      self,
      std,
      ...
    }@inputs:
    let
      inherit (hive) collect growOn harvest;

      collect' = collect // {
        renamer = _: target: target;
      };

      lib = nixpkgs.lib // builtins;

      systems = import nix-systems;
    in
    growOn
      {
        inherit systems;

        cellsFrom = ./nix;

        cellBlocks =
          (with std.blockTypes; [
            (devshells "devshells")
            (functions "homeProfiles")
            (functions "nixosProfiles")
            (functions "nixosSecrets")
            (functions "nixosSuites")
            (functions "nixosUsers")
            (installables "packages")
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
            "repo"
            "devshells"
          ]
        ];

        formatter = harvest self [
          [
            "repo"
            "formatter"
          ]
        ];

        packages = harvest self [
          [
            "soil"
            "packages"
          ]
        ];
      };
}
