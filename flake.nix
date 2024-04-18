{
  description = "Nixing - A place to collect things about Nix.";

  nixConfig = {
    experimental-features = [
      "flakes"
      "nix-command"
      "repl-flake"
    ];

    substituters = [
      "https://mirrors.cernet.edu.cn/nix-channels/store"
      "https://mirror.sjtu.edu.cn/nix-channels/store"
      "https://cache.nixos.org"
    ];

    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://brsvh.cachix.org"
    ];

    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "brsvh.cachix.org-1:DqtlvqnpP9g39l8Eo74AXRftGx1KJLid/ViADTNgDNE="
    ];
  };

  inputs = {
    my-emacs = {
      url = "github:brsvh/my-emacs/main";
    };
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
        flake-utils = {
          follows = "flake-utils";
        };
        nixpkgs = {
          follows = "nixpkgs";
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
    gitignore = {
      url = "github:hercules-ci/gitignore.nix/master";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
    hardware = {
      url = "github:NixOS/nixos-hardware/master";
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
    lanzaboote = {
      url = "github:nix-community/lanzaboote/v0.3.0";
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
        flake-utils = {
          follows = "flake-utils";
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
      follows = "nixpkgs-unstable";
    };
    nixpkgs-stable = {
      url = "github:NixOS/nixpkgs/nixos-23.11";
    };
    nixpkgs-unstable = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
    git-hooks = {
      url = "github:cachix/git-hooks.nix/master";
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
    tsangertype-fonts = {
      url = "github:brsvh/tsangertype-fonts.nix";
      inputs = {
        flake-parts = {
          follows = "flake-parts";
        };
        nixpkgs = {
          follows = "nixpkgs";
        };
        treefmt = {
          follows = "treefmt";
        };
      };
    };
  };

  outputs =
    {
      devshell,
      flake-parts,
      nixago,
      nixpkgs,
      git-hooks,
      treefmt,
      tsangertype-fonts,
      ...
    }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } (
      { config, ... }:
      with builtins;
      with nixpkgs.lib;
      {
        imports = [
          devshell.flakeModule
          git-hooks.flakeModule
          treefmt.flakeModule

          ./flakeModules/configurations
          ./flakeModules/homeOptions.nix
          ./flakeModules/nixago.nix

          ./nixos
          ./home
        ];

        perSystem =
          {
            config,
            inputs',
            pkgs,
            system,
            ...
          }:
          {
            devshells = {
              default = {
                name = "nixing:default";

                commands = [
                  {
                    package = pkgs.age;
                    category = "secrets";
                  }
                  {
                    package = inputs'.my-emacs.packages.nogui;
                    help = "The extensible, customizable GNU text editor";
                    category = "development";
                  }
                  {
                    package = pkgs.git;
                    category = "development";
                  }
                  {
                    package = pkgs.nixUnstable;
                    category = "development";
                  }
                  {
                    package = pkgs.sops;
                    category = "secrets";
                  }
                  {
                    package = pkgs.ssh-to-age;
                    category = "secrets";
                  }
                ];

                devshell = {
                  startup = {
                    nixago = {
                      text = (nixago.lib."${system}".makeAll (attrValues config.nixago.configs)).shellHook;
                    };

                    pre-commit = {
                      text = config.pre-commit.installationScript;
                    };
                  };
                };

                env = [
                  {
                    name = "EDITOR";
                    value = "emacs";
                  }
                ];

                packages = [ inputs'.my-emacs.packages.dependencies ];
              };
            };

            pre-commit = {
              check = {
                enable = true;
              };

              settings = {
                hooks = {
                  nixfmt = {
                    enable = true;
                    package = pkgs.nixfmt-rfc-style;
                  };
                };
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

        systems = [ "x86_64-linux" ];

        flake = {
          flakeModules = {
            configurations = ./configurations.nix;
            homeOptions = ./homeOptions.nix;
            nixago = ./nixago.nix;
          };

          homeConfigurations = mapAttrs (_: cfg: cfg.finalHomeConfiguration) config.configurations.home;

          homeModules = {
            fonts = import ./homeModules/fonts;
            home = import ./homeModules/home;
            programs = import ./homeModules/programs;
            services = import ./homeModules/services;
          };

          nixosConfigurations = mapAttrs (_: cfg: cfg.finalNixOSConfiguration) config.configurations.nixos;

          nixosModules = {
            workstation = import ./nixosModules/workstation;
          };

          overlays = {
            free = import ./overlays/free.nix;
            unfree = import ./overlays/unfree.nix;
          };
        };
      }
    );
}
