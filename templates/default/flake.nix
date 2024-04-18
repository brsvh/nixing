{
  description = ''
    DESCRIBE PROJECT HERE
  '';

  nixConfig = {
    experimental-features = [
      "flakes"
      "nix-command"
      "repl-flake"
    ];
  };

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
    gitignore = {
      url = "github:hercules-ci/gitignore.nix/master";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
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

  outputs =
    inputs@{
      devshell,
      flake-parts,
      git-hooks,
      nixpkgs,
      self,
      treefmt,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        flake-parts.flakeModules.easyOverlay

        devshell.flakeModule
        git-hooks.flakeModule
        treefmt.flakeModule
      ];

      systems = [ "x86_64-linux" ];

      perSystem =
        {
          config,
          final,
          pkgs,
          self',
          ...
        }:
        {
          apps = {

          };

          devshells = {
            default = {
              commands = [
                {
                  package = pkgs.git;
                  category = "development";
                }
                {
                  package = pkgs.nixUnstable;
                  category = "development";
                }
              ];

              devshell = {
                startup = {
                  pre-commit = {
                    text = config.pre-commit.installationScript;
                  };
                };
              };
            };
          };

          overlayAttrs = import ./pkgs final pkgs;

          packages = {
            default = pkgs.hello;
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
    };
}
