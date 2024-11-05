{
  inputs,
  lib,
  self,
  ...
}:
let
  inherit (builtins)
    removeAttrs
    ;

  inherit (inputs)
    browser
    home-manager
    nix-alien
    nixpkgs
    nixpkgs-darwin
    nixpkgs-stable
    nixpkgs-unstable
    rust-overlay
    x86_64-linux
    ;

  inherit (inputs.haumea.lib)
    load
    loaders
    matchers
    transformers
    ;

  home-manager-lib = home-manager.lib.hm;

  shelf-lib = import ./lib {
    inherit lib;
  };

  inherit (shelf-lib.collectors)
    rakeLeaves
    ;

  inherit (shelf-lib.importers)
    importSystem
    ;

  my =
    let
      my' = (
        removeAttrs (rakeLeaves ./.) [
          "all-modules"
          "channels"
          "packages"
          "templates"
        ]
      );
    in
    my'
    // {
      channels = load {
        inputs = {
          inherit
            nixpkgs-darwin
            nixpkgs-stable
            nixpkgs-unstable
            overlays
            ;

          x86_64-linux = importSystem x86_64-linux;
        };

        src = ./channels;
        transformer = transformers.liftDefault;
      };

      etc = load {
        loader = [
          (matchers.always loaders.path)
        ];

        src = ../etc;
      };

      lib = shelf-lib;

      root = ../.;
    };

  overlays = [
    nix-alien.overlays.default
    self.overlays.epkgs
    self.overlays.fonts
    (
      final: prev:
      let
        inherit (prev.stdenv)
          system
          ;
      in
      if system == "x86_64-linux" then
        {
          inherit (browser.packages.${system})
            google-chrome
            google-chrome-beta
            google-chrome-dev
            ;
        }
      else
        { }
    )
  ];
in
{
  imports = [
    my.flakeModules.devshell
    my.flakeModules.myConfigs
    my.flakeModules.options
  ];

  flake = {
    inherit
      my
      ;

    inherit (my)
      flakeModules
      ;

    lib = shelf-lib;

    overlays = {
      epkgs = final: prev: import my.overlays.epkgs final prev;
      fonts = final: prev: import my.overlays.fonts final prev;
    };

    templates = {
      flake-minimal = {
        description = "A minimal nix flake template";
        path = ./templates/flake-minimal;
      };
    };
  };

  myConfigs = {
    root = ./.;

    globalArgs = {
      inherit
        home-manager-lib
        inputs
        my
        self
        shelf-lib
        ;
    };

    home = {
      users = {
        bsc = {
          pkgs = my.channels.x86_64-linux-unstable;
        };

        changbingshan = {
          pkgs = my.channels.x86_64-linux-unstable;
        };
      };
    };
  };

  perSystem =
    {
      lib,
      pkgs,
      system,
      ...
    }:
    let
      inherit (lib)
        mapAttrs'
        nameValuePair
        ;

      devshells = mapAttrs' (
        n: s:
        nameValuePair n (
          import s {
            inherit
              lib
              pkgs
              ;
          }
        )
      ) my.devshells;
    in
    {
      _module = {
        args = {
          pkgs = import nixpkgs {
            inherit system;

            overlays = [
              rust-overlay.overlays.default
            ];
          };
        };
      };

      devshells = devshells // {
        default = devshells.shelf;
      };

      formatter = pkgs.treefmt;
    };
}
