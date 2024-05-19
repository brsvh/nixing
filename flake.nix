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
    nix-systems = {
      url = "github:nix-systems/x86_64-linux/main";
    };
  };

  outputs =
    inputs@{ flake-parts, nix-systems, ... }:
    let
      inherit (flake-parts.lib) mkFlake;

      systems = import nix-systems;
    in
    mkFlake { inherit inputs; } { inherit systems; };
}
