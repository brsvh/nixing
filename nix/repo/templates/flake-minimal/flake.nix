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
    flake-compat = {
      url = "github:edolstra/flake-compat/master";
      flake = false;
    };
    nix-systems = {
      url = "github:nix-systems/default/main";
    };
  };

  outputs =
    {
      self,
      nix-systems,
      nixpkgs,
      ...
    }@inputs:
    let
      inherit (nixpkgs.lib) genAttrs;

      lib = nixpkgs.lib // builtins;

      systems = import nix-systems;

      eachSystem = # eachSystem :: (Pkgs -> a) -> a
        f:
        genAttrs systems (
          system:
          let
            pkgs = nixpkgs.legacyPackages.${system};
          in
          f pkgs
        );
    in
    {
      devShells = eachSystem (pkgs: {
        default = pkgs.mkShell {
          packages = [ pkgs.emptyFile ];
          shellHook = ''
            # Here is what you need to do when activate the project shell.
          '';
        };
      });

      packages = eachSystem (pkgs: {
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
      });
    };
}
