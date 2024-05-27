{ cell, inputs }:
{
  flake-minimal = {
    path = ./flake-minimal;
    description = "A minimal nix flake template";
  };

  flake-parts = {
    path = ./flake-parts;
    description = "A nix flake template with flake-parts";
  };

  flake-utils = {
    path = ./flake-utils;
    description = "A nix flake template with flake-utils";
  };
}
