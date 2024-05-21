{ cell, inputs }:
let
  inherit (inputs) nixpkgs;

  pkgs = nixpkgs;
in
pkgs.treefmt
