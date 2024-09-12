{ inputs, ... }:
let
  inherit (inputs) nh nix-index-database;
in
{
  imports = [
    nix-index-database.nixosModules.nix-index
  ];
}
