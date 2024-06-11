{ inputs, ... }:
let
  inherit (inputs) nix-index-database;
in
{
  imports = [ nix-index-database.nixosModules.nix-index ];
}
