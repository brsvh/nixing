{ cell, inputs }:
let
  inherit (inputs) nixpkgs std colmena;

  lib = nixpkgs.lib // builtins;

  pkgs = nixpkgs;
in
{
  default = std.lib.dev.mkShell {
    commands = [
      # Development
      {
        category = "development";
        package = pkgs.git;
      }
      {
        category = "development";
        package = pkgs.nixVersions.latest;
      }

      # Tool
      {
        category = "tool";
        package = colmena.packages.colmena;
      }
      {
        category = "tool";
        package = pkgs.dconf2nix;
      }
      {
        category = "tool";
        package = pkgs.fish;
      }
      {
        category = "tool";
        package = std.packages.std;
      }
    ];

    name = "nixing";

    nixago = with cell.nixago; [
      conform
      editorconfig
      lefthook
      mdbook
      sops
      treefmt
    ];
  };
}
