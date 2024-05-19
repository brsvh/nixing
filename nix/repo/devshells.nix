{ cell, inputs }:
let
  inherit (inputs) nixpkgs std;

  lib = nixpkgs.lib // builtins;

  pkgs = nixpkgs;
in
{
  default = std.lib.dev.mkShell {
    commands = [
      # Development
      {
        package = pkgs.git;
        category = "development";
      }
      {
        category = "development";
        package = pkgs.nixVersions.latest;
      }

      # Tool
      {
        package = pkgs.fish;
        category = "tool";
      }
      {
        package = std.packages.std;
        category = "tool";
      }
    ];

    name = "nixing";

    nixago = with cell.nixago; [
      conform
      editorconfig
      lefthook
      treefmt
    ];
  };
}
