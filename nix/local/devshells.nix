{ cell, inputs }:
let
  inherit (inputs)
    nixpkgs
    rust-overlay
    std
    ;

  pkgs = nixpkgs.appendOverlays [ rust-overlay.overlays.default ];
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
        package = pkgs.nixVersions.stable;
      }

      # Tool
      {
        category = "tool";
        package = pkgs.colmena;
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

  cc = std.lib.dev.mkShell {
    name = "cc";

    packages = with pkgs.llvmPackages; ([
      clang-tools
      clang
    ]);
  };

  rust = std.lib.dev.mkShell {
    name = "rust";

    packages = with pkgs; [ rust-bin.stable.latest.default ];
  };

  scheme = std.lib.dev.mkShell {
    name = "scheme";

    packages = with pkgs; [
      chez
      guile
    ];
  };
}
