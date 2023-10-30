{ ... }:
{
  imports =
    [
      ./devshell.nix
      ./pre-commit.nix
      ./treefmt.nix
    ];

  flake = {
    flakeModules = {
      home-manager = ./home-manager;
    };
  };
}
