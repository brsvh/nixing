{ ... }:
{
  imports =
    [
      ./devshell.nix
      ./home-manager.nix
      ./pre-commit.nix
      ./treefmt.nix
    ];

  flake = {
    flakeModules = {
      home-manager = ./home-manager.nix;
    };
  };
}
