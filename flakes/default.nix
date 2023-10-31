{ ... }:
{
  imports =
    [
      ./devshell.nix
      ./homeConfigurations.nix
      ./pre-commit.nix
      ./treefmt.nix
    ];

  flake = {
    flakeModules = {
      homeConfigurations = ./homeConfigurations.nix;
    };
  };
}
