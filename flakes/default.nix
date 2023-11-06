{ ... }:
{
  imports =
    [
      ./configurations.nix
      ./devshell.nix
      ./homeConfigurations.nix
      ./nixago.nix
      ./pre-commit.nix
      ./treefmt.nix
    ];

  flake = {
    flakeModules = {
      configurations = ./configurations.nix;
      homeConfigurations = ./homeConfigurations.nix;
    };
  };
}
