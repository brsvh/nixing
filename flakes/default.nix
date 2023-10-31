{ ... }:
{
  imports =
    [
      ./configurations.nix
      ./devshell.nix
      ./homeConfigurations.nix
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
