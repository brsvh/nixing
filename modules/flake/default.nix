{ ... }:
{
  imports =
    [
      ./configurations.nix
      ./home-manager.nix
    ];

  flake = {
    flakeModules = {
      configurations = ./configurations.nix;
      home-manager = ./home-manager.nix;
    };
  };
}
