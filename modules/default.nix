{ ... }:
{
  imports =
    [
      ./flake
      ./nixos
    ];

  flake = {
    homeModules = {
      home-manager = import ./home-manager;
    };
  };
}
