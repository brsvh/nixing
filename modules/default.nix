{ ... }:
{
  imports =
    [
      ./flake
      ./nixos
    ];

  flake = {
    homeManagerModules = {
      home-manager = import ./home-manager;
    };
  };
}
