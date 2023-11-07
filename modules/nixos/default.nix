{ ... }:
{
  flake = {
    nixosModules = {
      workstation = import ./workstation;
    };
  };
}
