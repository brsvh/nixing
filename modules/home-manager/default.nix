{ ... }:
{
  flake = {
    homeModules = {
      workstation = import ./workstation;
    };
  };
}
